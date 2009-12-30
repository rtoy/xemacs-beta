;;; arabic.el --- pre-loaded support for Arabic. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2002 Ben Wing.

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

;;; Commentary:

;; Synched up with: Mule 2.3, FSF 21.1.

;;; Code:

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/arabic.el
(make-coding-system 'iso-8859-6 'iso2022
		    "ISO-8859-6 (Arabic)"
		    '(charset-g0 ascii
				 charset-g1 arabic-iso8859-6
				 charset-g2 t
				 charset-g3 t
				 no-iso6429 t
				 mnemonic "MIME/Arbc"
				 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARABIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-language-environment 'arabic
;;   "Arabic"
;;   (lambda ()
;;     (require 'arabic)))
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/arabic.el~base.mUtsOq
; (make-charset 'arabic-iso8859-6 
; 	      "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"
; 	      '(dimension
; 		1
; 		registry "ISO8859-6"
; 		chars 96
; 		columns 1
; 		direction r2l
; 		final ?G
; 		graphic 1
; 		short-name "RHP of ISO8859/6"
; 		long-name "RHP of Arabic (ISO 8859-6): ISO-IR-127"
; 		))

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(make-charset 'arabic-digit "Arabic digit"
	      '(dimension
		1
		registry "MuleArabic-0"
		chars 94
		columns 1
		direction l2r
		final ?2
		graphic 0
		short-name "Arabic digit"
		long-name "Arabic digit"
		))

(make-charset 'arabic-1-column "Arabic 1-column"
	      '(dimension
		1
		registry "MuleArabic-1"
		chars 94
		columns 1
		direction r2l
		final ?3
		graphic 0
		short-name "Arabic 1-col"
		long-name "Arabic 1-column"
		))

(make-charset 'arabic-2-column "Arabic 2-column"
	      '(dimension
		1
		registry "MuleArabic-2"
		chars 94
		columns 2
		direction r2l
		final ?4
		graphic 0
		short-name "Arabic 2-col"
		long-name "Arabic 2-column"
		))

(make-coding-system 'iso-8859-6 'iso2022
		    "ISO-8859-6 (Arabic)"
		    '(charset-g0 ascii
				 charset-g1 arabic-iso8859-6
				 charset-g2 t
				 charset-g3 t
				 no-iso6429 t
				 mnemonic "MIME/Arbc"
				 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARABIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-language-environment 'arabic
;;   "Arabic"
;;   (lambda ()
;;     (require 'arabic)))
=======
;; ISO 8859-6 is such a useless character set that it seems a waste of
;; codespace to dump it. Let me count the ways: 
;; 
;; 1. It doesn't support Persian or Urdu, let alone Sinhalese, despite
;;    plenty of unallocated code points.
;;
;; 2. It doesn't encode all the vowel diacritics (the Harakaat) despite that
;;    they are necessary, even for the Arabs, for basic things like
;;    dictionary entries, children's books, and occasional disambiguation.
;;
;; 3. The Arabs don't use it, they use Windows-1256, which also supports
;;    Persian, at least, as well as the French characters necessary in
;;    Lebanon and North Africa.

;; But; it's necessary for input on X11.

(make-charset
 'arabic-iso8859-6 
 "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"
 '(dimension 1
   registry "ISO8859-6"
   chars 96
   columns 1
   direction r2l
   final ?G
   graphic 1
   short-name "RHP of ISO8859/6"
   long-name "RHP of Arabic (ISO 8859-6): ISO-IR-127"))

(make-coding-system
 'iso-8859-6 'fixed-width "ISO 8859-6 (Arabic)"
 '(unicode-map
   ((#x80 ?\u0080) ;; <control>
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
    (#xA4 ?\u00A4) ;; CURRENCY SIGN
    (#xAC ?\u060C) ;; ARABIC COMMA
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xBB ?\u061B) ;; ARABIC SEMICOLON
    (#xBF ?\u061F) ;; ARABIC QUESTION MARK
    (#xC1 ?\u0621) ;; ARABIC LETTER HAMZA
    (#xC2 ?\u0622) ;; ARABIC LETTER ALEF WITH MADDA ABOVE
    (#xC3 ?\u0623) ;; ARABIC LETTER ALEF WITH HAMZA ABOVE
    (#xC4 ?\u0624) ;; ARABIC LETTER WAW WITH HAMZA ABOVE
    (#xC5 ?\u0625) ;; ARABIC LETTER ALEF WITH HAMZA BELOW
    (#xC6 ?\u0626) ;; ARABIC LETTER YEH WITH HAMZA ABOVE
    (#xC7 ?\u0627) ;; ARABIC LETTER ALEF
    (#xC8 ?\u0628) ;; ARABIC LETTER BEH
    (#xC9 ?\u0629) ;; ARABIC LETTER TEH MARBUTA
    (#xCA ?\u062A) ;; ARABIC LETTER TEH
    (#xCB ?\u062B) ;; ARABIC LETTER THEH
    (#xCC ?\u062C) ;; ARABIC LETTER JEEM
    (#xCD ?\u062D) ;; ARABIC LETTER HAH
    (#xCE ?\u062E) ;; ARABIC LETTER KHAH
    (#xCF ?\u062F) ;; ARABIC LETTER DAL
    (#xD0 ?\u0630) ;; ARABIC LETTER THAL
    (#xD1 ?\u0631) ;; ARABIC LETTER REH
    (#xD2 ?\u0632) ;; ARABIC LETTER ZAIN
    (#xD3 ?\u0633) ;; ARABIC LETTER SEEN
    (#xD4 ?\u0634) ;; ARABIC LETTER SHEEN
    (#xD5 ?\u0635) ;; ARABIC LETTER SAD
    (#xD6 ?\u0636) ;; ARABIC LETTER DAD
    (#xD7 ?\u0637) ;; ARABIC LETTER TAH
    (#xD8 ?\u0638) ;; ARABIC LETTER ZAH
    (#xD9 ?\u0639) ;; ARABIC LETTER AIN
    (#xDA ?\u063A) ;; ARABIC LETTER GHAIN
    (#xE0 ?\u0640) ;; ARABIC TATWEEL
    (#xE1 ?\u0641) ;; ARABIC LETTER FEH
    (#xE2 ?\u0642) ;; ARABIC LETTER QAF
    (#xE3 ?\u0643) ;; ARABIC LETTER KAF
    (#xE4 ?\u0644) ;; ARABIC LETTER LAM
    (#xE5 ?\u0645) ;; ARABIC LETTER MEEM
    (#xE6 ?\u0646) ;; ARABIC LETTER NOON
    (#xE7 ?\u0647) ;; ARABIC LETTER HEH
    (#xE8 ?\u0648) ;; ARABIC LETTER WAW
    (#xE9 ?\u0649) ;; ARABIC LETTER ALEF MAKSURA
    (#xEA ?\u064A) ;; ARABIC LETTER YEH
    (#xEB ?\u064B) ;; ARABIC FATHATAN
    (#xEC ?\u064C) ;; ARABIC DAMMATAN
    (#xED ?\u064D) ;; ARABIC KASRATAN
    (#xEE ?\u064E) ;; ARABIC FATHA
    (#xEF ?\u064F) ;; ARABIC DAMMA
    (#xF0 ?\u0650) ;; ARABIC KASRA
    (#xF1 ?\u0651) ;; ARABIC SHADDA
    (#xF2 ?\u0652)) ;; ARABIC SUKUN
   mnemonic "ArISO"))

(make-coding-system
 'windows-1256 'fixed-width "Windows-1256 (Arabic)"
 '(unicode-map
   ((#x80 ?\u20AC) ;; EURO SIGN
    (#x81 ?\u067E) ;; ARABIC LETTER PEH
    (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 ?\u0192) ;; LATIN SMALL LETTER F WITH HOOK
    (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x86 ?\u2020) ;; DAGGER
    (#x87 ?\u2021) ;; DOUBLE DAGGER
    (#x88 ?\u02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    (#x89 ?\u2030) ;; PER MILLE SIGN
    (#x8A ?\u0679) ;; ARABIC LETTER TTEH
    (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C ?\u0152) ;; LATIN CAPITAL LIGATURE OE
    (#x8D ?\u0686) ;; ARABIC LETTER TCHEH
    (#x8E ?\u0698) ;; ARABIC LETTER JEH
    (#x8F ?\u0688) ;; ARABIC LETTER DDAL
    (#x90 ?\u06AF) ;; ARABIC LETTER GAF
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#x98 ?\u06A9) ;; ARABIC LETTER KEHEH
    (#x99 ?\u2122) ;; TRADE MARK SIGN
    (#x9A ?\u0691) ;; ARABIC LETTER RREH
    (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C ?\u0153) ;; LATIN SMALL LIGATURE OE
    (#x9D ?\u200C) ;; ZERO WIDTH NON-JOINER
    (#x9E ?\u200D) ;; ZERO WIDTH JOINER
    (#x9F ?\u06BA) ;; ARABIC LETTER NOON GHUNNA
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u060C) ;; ARABIC COMMA
    (#xA2 ?\u00A2) ;; CENT SIGN
    (#xA3 ?\u00A3) ;; POUND SIGN
    (#xA4 ?\u00A4) ;; CURRENCY SIGN
    (#xA5 ?\u00A5) ;; YEN SIGN
    (#xA6 ?\u00A6) ;; BROKEN BAR
    (#xA7 ?\u00A7) ;; SECTION SIGN
    (#xA8 ?\u00A8) ;; DIAERESIS
    (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
    (#xAA ?\u06BE) ;; ARABIC LETTER HEH DOACHASHMEE
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
    (#xBA ?\u061B) ;; ARABIC SEMICOLON
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC ?\u00BC) ;; VULGAR FRACTION ONE QUARTER
    (#xBD ?\u00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE ?\u00BE) ;; VULGAR FRACTION THREE QUARTERS
    (#xBF ?\u061F) ;; ARABIC QUESTION MARK
    (#xC0 ?\u06C1) ;; ARABIC LETTER HEH GOAL
    (#xC1 ?\u0621) ;; ARABIC LETTER HAMZA
    (#xC2 ?\u0622) ;; ARABIC LETTER ALEF WITH MADDA ABOVE
    (#xC3 ?\u0623) ;; ARABIC LETTER ALEF WITH HAMZA ABOVE
    (#xC4 ?\u0624) ;; ARABIC LETTER WAW WITH HAMZA ABOVE
    (#xC5 ?\u0625) ;; ARABIC LETTER ALEF WITH HAMZA BELOW
    (#xC6 ?\u0626) ;; ARABIC LETTER YEH WITH HAMZA ABOVE
    (#xC7 ?\u0627) ;; ARABIC LETTER ALEF
    (#xC8 ?\u0628) ;; ARABIC LETTER BEH
    (#xC9 ?\u0629) ;; ARABIC LETTER TEH MARBUTA
    (#xCA ?\u062A) ;; ARABIC LETTER TEH
    (#xCB ?\u062B) ;; ARABIC LETTER THEH
    (#xCC ?\u062C) ;; ARABIC LETTER JEEM
    (#xCD ?\u062D) ;; ARABIC LETTER HAH
    (#xCE ?\u062E) ;; ARABIC LETTER KHAH
    (#xCF ?\u062F) ;; ARABIC LETTER DAL
    (#xD0 ?\u0630) ;; ARABIC LETTER THAL
    (#xD1 ?\u0631) ;; ARABIC LETTER REH
    (#xD2 ?\u0632) ;; ARABIC LETTER ZAIN
    (#xD3 ?\u0633) ;; ARABIC LETTER SEEN
    (#xD4 ?\u0634) ;; ARABIC LETTER SHEEN
    (#xD5 ?\u0635) ;; ARABIC LETTER SAD
    (#xD6 ?\u0636) ;; ARABIC LETTER DAD
    (#xD7 ?\u00D7) ;; MULTIPLICATION SIGN
    (#xD8 ?\u0637) ;; ARABIC LETTER TAH
    (#xD9 ?\u0638) ;; ARABIC LETTER ZAH
    (#xDA ?\u0639) ;; ARABIC LETTER AIN
    (#xDB ?\u063A) ;; ARABIC LETTER GHAIN
    (#xDC ?\u0640) ;; ARABIC TATWEEL
    (#xDD ?\u0641) ;; ARABIC LETTER FEH
    (#xDE ?\u0642) ;; ARABIC LETTER QAF
    (#xDF ?\u0643) ;; ARABIC LETTER KAF
    (#xE0 ?\u00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 ?\u0644) ;; ARABIC LETTER LAM
    (#xE2 ?\u00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 ?\u0645) ;; ARABIC LETTER MEEM
    (#xE4 ?\u0646) ;; ARABIC LETTER NOON
    (#xE5 ?\u0647) ;; ARABIC LETTER HEH
    (#xE6 ?\u0648) ;; ARABIC LETTER WAW
    (#xE7 ?\u00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 ?\u00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 ?\u00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA ?\u00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB ?\u00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC ?\u0649) ;; ARABIC LETTER ALEF MAKSURA
    (#xED ?\u064A) ;; ARABIC LETTER YEH
    (#xEE ?\u00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF ?\u00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 ?\u064B) ;; ARABIC FATHATAN
    (#xF1 ?\u064C) ;; ARABIC DAMMATAN
    (#xF2 ?\u064D) ;; ARABIC KASRATAN
    (#xF3 ?\u064E) ;; ARABIC FATHA
    (#xF4 ?\u00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 ?\u064F) ;; ARABIC DAMMA
    (#xF6 ?\u0650) ;; ARABIC KASRA
    (#xF7 ?\u00F7) ;; DIVISION SIGN
    (#xF8 ?\u0651) ;; ARABIC SHADDA
    (#xF9 ?\u00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA ?\u0652) ;; ARABIC SUKUN
    (#xFB ?\u00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC ?\u00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD ?\u200E) ;; LEFT-TO-RIGHT MARK
    (#xFE ?\u200F) ;; RIGHT-TO-LEFT MARK
    (#xFF ?\u06D2)) ;; ARABIC LETTER YEH BARREE
   mnemonic "cp1256"
   documentation
   "This is the much Windows encoding for Arabic, much superior to the ISO
standard one."
   aliases (cp1256)))

;; The Mac Arabic coding systems don't have defined MIME names. 

;; #### Decide what to do about the syntax of the Arabic punctuation. 
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/arabic.el~other.atcHn-

;;; arabic.el ends here
