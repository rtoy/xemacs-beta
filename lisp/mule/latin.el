;;; latin.el --- Roman-alphabet languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001 Ben Wing.
;; Copyright (C) 2002, 2005, 2006 Free Software Foundation

;; Keywords: multilingual, latin, dumped

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

;; For Roman-alphabet-using Europeans, eight coded character sets,
;; ISO8859-1,2,3,4,9,14,15,16 are supported.

;;; Code:

;; Case table setup.  We set up all the case tables using
;; put-case-table-pair.  The data for this comes from FSF Emacs 20.7
;; (lisp/international/latin-*.el), written by several people and
;; updated by Erik Naggum.

(defun setup-case-pairs (charset pairs)
  (loop 
    for (uc lc) in pairs 
    with table = (standard-case-table)
    do (put-case-table-pair
        (make-char charset uc) (make-char charset lc) table)))

;; Latin-1's case is dealt with in iso8859-1.el, which see. Its syntax is
;; initialised in syntax.c:complex_vars_of_syntax.


;; Latin-2 (ISO-8859-2). Central Europe; Czech, Slovak, Hungarian, Polish,
;; Croatian, other languages.
;;
;; (Yes, it really is Central European. German written in Latin 2 and using
;; only Umlaute and the sharp S in its non-ASCII repertoire is bit-for-bit
;; identical with the same text in Latin-1.)

;; The default character syntax is now word. Pay attention to the
;; exceptions in ISO-8859-2, copying them from ISO-8859-1. 
(loop
  for (latin-2 latin-1) 
  in '((#xA0 #xA0) ;; NO BREAK SPACE
       (#xA2 #xB4) ;; BREVE, ACUTE ACCENT
       (#xA4 #xA4) ;; CURRENCY SIGN
       (#xA7 #xA7) ;; SECTION SIGN
       (#xA8 #xA8) ;; DIAERESIS
       (#xAD #xAD) ;; SOFT HYPHEN
       (#xB0 #xB0) ;; DEGREE SIGN
       (#xB2 #xB4) ;; OGONEK, ACUTE ACCENT
       (#xB4 #xB4) ;; ACUTE ACCENT
       (#xB7 #xB4) ;; CARON, ACUTE ACCENT
       (#xB8 #xB8) ;; CEDILLA
       (#xBD #xB4) ;; DOUBLE ACUTE ACCENT, ACUTE ACCENT
       (#xD7 #xD7) ;; MULTIPLICATION SIGN
       (#xF7 #xF7) ;; DIVISION SIGN
       (#xFF #xB4)) ;; DOT ABOVE, ACUTE ACCENT
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'latin-iso8859-2 latin-2)
      (string (char-syntax (make-char 'latin-iso8859-1 latin-1)))
      syntax-table))

;; Case. 
(setup-case-pairs
 'latin-iso8859-2
 '((#xA1 #xB1) ;; A WITH OGONEK
   (#xA3 #xB3) ;; L WITH STROKE
   (#xA5 #xB5) ;; L WITH CARON
   (#xA6 #xB6) ;; S WITH ACUTE
   (#xA9 #xB9) ;; S WITH CARON
   (#xAA #xBA) ;; S WITH CEDILLA
   (#xAB #xBB) ;; T WITH CARON
   (#xAC #xBC) ;; Z WITH ACUTE
   (#xAE #xBE) ;; Z WITH CARON
   (#xAF #xBF) ;; Z WITH DOT ABOVE
   (#xC0 #xE0) ;; R WITH ACUTE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH BREVE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; L WITH ACUTE
   (#xC6 #xE6) ;; C WITH ACUTE
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; C WITH CARON
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH OGONEK
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; E WITH CARON
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; D WITH CARON
   (#xD0 #xF0) ;; D WITH STROKE
   (#xD1 #xF1) ;; N WITH ACUTE
   (#xD2 #xF2) ;; N WITH CARON
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH DOUBLE ACUTE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD8 #xF8) ;; R WITH CARON
   (#xD9 #xF9) ;; U WITH RING ABOVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH DOUBLE ACUTE
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; Y WITH ACUTE
   (#xDE #xFE))) ;; T WITH CEDILLA

(make-8-bit-coding-system
 'iso-8859-2
 '((#xA1 ?\u0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
   (#xA2 ?\u02D8) ;; BREVE
   (#xA3 ?\u0141) ;; LATIN CAPITAL LETTER L WITH STROKE
   (#xA5 ?\u013D) ;; LATIN CAPITAL LETTER L WITH CARON
   (#xA6 ?\u015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
   (#xA9 ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#xAA ?\u015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
   (#xAB ?\u0164) ;; LATIN CAPITAL LETTER T WITH CARON
   (#xAC ?\u0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
   (#xAE ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#xAF ?\u017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
   (#xB1 ?\u0105) ;; LATIN SMALL LETTER A WITH OGONEK
   (#xB2 ?\u02DB) ;; OGONEK
   (#xB3 ?\u0142) ;; LATIN SMALL LETTER L WITH STROKE
   (#xB5 ?\u013E) ;; LATIN SMALL LETTER L WITH CARON
   (#xB6 ?\u015B) ;; LATIN SMALL LETTER S WITH ACUTE
   (#xB7 ?\u02C7) ;; CARON
   (#xB9 ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#xBA ?\u015F) ;; LATIN SMALL LETTER S WITH CEDILLA
   (#xBB ?\u0165) ;; LATIN SMALL LETTER T WITH CARON
   (#xBC ?\u017A) ;; LATIN SMALL LETTER Z WITH ACUTE
   (#xBD ?\u02DD) ;; DOUBLE ACUTE ACCENT
   (#xBE ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#xBF ?\u017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
   (#xC0 ?\u0154) ;; LATIN CAPITAL LETTER R WITH ACUTE
   (#xC3 ?\u0102) ;; LATIN CAPITAL LETTER A WITH BREVE
   (#xC5 ?\u0139) ;; LATIN CAPITAL LETTER L WITH ACUTE
   (#xC6 ?\u0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
   (#xC8 ?\u010C) ;; LATIN CAPITAL LETTER C WITH CARON
   (#xCA ?\u0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
   (#xCC ?\u011A) ;; LATIN CAPITAL LETTER E WITH CARON
   (#xCF ?\u010E) ;; LATIN CAPITAL LETTER D WITH CARON
   (#xD0 ?\u0110) ;; LATIN CAPITAL LETTER D WITH STROKE
   (#xD1 ?\u0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
   (#xD2 ?\u0147) ;; LATIN CAPITAL LETTER N WITH CARON
   (#xD5 ?\u0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
   (#xD8 ?\u0158) ;; LATIN CAPITAL LETTER R WITH CARON
   (#xD9 ?\u016E) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
   (#xDB ?\u0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
   (#xDE ?\u0162) ;; LATIN CAPITAL LETTER T WITH CEDILLA
   (#xE0 ?\u0155) ;; LATIN SMALL LETTER R WITH ACUTE
   (#xE3 ?\u0103) ;; LATIN SMALL LETTER A WITH BREVE
   (#xE5 ?\u013A) ;; LATIN SMALL LETTER L WITH ACUTE
   (#xE6 ?\u0107) ;; LATIN SMALL LETTER C WITH ACUTE
   (#xE8 ?\u010D) ;; LATIN SMALL LETTER C WITH CARON
   (#xEA ?\u0119) ;; LATIN SMALL LETTER E WITH OGONEK
   (#xEC ?\u011B) ;; LATIN SMALL LETTER E WITH CARON
   (#xEF ?\u010F) ;; LATIN SMALL LETTER D WITH CARON
   (#xF0 ?\u0111) ;; LATIN SMALL LETTER D WITH STROKE
   (#xF1 ?\u0144) ;; LATIN SMALL LETTER N WITH ACUTE
   (#xF2 ?\u0148) ;; LATIN SMALL LETTER N WITH CARON
   (#xF5 ?\u0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
   (#xF8 ?\u0159) ;; LATIN SMALL LETTER R WITH CARON
   (#xF9 ?\u016F) ;; LATIN SMALL LETTER U WITH RING ABOVE
   (#xFB ?\u0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
   (#xFE ?\u0163) ;; LATIN SMALL LETTER T WITH CEDILLA
   (#xFF ?\u02D9));; DOT ABOVE
 "ISO-8859-2 (Latin-2) for Central Europe.
See also `windows-1250', and `iso-8859-1', which is compatible with Latin 2
when used to write German (or English, of course).  "
 '(mnemonic "Latin 2"
   aliases (iso-latin-2 latin-2)))

(make-8-bit-coding-system
 'windows-1250
 '((#x80 ?\u20AC) ;; EURO SIGN
   (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
   (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
   (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
   (#x86 ?\u2020) ;; DAGGER
   (#x87 ?\u2021) ;; DOUBLE DAGGER
   (#x89 ?\u2030) ;; PER MILLE SIGN
   (#x8A ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
   (#x8C ?\u015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
   (#x8D ?\u0164) ;; LATIN CAPITAL LETTER T WITH CARON
   (#x8E ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#x8F ?\u0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
   (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
   (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
   (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
   (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
   (#x95 ?\u2022) ;; BULLET
   (#x96 ?\u2013) ;; EN DASH
   (#x97 ?\u2014) ;; EM DASH
   (#x99 ?\u2122) ;; TRADE MARK SIGN
   (#x9A ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
   (#x9C ?\u015B) ;; LATIN SMALL LETTER S WITH ACUTE
   (#x9D ?\u0165) ;; LATIN SMALL LETTER T WITH CARON
   (#x9E ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#x9F ?\u017A) ;; LATIN SMALL LETTER Z WITH ACUTE
   (#xA0 ?\u00A0) ;; NO-BREAK SPACE
   (#xA1 ?\u02C7) ;; CARON
   (#xA2 ?\u02D8) ;; BREVE
   (#xA3 ?\u0141) ;; LATIN CAPITAL LETTER L WITH STROKE
   (#xA4 ?\u00A4) ;; CURRENCY SIGN
   (#xA5 ?\u0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
   (#xA6 ?\u00A6) ;; BROKEN BAR
   (#xA7 ?\u00A7) ;; SECTION SIGN
   (#xA8 ?\u00A8) ;; DIAERESIS
   (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
   (#xAA ?\u015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
   (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xAC ?\u00AC) ;; NOT SIGN
   (#xAD ?\u00AD) ;; SOFT HYPHEN
   (#xAE ?\u00AE) ;; REGISTERED SIGN
   (#xAF ?\u017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
   (#xB0 ?\u00B0) ;; DEGREE SIGN
   (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
   (#xB2 ?\u02DB) ;; OGONEK
   (#xB3 ?\u0142) ;; LATIN SMALL LETTER L WITH STROKE
   (#xB4 ?\u00B4) ;; ACUTE ACCENT
   (#xB5 ?\u00B5) ;; MICRO SIGN
   (#xB6 ?\u00B6) ;; PILCROW SIGN
   (#xB7 ?\u00B7) ;; MIDDLE DOT
   (#xB8 ?\u00B8) ;; CEDILLA
   (#xB9 ?\u0105) ;; LATIN SMALL LETTER A WITH OGONEK
   (#xBA ?\u015F) ;; LATIN SMALL LETTER S WITH CEDILLA
   (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xBC ?\u013D) ;; LATIN CAPITAL LETTER L WITH CARON
   (#xBD ?\u02DD) ;; DOUBLE ACUTE ACCENT
   (#xBE ?\u013E) ;; LATIN SMALL LETTER L WITH CARON
   (#xBF ?\u017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
   (#xC0 ?\u0154) ;; LATIN CAPITAL LETTER R WITH ACUTE
   (#xC1 ?\u00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
   (#xC2 ?\u00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
   (#xC3 ?\u0102) ;; LATIN CAPITAL LETTER A WITH BREVE
   (#xC4 ?\u00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
   (#xC5 ?\u0139) ;; LATIN CAPITAL LETTER L WITH ACUTE
   (#xC6 ?\u0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
   (#xC7 ?\u00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
   (#xC8 ?\u010C) ;; LATIN CAPITAL LETTER C WITH CARON
   (#xC9 ?\u00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
   (#xCA ?\u0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
   (#xCB ?\u00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
   (#xCC ?\u011A) ;; LATIN CAPITAL LETTER E WITH CARON
   (#xCD ?\u00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
   (#xCE ?\u00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
   (#xCF ?\u010E) ;; LATIN CAPITAL LETTER D WITH CARON
   (#xD0 ?\u0110) ;; LATIN CAPITAL LETTER D WITH STROKE
   (#xD1 ?\u0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
   (#xD2 ?\u0147) ;; LATIN CAPITAL LETTER N WITH CARON
   (#xD3 ?\u00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
   (#xD4 ?\u00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
   (#xD5 ?\u0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
   (#xD6 ?\u00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
   (#xD7 ?\u00D7) ;; MULTIPLICATION SIGN
   (#xD8 ?\u0158) ;; LATIN CAPITAL LETTER R WITH CARON
   (#xD9 ?\u016E) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
   (#xDA ?\u00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
   (#xDB ?\u0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
   (#xDC ?\u00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
   (#xDD ?\u00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
   (#xDE ?\u0162) ;; LATIN CAPITAL LETTER T WITH CEDILLA
   (#xDF ?\u00DF) ;; LATIN SMALL LETTER SHARP S
   (#xE0 ?\u0155) ;; LATIN SMALL LETTER R WITH ACUTE
   (#xE1 ?\u00E1) ;; LATIN SMALL LETTER A WITH ACUTE
   (#xE2 ?\u00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
   (#xE3 ?\u0103) ;; LATIN SMALL LETTER A WITH BREVE
   (#xE4 ?\u00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
   (#xE5 ?\u013A) ;; LATIN SMALL LETTER L WITH ACUTE
   (#xE6 ?\u0107) ;; LATIN SMALL LETTER C WITH ACUTE
   (#xE7 ?\u00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
   (#xE8 ?\u010D) ;; LATIN SMALL LETTER C WITH CARON
   (#xE9 ?\u00E9) ;; LATIN SMALL LETTER E WITH ACUTE
   (#xEA ?\u0119) ;; LATIN SMALL LETTER E WITH OGONEK
   (#xEB ?\u00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
   (#xEC ?\u011B) ;; LATIN SMALL LETTER E WITH CARON
   (#xED ?\u00ED) ;; LATIN SMALL LETTER I WITH ACUTE
   (#xEE ?\u00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
   (#xEF ?\u010F) ;; LATIN SMALL LETTER D WITH CARON
   (#xF0 ?\u0111) ;; LATIN SMALL LETTER D WITH STROKE
   (#xF1 ?\u0144) ;; LATIN SMALL LETTER N WITH ACUTE
   (#xF2 ?\u0148) ;; LATIN SMALL LETTER N WITH CARON
   (#xF3 ?\u00F3) ;; LATIN SMALL LETTER O WITH ACUTE
   (#xF4 ?\u00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
   (#xF5 ?\u0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
   (#xF6 ?\u00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
   (#xF7 ?\u00F7) ;; DIVISION SIGN
   (#xF8 ?\u0159) ;; LATIN SMALL LETTER R WITH CARON
   (#xF9 ?\u016F) ;; LATIN SMALL LETTER U WITH RING ABOVE
   (#xFA ?\u00FA) ;; LATIN SMALL LETTER U WITH ACUTE
   (#xFB ?\u0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
   (#xFC ?\u00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
   (#xFD ?\u00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
   (#xFE ?\u0163) ;; LATIN SMALL LETTER T WITH CEDILLA
   (#xFF ?\u02D9)) ;; DOT ABOVE
 "CP 1250, Microsoft's encoding for Central Europe. 
See also `iso-8859-2' and `window-1252' for Western Europe.  "
 '(mnemonic "CP1250"
   aliases (cp1250)))


;; 
;; Latin-3 (ISO-8859-3). Esperanto, Maltese and Turkish. Obsolescent.

;; Initialise the non-word syntax codes in ISO-8859-3, copying them from
;; ISO-8859-1.
(loop
  for (latin-3 latin-1) 
  in '((#xA0 #xA0) ;; NO BREAK SPACE
       (#xA2 #xB4) ;; BREVE, ACUTE ACCENT
       (#xA3 #xA3) ;; POUND SIGN
       (#xA4 #xA4) ;; CURRENCY SIGN
       (#xA7 #xA7) ;; SECTION SIGN
       (#xA8 #xA8) ;; DIAERESIS
       (#xAD #xAD) ;; SOFT HYPHEN
       (#xB0 #xB0) ;; DEGREE SIGN
       (#xB2 #xB2) ;; SUPERSCRIPT TWO
       (#xB3 #xB3) ;; SUPERSCRIPT THREE
       (#xB4 #xB4) ;; ACUTE ACCENT
       (#xB5 #xB5) ;; MICRO SIGN
       (#xB7 #xB7) ;; MIDDLE DOT
       (#xB8 #xB8) ;; CEDILLA
       (#xBD #xBD) ;; VULGAR FRACTION ONE HALF
       (#xD7 #xD7) ;; MULTIPLICATION SIGN
       (#xF7 #xF7) ;; DIVISION SIGN
       (#xFF #xB4)) ;; DOT ABOVE, ACUTE ACCENT
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'latin-iso8859-3 latin-3)
      (string (char-syntax (make-char 'latin-iso8859-1 latin-1)))
      syntax-table))

;; Case. 
(setup-case-pairs
 'latin-iso8859-3
 '((#xA1 #xB1) ;; H WITH STROKE
   (#xA6 #xB6) ;; H WITH CIRCUMFLEX
   (#xAA #xBA) ;; S WITH CEDILLA
   (#xAB #xBB) ;; G WITH BREVE
   (#xAC #xBC) ;; J WITH CIRCUMFLEX
   (#xAF #xBF) ;; Z WITH DOT ABOVE
   (#xC0 #xE0) ;; A WITH GRAVE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; C WITH DOT ABOVE
   (#xC6 #xE6) ;; C WITH CIRCUMFLEX
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; E WITH GRAVE
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH CIRCUMFLEX
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; I WITH GRAVE
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; I WITH DIAERESIS
   (#xD1 #xF1) ;; N WITH TILDE
   (#xD2 #xF2) ;; O WITH GRAVE
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; G WITH DOT ABOVE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD8 #xF8) ;; G WITH CIRCUMFLEX
   (#xD9 #xF9) ;; U WITH GRAVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; U WITH BREVE
   (#xDE #xFE))) ;; S WITH CIRCUMFLEX

(make-8-bit-coding-system
 'iso-8859-3
 '((#xA1 ?\u0126) ;; LATIN CAPITAL LETTER H WITH STROKE
   (#xA2 ?\u02D8) ;; BREVE
   (#xA6 ?\u0124) ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
   (#xA9 ?\u0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
   (#xAA ?\u015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
   (#xAB ?\u011E) ;; LATIN CAPITAL LETTER G WITH BREVE
   (#xAC ?\u0134) ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
   (#xAF ?\u017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
   (#xB1 ?\u0127) ;; LATIN SMALL LETTER H WITH STROKE
   (#xB6 ?\u0125) ;; LATIN SMALL LETTER H WITH CIRCUMFLEX
   (#xB9 ?\u0131) ;; LATIN SMALL LETTER DOTLESS I
   (#xBA ?\u015F) ;; LATIN SMALL LETTER S WITH CEDILLA
   (#xBB ?\u011F) ;; LATIN SMALL LETTER G WITH BREVE
   (#xBC ?\u0135) ;; LATIN SMALL LETTER J WITH CIRCUMFLEX
   (#xBF ?\u017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
   (#xC5 ?\u010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
   (#xC6 ?\u0108) ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
   (#xD5 ?\u0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
   (#xD8 ?\u011C) ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
   (#xDD ?\u016C) ;; LATIN CAPITAL LETTER U WITH BREVE
   (#xDE ?\u015C) ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
   (#xE5 ?\u010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
   (#xE6 ?\u0109) ;; LATIN SMALL LETTER C WITH CIRCUMFLEX
   (#xF5 ?\u0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
   (#xF8 ?\u011D) ;; LATIN SMALL LETTER G WITH CIRCUMFLEX
   (#xFD ?\u016D) ;; LATIN SMALL LETTER U WITH BREVE
   (#xFE ?\u015D) ;; LATIN SMALL LETTER S WITH CIRCUMFLEX
   (#xFF ?\u02D9)) ;; DOT ABOVE
 "ISO-8859-3 (Latin-3)"
 '(mnemonic "Latin 3"
   documentation "Aimed at Turkish, Maltese and Esperanto.  "
   aliases (iso-latin-3 latin-3)))


;; Latin-4 (ISO-8859-4)

;; Estonian, Latvian, Lithuanian, Greenlandic, and Sami. Obsolescent.

;; The default character syntax is now word. Pay attention to the
;; exceptions in ISO-8859-4, copying them from ISO-8859-1. 
(loop
  for (latin-4 latin-1) 
  in '((#xA0 #xA0) ;; NO BREAK SPACE
       (#xA4 #xA4) ;; CURRENCY SIGN
       (#xA7 #xA7) ;; SECTION SIGN
       (#xA8 #xA8) ;; DIAERESIS
       (#xAD #xAD) ;; SOFT HYPHEN
       (#xB0 #xB0) ;; DEGREE SIGN
       (#xB2 #xB4) ;; OGONEK, ACUTE ACCENT
       (#xB4 #xB4) ;; ACUTE ACCENT
       (#xB7 #xB4) ;; CARON, ACUTE ACCENT
       (#xB8 #xB8) ;; CEDILLA
       (#xD7 #xD7) ;; MULTIPLICATION SIGN
       (#xF7 #xF7) ;; DIVISION SIGN
       (#xFF #xB4)) ;; DOT ABOVE, ACUTE ACCENT
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'latin-iso8859-4 latin-4)
      (string (char-syntax (make-char 'latin-iso8859-1 latin-1)))
      syntax-table))

;; Case. 
(setup-case-pairs
 'latin-iso8859-4
 '((#xA1 #xB1) ;; A WITH OGONEK
   (#xA3 #xB3) ;; R WITH CEDILLA
   (#xA5 #xB5) ;; I WITH TILDE
   (#xA6 #xB6) ;; L WITH CEDILLA
   (#xA9 #xB9) ;; S WITH CARON
   (#xAA #xBA) ;; E WITH MACRON
   (#xAB #xBB) ;; G WITH CEDILLA
   (#xAC #xBC) ;; T WITH STROKE
   (#xAE #xBE) ;; Z WITH CARON
   (#xBD #xBF) ;; ENG
   (#xC0 #xE0) ;; A WITH MACRON
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH TILDE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; A WITH RING ABOVE
   (#xC6 #xE6) ;; AE
   (#xC7 #xE7) ;; I WITH OGONEK
   (#xC8 #xE8) ;; C WITH CARON
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH OGONEK
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; E WITH DOT ABOVE
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; I WITH MACRON
   (#xD0 #xF0) ;; D WITH STROKE
   (#xD1 #xF1) ;; N WITH CEDILLA
   (#xD2 #xF2) ;; O WITH MACRON
   (#xD3 #xF3) ;; K WITH CEDILLA
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH TILDE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD8 #xF8) ;; O WITH STROKE
   (#xD9 #xF9) ;; U WITH OGONEK
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; U WITH TILDE
   (#xDE #xFE))) ;; U WITH MACRON

(make-8-bit-coding-system
 'iso-8859-4
 '((#xA1 ?\u0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
   (#xA2 ?\u0138) ;; LATIN SMALL LETTER KRA
   (#xA3 ?\u0156) ;; LATIN CAPITAL LETTER R WITH CEDILLA
   (#xA5 ?\u0128) ;; LATIN CAPITAL LETTER I WITH TILDE
   (#xA6 ?\u013B) ;; LATIN CAPITAL LETTER L WITH CEDILLA
   (#xA9 ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#xAA ?\u0112) ;; LATIN CAPITAL LETTER E WITH MACRON
   (#xAB ?\u0122) ;; LATIN CAPITAL LETTER G WITH CEDILLA
   (#xAC ?\u0166) ;; LATIN CAPITAL LETTER T WITH STROKE
   (#xAE ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#xB1 ?\u0105) ;; LATIN SMALL LETTER A WITH OGONEK
   (#xB2 ?\u02DB) ;; OGONEK
   (#xB3 ?\u0157) ;; LATIN SMALL LETTER R WITH CEDILLA
   (#xB5 ?\u0129) ;; LATIN SMALL LETTER I WITH TILDE
   (#xB6 ?\u013C) ;; LATIN SMALL LETTER L WITH CEDILLA
   (#xB7 ?\u02C7) ;; CARON
   (#xB9 ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#xBA ?\u0113) ;; LATIN SMALL LETTER E WITH MACRON
   (#xBB ?\u0123) ;; LATIN SMALL LETTER G WITH CEDILLA
   (#xBC ?\u0167) ;; LATIN SMALL LETTER T WITH STROKE
   (#xBD ?\u014A) ;; LATIN CAPITAL LETTER ENG
   (#xBE ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#xBF ?\u014B) ;; LATIN SMALL LETTER ENG
   (#xC0 ?\u0100) ;; LATIN CAPITAL LETTER A WITH MACRON
   (#xC7 ?\u012E) ;; LATIN CAPITAL LETTER I WITH OGONEK
   (#xC8 ?\u010C) ;; LATIN CAPITAL LETTER C WITH CARON
   (#xCA ?\u0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
   (#xCC ?\u0116) ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
   (#xCF ?\u012A) ;; LATIN CAPITAL LETTER I WITH MACRON
   (#xD0 ?\u0110) ;; LATIN CAPITAL LETTER D WITH STROKE
   (#xD1 ?\u0145) ;; LATIN CAPITAL LETTER N WITH CEDILLA
   (#xD2 ?\u014C) ;; LATIN CAPITAL LETTER O WITH MACRON
   (#xD3 ?\u0136) ;; LATIN CAPITAL LETTER K WITH CEDILLA
   (#xD9 ?\u0172) ;; LATIN CAPITAL LETTER U WITH OGONEK
   (#xDD ?\u0168) ;; LATIN CAPITAL LETTER U WITH TILDE
   (#xDE ?\u016A) ;; LATIN CAPITAL LETTER U WITH MACRON
   (#xE0 ?\u0101) ;; LATIN SMALL LETTER A WITH MACRON
   (#xE7 ?\u012F) ;; LATIN SMALL LETTER I WITH OGONEK
   (#xE8 ?\u010D) ;; LATIN SMALL LETTER C WITH CARON
   (#xEA ?\u0119) ;; LATIN SMALL LETTER E WITH OGONEK
   (#xEC ?\u0117) ;; LATIN SMALL LETTER E WITH DOT ABOVE
   (#xEF ?\u012B) ;; LATIN SMALL LETTER I WITH MACRON
   (#xF0 ?\u0111) ;; LATIN SMALL LETTER D WITH STROKE
   (#xF1 ?\u0146) ;; LATIN SMALL LETTER N WITH CEDILLA
   (#xF2 ?\u014D) ;; LATIN SMALL LETTER O WITH MACRON
   (#xF3 ?\u0137) ;; LATIN SMALL LETTER K WITH CEDILLA
   (#xF9 ?\u0173) ;; LATIN SMALL LETTER U WITH OGONEK
   (#xFD ?\u0169) ;; LATIN SMALL LETTER U WITH TILDE
   (#xFE ?\u016B) ;; LATIN SMALL LETTER U WITH MACRON
   (#xFF ?\u02D9));; DOT ABOVE
 "ISO-8859-4 (Latin-4)"
 '(mnemonic "Latin 4"
   aliases (iso-latin-4 latin-4)
   documentation "Obsolete coding system for the Baltic rim.  "))


;; Latin-8 (ISO 8859-14) Celtic.

;; Never widely used. Current-orthography Gaelic, both Irish and Scots, is
;; easily written with Latin-1. Wikipedia says the same about Welsh.

(make-charset 'latin-iso8859-14 
	      "Right-Hand Part of Latin Alphabet 8 (ISO/IEC 8859-14)"
	      '(dimension 1
		registries ["ISO8859-14"]
		chars 96
		columns 1
		direction l2r
		final ?_
		graphic 1
		short-name "RHP of Latin-8"
		long-name "RHP of Latin-8 (ISO 8859-14)"))

;; 
;; Character syntax defaults to word. The exceptions here shared with Latin-1.
(dolist (code '(#xa0	;; NO BREAK SPACE
		#xa3	;; POUND SIGN
		#xa7	;; SECTION SIGN
		#xa9	;; COPYRIGHT
		#xad	;; SOFT HYPHEN
		#xae	;; REGISTERED
		#xb6))	;; PILCROW SIGN
  (modify-syntax-entry (make-char 'latin-iso8859-14 code)
                       (string (char-syntax (make-char 'latin-iso8859-1 code)))
                       (standard-syntax-table)))
;; Case. 
(setup-case-pairs
 'latin-iso8859-14
 '((#xA1 #xA2) ;; B WITH DOT ABOVE
   (#xA4 #xA5) ;; C WITH DOT ABOVE
   (#xA6 #xAB) ;; D WITH DOT ABOVE
   (#xA8 #xB8) ;; W WITH GRAVE
   (#xAA #xBA) ;; W WITH ACUTE
   (#xAC #xBC) ;; Y WITH GRAVE
   (#xAF #xFF) ;; Y WITH DIAERESIS
   (#xB0 #xB1) ;; F WITH DOT ABOVE
   (#xB2 #xB3) ;; G WITH DOT ABOVE
   (#xB4 #xB5) ;; M WITH DOT ABOVE
   (#xB7 #xB9) ;; P WITH DOT ABOVE
   (#xBB #xBF) ;; S WITH DOT ABOVE
   (#xBD #xBE) ;; W WITH DIAERESIS
   (#xC0 #xE0) ;; A WITH GRAVE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH TILDE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; A WITH RING ABOVE
   (#xC6 #xE6) ;; AE
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; E WITH GRAVE
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH CIRCUMFLEX
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; I WITH GRAVE
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; I WITH DIAERESIS
   (#xD0 #xF0) ;; W WITH CIRCUMFLEX
   (#xD1 #xF1) ;; N WITH TILDE
   (#xD2 #xF2) ;; O WITH GRAVE
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH TILDE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD7 #xF7) ;; T WITH DOT ABOVE
   (#xD8 #xF8) ;; O WITH STROKE
   (#xD9 #xF9) ;; U WITH GRAVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; Y WITH ACUTE
   (#xDE #xFE))) ;; Y WITH CIRCUMFLEX

(make-8-bit-coding-system
 'iso-8859-14
 '((#xA1 ?\u1E02) ;; LATIN CAPITAL LETTER B WITH DOT ABOVE
   (#xA2 ?\u1E03) ;; LATIN SMALL LETTER B WITH DOT ABOVE
   (#xA4 ?\u010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
   (#xA5 ?\u010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
   (#xA6 ?\u1E0A) ;; LATIN CAPITAL LETTER D WITH DOT ABOVE
   (#xA8 ?\u1E80) ;; LATIN CAPITAL LETTER W WITH GRAVE
   (#xAA ?\u1E82) ;; LATIN CAPITAL LETTER W WITH ACUTE
   (#xAB ?\u1E0B) ;; LATIN SMALL LETTER D WITH DOT ABOVE
   (#xAC ?\u1EF2) ;; LATIN CAPITAL LETTER Y WITH GRAVE
   (#xAF ?\u0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
   (#xB0 ?\u1E1E) ;; LATIN CAPITAL LETTER F WITH DOT ABOVE
   (#xB1 ?\u1E1F) ;; LATIN SMALL LETTER F WITH DOT ABOVE
   (#xB2 ?\u0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
   (#xB3 ?\u0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
   (#xB4 ?\u1E40) ;; LATIN CAPITAL LETTER M WITH DOT ABOVE
   (#xB5 ?\u1E41) ;; LATIN SMALL LETTER M WITH DOT ABOVE
   (#xB7 ?\u1E56) ;; LATIN CAPITAL LETTER P WITH DOT ABOVE
   (#xB8 ?\u1E81) ;; LATIN SMALL LETTER W WITH GRAVE
   (#xB9 ?\u1E57) ;; LATIN SMALL LETTER P WITH DOT ABOVE
   (#xBA ?\u1E83) ;; LATIN SMALL LETTER W WITH ACUTE
   (#xBB ?\u1E60) ;; LATIN CAPITAL LETTER S WITH DOT ABOVE
   (#xBC ?\u1EF3) ;; LATIN SMALL LETTER Y WITH GRAVE
   (#xBD ?\u1E84) ;; LATIN CAPITAL LETTER W WITH DIAERESIS
   (#xBE ?\u1E85) ;; LATIN SMALL LETTER W WITH DIAERESIS
   (#xBF ?\u1E61) ;; LATIN SMALL LETTER S WITH DOT ABOVE
   (#xD0 ?\u0174) ;; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
   (#xD7 ?\u1E6A) ;; LATIN CAPITAL LETTER T WITH DOT ABOVE
   (#xDE ?\u0176) ;; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
   (#xF0 ?\u0175) ;; LATIN SMALL LETTER W WITH CIRCUMFLEX
   (#xF7 ?\u1E6B) ;; LATIN SMALL LETTER T WITH DOT ABOVE
   (#xFE ?\u0177)) ;; LATIN SMALL LETTER Y WITH CIRCUMFLEX
 "ISO-8859-14 (Latin-8)"
 '(mnemonic "Latin 8"
   aliases (iso-latin-8 latin-8)))


;; The syntax table code for ISO 8859-15 and ISO 8859-16 requires that the
;; guillemets not have parenthesis syntax, which they used to have in the
;; past. See syntax.c:complex_vars_of_syntax.
(assert (not (memq (char-syntax (make-char 'latin-iso8859-1 #xAB)) '(?\( ?\))))
        t "This code assumes \xAB does not have parenthesis syntax.  ")

(assert (not (memq (char-syntax (make-char 'latin-iso8859-1 #xBB)) '(?\( ?\))))
        t "This code assumes \xBB does not have parenthesis syntax.  ")


;; Latin-9 (ISO-8859-15)
;;
;; Latin-1 plus Euro, plus a few accented characters for the sake of correct
;; Finnish and French orthography. Only ever widely used on Unix. 

;; 
;; Based on Latin-1 and differences therefrom.
;; 
;; First, initialise the syntax from the corresponding Latin-1 characters. 
(loop
  for c from #xa0 to #xff
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry 
      (make-char 'latin-iso8859-15 c)
      (string (char-syntax (make-char 'latin-iso8859-1 c)))
      syntax-table))

;; Now, the exceptions. The Euro sign retains the syntax of CURRENCY SIGN.
(loop
  for c in '(?,b&(B ?,b((B ?,b4(B ?,b8(B ?,b<(B ?,b=(B ?,b>(B)
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry c "w" syntax-table))

;; Case. 
(setup-case-pairs
 'latin-iso8859-15
 '((#xA6 #xA8) ;; S WITH CARON *
   (#xB4 #xB8) ;; Z WITH CARON *
   (#xBC #xBD) ;; LATIN LIGATURE OE *
   (#xBE #xFF) ;; Y WITH DIAERESIS *
   (#xC0 #xE0) ;; A WITH GRAVE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH TILDE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; A WITH RING ABOVE
   (#xC6 #xE6) ;; AE
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; E WITH GRAVE
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH CIRCUMFLEX
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; I WITH GRAVE
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; I WITH DIAERESIS
   (#xD0 #xF0) ;; ETH
   (#xD1 #xF1) ;; N WITH TILDE
   (#xD2 #xF2) ;; O WITH GRAVE
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH TILDE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD8 #xF8) ;; O WITH STROKE
   (#xD9 #xF9) ;; U WITH GRAVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; Y WITH ACUTE
   (#xDE #xFE))) ;; THORN

(make-8-bit-coding-system
 'iso-8859-15 
 '((#xA4 ?\u20AC) ;; EURO SIGN
   (#xA6 ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#xA8 ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#xB4 ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#xB8 ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#xBC ?\u0152) ;; LATIN CAPITAL LIGATURE OE
   (#xBD ?\u0153) ;; LATIN SMALL LIGATURE OE
   (#xBE ?\u0178)) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
 "ISO 4873 conforming 8-bit code (ASCII + Latin 9; aka Latin-1 with Euro)"
  '(mnemonic "Latin 9"
    aliases (iso-latin-9 latin-9 latin-0)))

;; end of ISO 8859-15. 

;;
;; Latin-10 (ISO 8859-16).
;;
;; "South-Eastern European." Not, to my knowledge, ever widely used. 

(make-charset 'latin-iso8859-16
	      "Right-Hand Part of Latin Alphabet 10 (ISO/IEC 8859-16)"
	      '(dimension 1
		registries ["ISO8859-16"]
		chars 96
		columns 1
		direction l2r
		final ?f			; octet 06/06; cf ISO-IR 226
		graphic 1
		short-name "RHP of Latin-10"
		long-name "RHP of Latin-10 (ISO 8859-16)"))

;; Copy over the non-word syntax this charset has in common with Latin 1.
(dolist (code '(#xa0	;; NO BREAK SPACE
		#xa7	;; SECTION SIGN
		#xa9	;; COPYRIGHT
                #xab    ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
		#xad	;; SOFT HYPHEN
		#xb0	;; DEGREE
		#xb1	;; PLUS-MINUS SIGN
		#xb6	;; PILCROW SIGN
		#xb7    ;; MIDDLE DOT 
                #xbb))  ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  (modify-syntax-entry (make-char 'latin-iso8859-16 code)
                       (string (char-syntax (make-char 'latin-iso8859-1 code)))
                       (standard-syntax-table)))

;; EURO SIGN. Take its syntax from the pound sign. 
(modify-syntax-entry (make-char 'latin-iso8859-16 #xa4)
                     (string (char-syntax (make-char 'latin-iso8859-1 #xa3)))
                     (standard-syntax-table))

;; Take DOUBLE LOW-9 QUOTATION MARK's syntax from that of LEFT-POINTING
;; DOUBLE ANGLE QUOTATION MARK.
(modify-syntax-entry (make-char 'latin-iso8859-16 #xa5) 
                     (string (char-syntax (make-char 'latin-iso8859-1 #xab)))
                     (standard-syntax-table))

;; Take RIGHT DOUBLE QUOTATION MARK's syntax from that of RIGHT-POINTING
;; DOUBLE ANGLE QUOTATION MARK.
(modify-syntax-entry (make-char 'latin-iso8859-16 #xb5)
                     (string (char-syntax (make-char 'latin-iso8859-1 #xbb)))
                     (standard-syntax-table))

;; Case. 
(setup-case-pairs
 'latin-iso8859-16
 '((#xA1 #xA2) ;; A WITH OGONEK
   (#xA3 #xB3) ;; L WITH STROKE
   (#xA6 #xA8) ;; S WITH CARON
   (#xAA #xBA) ;; S WITH COMMA BELOW
   (#xAC #xAE) ;; Z WITH ACUTE
   (#xAF #xBF) ;; Z WITH DOT ABOVE
   (#xB2 #xB9) ;; C WITH CARON
   (#xB4 #xB8) ;; Z WITH CARON
   (#xBE #xFF) ;; Y WITH DIAERESIS
   (#xC0 #xE0) ;; A WITH GRAVE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH BREVE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; C WITH ACUTE
   (#xC6 #xE6) ;; AE
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; E WITH GRAVE
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCA #xEA) ;; E WITH CIRCUMFLEX
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCC #xEC) ;; I WITH GRAVE
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xCF #xEF) ;; I WITH DIAERESIS
   (#xD0 #xF0) ;; D WITH STROKE
   (#xD1 #xF1) ;; N WITH ACUTE
   (#xD2 #xF2) ;; O WITH GRAVE
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH DOUBLE ACUTE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD7 #xF7) ;; S WITH ACUTE
   (#xD8 #xF8) ;; U WITH DOUBLE ACUTE
   (#xD9 #xF9) ;; U WITH GRAVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDD #xFD) ;; E WITH OGONEK
   (#xDE #xFE))) ;; T WITH COMMA BELOW

;; Add a coding system for ISO 8859-16.
(make-8-bit-coding-system
 'iso-8859-16
 '((#xA1 ?\u0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
   (#xA2 ?\u0105) ;; LATIN SMALL LETTER A WITH OGONEK
   (#xA3 ?\u0141) ;; LATIN CAPITAL LETTER L WITH STROKE
   (#xA4 ?\u20AC) ;; EURO SIGN
   (#xA5 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
   (#xA6 ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#xA8 ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#xAA ?\u0218) ;; LATIN CAPITAL LETTER S WITH COMMA BELOW
   (#xAC ?\u0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
   (#xAE ?\u017A) ;; LATIN SMALL LETTER Z WITH ACUTE
   (#xAF ?\u017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
   (#xB2 ?\u010C) ;; LATIN CAPITAL LETTER C WITH CARON
   (#xB3 ?\u0142) ;; LATIN SMALL LETTER L WITH STROKE
   (#xB4 ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#xB5 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
   (#xB8 ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#xB9 ?\u010D) ;; LATIN SMALL LETTER C WITH CARON
   (#xBA ?\u0219) ;; LATIN SMALL LETTER S WITH COMMA BELOW
   (#xBC ?\u0152) ;; LATIN CAPITAL LIGATURE OE
   (#xBD ?\u0153) ;; LATIN SMALL LIGATURE OE
   (#xBE ?\u0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
   (#xBF ?\u017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
   (#xC3 ?\u0102) ;; LATIN CAPITAL LETTER A WITH BREVE
   (#xC5 ?\u0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
   (#xD0 ?\u0110) ;; LATIN CAPITAL LETTER D WITH STROKE
   (#xD1 ?\u0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
   (#xD5 ?\u0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
   (#xD7 ?\u015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
   (#xD8 ?\u0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
   (#xDD ?\u0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
   (#xDE ?\u021A) ;; LATIN CAPITAL LETTER T WITH COMMA BELOW
   (#xE3 ?\u0103) ;; LATIN SMALL LETTER A WITH BREVE
   (#xE5 ?\u0107) ;; LATIN SMALL LETTER C WITH ACUTE
   (#xF0 ?\u0111) ;; LATIN SMALL LETTER D WITH STROKE
   (#xF1 ?\u0144) ;; LATIN SMALL LETTER N WITH ACUTE
   (#xF5 ?\u0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
   (#xF7 ?\u015B) ;; LATIN SMALL LETTER S WITH ACUTE
   (#xF8 ?\u0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
   (#xFD ?\u0119) ;; LATIN SMALL LETTER E WITH OGONEK
   (#xFE ?\u021B)) ;; LATIN SMALL LETTER T WITH COMMA BELOW
 "ISO-8859-16 (Latin-10)"
 '(mnemonic "Latin 10"
   aliases (iso-latin-10)))

;; end of ISO 8859-16. 


(provide 'romanian)

;; Czech support originally from czech.el
;; Author: Milan Zamazal <pdm@zamazal.org>
;; Maintainer (FSF): Pavel Jan,Am(Bk <Pavel@Janik.cz>
;; Maintainer (for XEmacs): David Sauer <davids@penguin.cz>

(provide 'czech)

;; Slovak support originally from slovak.el
;; Authors:    Tibor ,B)(Bimko <tibor.simko@fmph.uniba.sk>,
;;             Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer: Milan Zamazal <pdm@fi.muni.cz>

(provide 'slovenian)

;; Latin-5 (ISO-8859-9)

;; Turkish (more generally Turkic.) This is identical to Latin-1, with the
;; exception that the Icelandic-specific letters have been replaced by
;; Turkish-specific letters. As such, we can simply copy the Latin-1 syntax
;; table. 

(loop
  for i from #xA0 to #xFF
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'latin-iso8859-9 i)
      (string (char-syntax (make-char 'latin-iso8859-1 i)))
      syntax-table))

;; Case. The Turkish case idiosyncracy is handled with its language environment.  
(setup-case-pairs
 'latin-iso8859-9
 '((#xC0 #xE0) ;; A WITH GRAVE
   (#xC1 #xE1) ;; A WITH ACUTE
   (#xC2 #xE2) ;; A WITH CIRCUMFLEX
   (#xC3 #xE3) ;; A WITH TILDE
   (#xC4 #xE4) ;; A WITH DIAERESIS
   (#xC5 #xE5) ;; A WITH RING ABOVE
   (#xC6 #xE6) ;; AE
   (#xC7 #xE7) ;; C WITH CEDILLA
   (#xC8 #xE8) ;; E WITH GRAVE
   (#xC9 #xE9) ;; E WITH ACUTE
   (#xCB #xEB) ;; E WITH DIAERESIS
   (#xCD #xED) ;; I WITH ACUTE
   (#xCE #xEE) ;; I WITH CIRCUMFLEX
   (#xD0 #xF0) ;; G WITH BREVE
   (#xD1 #xF1) ;; N WITH TILDE
   (#xD2 #xF2) ;; O WITH GRAVE
   (#xD3 #xF3) ;; O WITH ACUTE
   (#xD4 #xF4) ;; O WITH CIRCUMFLEX
   (#xD5 #xF5) ;; O WITH TILDE
   (#xD6 #xF6) ;; O WITH DIAERESIS
   (#xD8 #xF8) ;; O WITH STROKE
   (#xD9 #xF9) ;; U WITH GRAVE
   (#xDA #xFA) ;; U WITH ACUTE
   (#xDB #xFB) ;; U WITH CIRCUMFLEX
   (#xDC #xFC) ;; U WITH DIAERESIS
   (#xDE #xFE))) ;; S WITH CEDILLA

;; LATIN CAPITAL LETTER I WITH DOT ABOVE
(put-case-table 'downcase
                (make-char 'latin-iso8859-9 #xdd)
                ?i (standard-case-table))

;; LATIN SMALL LETTER DOTLESS I
(put-case-table 'upcase
                (make-char 'latin-iso8859-9 #xfd)
                ?I (standard-case-table))

(make-8-bit-coding-system
 'iso-8859-9 
 '((#xD0 ?\u011E) ;; LATIN CAPITAL LETTER G WITH BREVE
   (#xDD ?\u0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
   (#xDE ?\u015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
   (#xF0 ?\u011F) ;; LATIN SMALL LETTER G WITH BREVE
   (#xFD ?\u0131) ;; LATIN SMALL LETTER DOTLESS I
   (#xFE ?\u015F)) ;; LATIN SMALL LETTER S WITH CEDILLA
 "ISO-8859-9 (Latin-5)"
 '(mnemonic "Latin 5"
   aliases (iso-latin-5 latin-5)))

;; end of ISO-8859-9

;; This is a utility function; we don't want it in the dumped XEmacs.

(fmakunbound 'setup-case-pairs)


;; Language environments. 
(loop 
  for ((charset codesys default-input nice-charset-1 nice-charset-2
                ;; supported-langs is a list if the doc string is replaced
                ;; entirely
                supported-langs invalid-sequence-coding-system) 
       langenvs) in
  '(((latin-iso8859-1 iso-8859-1 "latin-1-prefix" "Latin-1" "ISO-8859-1"
" Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish."
      windows-1252)
     (("Danish" "da")
      ("Dutch" "nl" "TUTORIAL.nl")
      ("Faeroese" "fo")
      ("Finnish" "fi")
      ("French" "fr" "TUTORIAL.fr" "Bonjour, ,Ag(Ba va?")
      ("German" "de" "TUTORIAL.de" "\
German (Deutsch Nord)	Guten Tag
German (Deutsch S,A|(Bd)	Gr,A|_(B Gott"
       "german-postfix")
      ("Icelandic" "is")
      ("Irish" "ga")
      ("Italian" "it")
      ("Norwegian" "no" "TUTORIAL.no")
      ("Portuguese" "pt" nil "Bem-vindo! Tudo bem?")
      ("Spanish" "es" "TUTORIAL.es" ",A!(BHola!")
      ("Swedish" "sv" "TUTORIAL.se" "Hej!")))
    ((latin-iso8859-15 iso-8859-15 "latin-1-prefix" ;; #### FIXME
		       "Latin-9" "ISO-8859-15")
     ())
    ((latin-iso8859-2 iso-8859-2 "latin-2-prefix" "Latin-2" "ISO-8859-2"
" Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbian, Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish.") ;; " added because fontification got screwed up, CVS-20061203.
     (("Albanian" "sq")
      ("Croatian" ("hrvatski" "hr") "TUTORIAL.hr")
      ("Czech" ("cs" "cz") "TUTORIAL.cs" "P,Bx(Bejeme v,Ba(Bm hezk,B}(B den!"
       "latin-2-postfix")
      ("Hungarian" ("hungarian" "hu"))
      ("Polish" ("pl" "po") "TUTORIAL.pl") ;; #### Is "po" actually used?
      ("Romanian" "ro" "TUTORIAL.ro" "Bun,Bc(B ziua, bine a,B~(Bi venit!"
       "latin-2-postfix")
      ("Serbian" "sr")
      ("Slovak" "sk" "TUTORIAL.sk" "Prajeme V,Ba(Bm pr,Bm(Bjemn,B}(B de,Br(B!"
       "latin-2-postfix")
      ("Slovenian" "sl" "TUTORIAL.sl" ",B.(Belimo vam uspe,B9(Ben dan!"
       "latin-2-postfix")
      ("Sorbian" nil)))
    ((latin-iso8859-3 iso-8859-3 "latin-3-prefix" "Latin-3" "ISO-8859-3"
" Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish.")
     (("Afrikaans" "af")
      ("Catalan" ("catalan" "ca"))
      ("Esperanto" "eo")
      ("Galician" "gl")
      ("Maltese" "mt")))
    ((latin-iso8859-4 iso-8859-4 "latin-4-prefix" "Latin-4" "ISO-8859-4"
" Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian.")
     (("Estonian" "et")
      ("Greenlandic" "kl")
      ("Lappish" "se")
      ("Latvian" "lv")
      ("Lithuanian" "li")))
    ((latin-iso8859-9 iso-8859-9 "latin-5-prefix" "Latin-5" "ISO-8859-9")
     (("Turkish" "tr"))))
  do
  (set-language-info-alist
   nice-charset-1
   `((charset ascii ,charset)
     (coding-system ,codesys)
     (coding-priority ,codesys)
     (native-coding-system ,codesys)
     (invalid-sequence-coding-system ,(or invalid-sequence-coding-system
                                          codesys))
     (documentation . ,(if (listp supported-langs) (car supported-langs)
			 (format "\
Generic language environment for %s (%s)." nice-charset-1 nice-charset-2))))
   '("European"))
  (loop for (name locale tutorial sample-text input-method) in langenvs
    do
    (set-language-info-alist
     name
     `((charset ascii ,charset)
       (coding-system ,codesys)
       (coding-priority ,codesys)
       (native-coding-system ,codesys)
       ,@(if locale `((locale . ,locale)))
       ,@(if tutorial `((tutorial . ,tutorial)
			(tutorial-coding-system . ,codesys)))
       ,@(if sample-text `((sample-text . ,sample-text)))
       (input-method . ,(or input-method default-input))
       (documentation . ,(format "\
This language environment supports %s. " name)))
     '("European"))))

;; The case table for Turkish is special:
;; #### Maybe we should limit this change to interactive functions; this may
;; well be awkward for protocols and so on. 
(set-language-info "Turkish"
                   'setup-function
                   (lambda ()
                     ;; The lowercase version of I is dotless i
                     (put-case-table-pair ?I 
                                          (make-char 'latin-iso8859-9 #xfd)
                                          (standard-case-table))
                     ;; The uppercase version of i is I with dot
                     (put-case-table-pair (make-char 'latin-iso8859-9 #xdd)
                                          ?i (standard-case-table))))

(set-language-info "Turkish"
                   'exit-function
                   (lambda ()
                     ;; Restore the normal case mappings for the characters.
                     (put-case-table-pair ?I ?i (standard-case-table))))

(make-8-bit-coding-system
 'macintosh
 '((#x80 ?\u00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
   (#x81 ?\u00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
   (#x82 ?\u00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
   (#x83 ?\u00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
   (#x84 ?\u00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
   (#x85 ?\u00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
   (#x86 ?\u00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
   (#x87 ?\u00E1) ;; LATIN SMALL LETTER A WITH ACUTE
   (#x88 ?\u00E0) ;; LATIN SMALL LETTER A WITH GRAVE
   (#x89 ?\u00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
   (#x8A ?\u00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
   (#x8B ?\u00E3) ;; LATIN SMALL LETTER A WITH TILDE
   (#x8C ?\u00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
   (#x8D ?\u00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
   (#x8E ?\u00E9) ;; LATIN SMALL LETTER E WITH ACUTE
   (#x8F ?\u00E8) ;; LATIN SMALL LETTER E WITH GRAVE
   (#x90 ?\u00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
   (#x91 ?\u00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
   (#x92 ?\u00ED) ;; LATIN SMALL LETTER I WITH ACUTE
   (#x93 ?\u00EC) ;; LATIN SMALL LETTER I WITH GRAVE
   (#x94 ?\u00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
   (#x95 ?\u00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
   (#x96 ?\u00F1) ;; LATIN SMALL LETTER N WITH TILDE
   (#x97 ?\u00F3) ;; LATIN SMALL LETTER O WITH ACUTE
   (#x98 ?\u00F2) ;; LATIN SMALL LETTER O WITH GRAVE
   (#x99 ?\u00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
   (#x9A ?\u00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
   (#x9B ?\u00F5) ;; LATIN SMALL LETTER O WITH TILDE
   (#x9C ?\u00FA) ;; LATIN SMALL LETTER U WITH ACUTE
   (#x9D ?\u00F9) ;; LATIN SMALL LETTER U WITH GRAVE
   (#x9E ?\u00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
   (#x9F ?\u00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
   (#xA0 ?\u2020) ;; DAGGER
   (#xA1 ?\u00B0) ;; DEGREE SIGN
   (#xA2 ?\u00A2) ;; CENT SIGN
   (#xA3 ?\u00A3) ;; POUND SIGN
   (#xA4 ?\u00A7) ;; SECTION SIGN
   (#xA5 ?\u2022) ;; BULLET
   (#xA6 ?\u00B6) ;; PILCROW SIGN
   (#xA7 ?\u00DF) ;; LATIN SMALL LETTER SHARP S
   (#xA8 ?\u00AE) ;; REGISTERED SIGN
   (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
   (#xAA ?\u2122) ;; TRADE MARK SIGN
   (#xAB ?\u00B4) ;; ACUTE ACCENT
   (#xAC ?\u00A8) ;; DIAERESIS
   (#xAD ?\u2260) ;; NOT EQUAL TO
   (#xAE ?\u00C6) ;; LATIN CAPITAL LETTER AE
   (#xAF ?\u00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
   (#xB0 ?\u221E) ;; INFINITY
   (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
   (#xB2 ?\u2264) ;; LESS-THAN OR EQUAL TO
   (#xB3 ?\u2265) ;; GREATER-THAN OR EQUAL TO
   (#xB4 ?\u00A5) ;; YEN SIGN
   (#xB5 ?\u00B5) ;; MICRO SIGN
   (#xB6 ?\u2202) ;; PARTIAL DIFFERENTIAL
   (#xB7 ?\u2211) ;; N-ARY SUMMATION
   (#xB8 ?\u220F) ;; N-ARY PRODUCT
   (#xB9 ?\u03C0) ;; GREEK SMALL LETTER PI
   (#xBA ?\u222B) ;; INTEGRAL
   (#xBB ?\u00AA) ;; FEMININE ORDINAL INDICATOR
   (#xBC ?\u00BA) ;; MASCULINE ORDINAL INDICATOR
   (#xBD ?\u03A9) ;; GREEK CAPITAL LETTER OMEGA
   (#xBE ?\u00E6) ;; LATIN SMALL LETTER AE
   (#xBF ?\u00F8) ;; LATIN SMALL LETTER O WITH STROKE
   (#xC0 ?\u00BF) ;; INVERTED QUESTION MARK
   (#xC1 ?\u00A1) ;; INVERTED EXCLAMATION MARK
   (#xC2 ?\u00AC) ;; NOT SIGN
   (#xC3 ?\u221A) ;; SQUARE ROOT
   (#xC4 ?\u0192) ;; LATIN SMALL LETTER F WITH HOOK
   (#xC5 ?\u2248) ;; ALMOST EQUAL TO
   (#xC6 ?\u2206) ;; INCREMENT
   (#xC7 ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xC8 ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
   (#xC9 ?\u2026) ;; HORIZONTAL ELLIPSIS
   (#xCA ?\u00A0) ;; NO-BREAK SPACE
   (#xCB ?\u00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
   (#xCC ?\u00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
   (#xCD ?\u00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
   (#xCE ?\u0152) ;; LATIN CAPITAL LIGATURE OE
   (#xCF ?\u0153) ;; LATIN SMALL LIGATURE OE
   (#xD0 ?\u2013) ;; EN DASH
   (#xD1 ?\u2014) ;; EM DASH
   (#xD2 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
   (#xD3 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
   (#xD4 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
   (#xD5 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
   (#xD6 ?\u00F7) ;; DIVISION SIGN
   (#xD7 ?\u25CA) ;; LOZENGE
   (#xD8 ?\u00FF) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   (#xD9 ?\u0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
   (#xDA ?\u2044) ;; FRACTION SLASH
   (#xDB ?\u20AC) ;; EURO SIGN
   (#xDC ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
   (#xDD ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
   (#xDE ?\uFB01) ;; LATIN SMALL LIGATURE FI
   (#xDF ?\uFB02) ;; LATIN SMALL LIGATURE FL
   (#xE0 ?\u2021) ;; DOUBLE DAGGER
   (#xE1 ?\u00B7) ;; MIDDLE DOT
   (#xE2 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
   (#xE3 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
   (#xE4 ?\u2030) ;; PER MILLE SIGN
   (#xE5 ?\u00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
   (#xE6 ?\u00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
   (#xE7 ?\u00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
   (#xE8 ?\u00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
   (#xE9 ?\u00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
   (#xEA ?\u00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
   (#xEB ?\u00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
   (#xEC ?\u00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
   (#xED ?\u00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
   (#xEE ?\u00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
   (#xEF ?\u00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
   (#xF0 ?\uF8FF) ;; Apple logo
   (#xF1 ?\u00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
   (#xF2 ?\u00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
   (#xF3 ?\u00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
   (#xF4 ?\u00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
   (#xF5 ?\u0131) ;; LATIN SMALL LETTER DOTLESS I
   (#xF6 ?\u02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
   (#xF7 ?\u02DC) ;; SMALL TILDE
   (#xF8 ?\u00AF) ;; MACRON
   (#xF9 ?\u02D8) ;; BREVE
   (#xFA ?\u02D9) ;; DOT ABOVE
   (#xFB ?\u02DA) ;; RING ABOVE
   (#xFC ?\u00B8) ;; CEDILLA
   (#xFD ?\u02DD) ;; DOUBLE ACUTE ACCENT
   (#xFE ?\u02DB) ;; OGONEK
   (#xFF ?\u02C7)) ;; CARON
 "The Macintosh encoding for Western Europe and the Americas"
 '(mnemonic "MR"
   documentation "MacRoman, MIME name macintosh"
   aliases (cp10000 MacRoman)))

(make-8-bit-coding-system
 'windows-1252
 '((#x80 ?\u20AC) ;; EURO SIGN
   (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
   (#x83 ?\u0192) ;; LATIN SMALL LETTER F WITH HOOK
   (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
   (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
   (#x86 ?\u2020) ;; DAGGER
   (#x87 ?\u2021) ;; DOUBLE DAGGER
   (#x88 ?\u02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
   (#x89 ?\u2030) ;; PER MILLE SIGN
   (#x8A ?\u0160) ;; LATIN CAPITAL LETTER S WITH CARON
   (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
   (#x8C ?\u0152) ;; LATIN CAPITAL LIGATURE OE
   (#x8E ?\u017D) ;; LATIN CAPITAL LETTER Z WITH CARON
   (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
   (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
   (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
   (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
   (#x95 ?\u2022) ;; BULLET
   (#x96 ?\u2013) ;; EN DASH
   (#x97 ?\u2014) ;; EM DASH
   (#x98 ?\u02DC) ;; SMALL TILDE
   (#x99 ?\u2122) ;; TRADE MARK SIGN
   (#x9A ?\u0161) ;; LATIN SMALL LETTER S WITH CARON
   (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
   (#x9C ?\u0153) ;; LATIN SMALL LIGATURE OE
   (#x9E ?\u017E) ;; LATIN SMALL LETTER Z WITH CARON
   (#x9F ?\u0178));; LATIN CAPITAL LETTER Y WITH DIAERESIS
 "Microsoft's extension of iso-8859-1 for Western Europe and the Americas.  "
 '(mnemonic "cp1252"
   aliases (cp1252)))

;; Provide language environments that prefer specific coding systems.
(loop
  for coding-system in '(utf-8 windows-1252 macintosh)
  with name = nil
  with assocked = nil
  do
  (setq name (create-variant-language-environment "English" coding-system)
        assocked (assoc name language-info-alist))
  (setcar assocked
          (upcase (symbol-name coding-system)))
  (setcdr assocked
          (remassq 'locale (cdr assocked))))

;;; latin.el ends here
