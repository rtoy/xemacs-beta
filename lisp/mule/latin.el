;;; latin.el --- Roman-alphabet languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2010 Ben Wing.
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

(make-coding-system
 'iso-8859-2 'fixed-width "ISO-8859-2 (Latin-2)"
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
    (#xA1 #x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
    (#xA2 #x02D8) ;; BREVE
    (#xA3 #x0141) ;; LATIN CAPITAL LETTER L WITH STROKE
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x013D) ;; LATIN CAPITAL LETTER L WITH CARON
    (#xA6 #x015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#xAA #x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
    (#xAB #x0164) ;; LATIN CAPITAL LETTER T WITH CARON
    (#xAC #x0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#xAF #x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x0105) ;; LATIN SMALL LETTER A WITH OGONEK
    (#xB2 #x02DB) ;; OGONEK
    (#xB3 #x0142) ;; LATIN SMALL LETTER L WITH STROKE
    (#xB4 #x00B4) ;; ACUTE ACCENT
    (#xB5 #x013E) ;; LATIN SMALL LETTER L WITH CARON
    (#xB6 #x015B) ;; LATIN SMALL LETTER S WITH ACUTE
    (#xB7 #x02C7) ;; CARON
    (#xB8 #x00B8) ;; CEDILLA
    (#xB9 #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#xBA #x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
    (#xBB #x0165) ;; LATIN SMALL LETTER T WITH CARON
    (#xBC #x017A) ;; LATIN SMALL LETTER Z WITH ACUTE
    (#xBD #x02DD) ;; DOUBLE ACUTE ACCENT
    (#xBE #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#xBF #x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
    (#xC0 #x0154) ;; LATIN CAPITAL LETTER R WITH ACUTE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x0102) ;; LATIN CAPITAL LETTER A WITH BREVE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x0139) ;; LATIN CAPITAL LETTER L WITH ACUTE
    (#xC6 #x0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x010C) ;; LATIN CAPITAL LETTER C WITH CARON
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x011A) ;; LATIN CAPITAL LETTER E WITH CARON
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x010E) ;; LATIN CAPITAL LETTER D WITH CARON
    (#xD0 #x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
    (#xD1 #x0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
    (#xD2 #x0147) ;; LATIN CAPITAL LETTER N WITH CARON
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x0158) ;; LATIN CAPITAL LETTER R WITH CARON
    (#xD9 #x016E) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
    (#xDE #x0162) ;; LATIN CAPITAL LETTER T WITH CEDILLA
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x0155) ;; LATIN SMALL LETTER R WITH ACUTE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x0103) ;; LATIN SMALL LETTER A WITH BREVE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x013A) ;; LATIN SMALL LETTER L WITH ACUTE
    (#xE6 #x0107) ;; LATIN SMALL LETTER C WITH ACUTE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x010D) ;; LATIN SMALL LETTER C WITH CARON
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x0119) ;; LATIN SMALL LETTER E WITH OGONEK
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x011B) ;; LATIN SMALL LETTER E WITH CARON
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x010F) ;; LATIN SMALL LETTER D WITH CARON
    (#xF0 #x0111) ;; LATIN SMALL LETTER D WITH STROKE
    (#xF1 #x0144) ;; LATIN SMALL LETTER N WITH ACUTE
    (#xF2 #x0148) ;; LATIN SMALL LETTER N WITH CARON
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x0159) ;; LATIN SMALL LETTER R WITH CARON
    (#xF9 #x016F) ;; LATIN SMALL LETTER U WITH RING ABOVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
    (#xFE #x0163) ;; LATIN SMALL LETTER T WITH CEDILLA
    (#xFF #x02D9)) ;; DOT ABOVE
   documentation "ISO-8859-2 (Latin-2) for Central Europe.
See also `windows-1250', and `iso-8859-1', which is compatible with Latin 2
when used to write German (or English, of course).  "
   mnemonic "Latin 2"
   aliases (iso-latin-2 latin-2)))

(make-coding-system
 'windows-1250 'fixed-width "Microsoft's CP1250"
 '(unicode-map
   ((#x80 #x20AC) ;; EURO SIGN
    (#x82 #x201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 #x2026) ;; HORIZONTAL ELLIPSIS
    (#x86 #x2020) ;; DAGGER
    (#x87 #x2021) ;; DOUBLE DAGGER
    (#x89 #x2030) ;; PER MILLE SIGN
    (#x8A #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C #x015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
    (#x8D #x0164) ;; LATIN CAPITAL LETTER T WITH CARON
    (#x8E #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#x8F #x0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
    (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 #x2022) ;; BULLET
    (#x96 #x2013) ;; EN DASH
    (#x97 #x2014) ;; EM DASH
    (#x99 #x2122) ;; TRADE MARK SIGN
    (#x9A #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C #x015B) ;; LATIN SMALL LETTER S WITH ACUTE
    (#x9D #x0165) ;; LATIN SMALL LETTER T WITH CARON
    (#x9E #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#x9F #x017A) ;; LATIN SMALL LETTER Z WITH ACUTE
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA1 #x02C7) ;; CARON
    (#xA2 #x02D8) ;; BREVE
    (#xA3 #x0141) ;; LATIN CAPITAL LETTER L WITH STROKE
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xAF #x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x02DB) ;; OGONEK
    (#xB3 #x0142) ;; LATIN SMALL LETTER L WITH STROKE
    (#xB4 #x00B4) ;; ACUTE ACCENT
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x00B8) ;; CEDILLA
    (#xB9 #x0105) ;; LATIN SMALL LETTER A WITH OGONEK
    (#xBA #x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x013D) ;; LATIN CAPITAL LETTER L WITH CARON
    (#xBD #x02DD) ;; DOUBLE ACUTE ACCENT
    (#xBE #x013E) ;; LATIN SMALL LETTER L WITH CARON
    (#xBF #x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
    (#xC0 #x0154) ;; LATIN CAPITAL LETTER R WITH ACUTE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x0102) ;; LATIN CAPITAL LETTER A WITH BREVE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x0139) ;; LATIN CAPITAL LETTER L WITH ACUTE
    (#xC6 #x0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x010C) ;; LATIN CAPITAL LETTER C WITH CARON
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x011A) ;; LATIN CAPITAL LETTER E WITH CARON
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x010E) ;; LATIN CAPITAL LETTER D WITH CARON
    (#xD0 #x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
    (#xD1 #x0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
    (#xD2 #x0147) ;; LATIN CAPITAL LETTER N WITH CARON
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x0158) ;; LATIN CAPITAL LETTER R WITH CARON
    (#xD9 #x016E) ;; LATIN CAPITAL LETTER U WITH RING ABOVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
    (#xDE #x0162) ;; LATIN CAPITAL LETTER T WITH CEDILLA
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x0155) ;; LATIN SMALL LETTER R WITH ACUTE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x0103) ;; LATIN SMALL LETTER A WITH BREVE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x013A) ;; LATIN SMALL LETTER L WITH ACUTE
    (#xE6 #x0107) ;; LATIN SMALL LETTER C WITH ACUTE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x010D) ;; LATIN SMALL LETTER C WITH CARON
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x0119) ;; LATIN SMALL LETTER E WITH OGONEK
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x011B) ;; LATIN SMALL LETTER E WITH CARON
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x010F) ;; LATIN SMALL LETTER D WITH CARON
    (#xF0 #x0111) ;; LATIN SMALL LETTER D WITH STROKE
    (#xF1 #x0144) ;; LATIN SMALL LETTER N WITH ACUTE
    (#xF2 #x0148) ;; LATIN SMALL LETTER N WITH CARON
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x0159) ;; LATIN SMALL LETTER R WITH CARON
    (#xF9 #x016F) ;; LATIN SMALL LETTER U WITH RING ABOVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
    (#xFE #x0163) ;; LATIN SMALL LETTER T WITH CEDILLA
    (#xFF #x02D9)) ;; DOT ABOVE
   documentation
   "CP 1250, Microsoft's encoding for Central Europe. 
See also `iso-8859-2' and `window-1252' for Western Europe.  "
   mnemonic "CP1250"
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

(make-coding-system
 'iso-8859-3 'fixed-width "ISO-8859-3 (Latin-3)"
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
    (#xA1 #x0126) ;; LATIN CAPITAL LETTER H WITH STROKE
    (#xA2 #x02D8) ;; BREVE
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA6 #x0124) ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
    (#xAA #x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
    (#xAB #x011E) ;; LATIN CAPITAL LETTER G WITH BREVE
    (#xAC #x0134) ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAF #x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x0127) ;; LATIN SMALL LETTER H WITH STROKE
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x00B3) ;; SUPERSCRIPT THREE
    (#xB4 #x00B4) ;; ACUTE ACCENT
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x0125) ;; LATIN SMALL LETTER H WITH CIRCUMFLEX
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x00B8) ;; CEDILLA
    (#xB9 #x0131) ;; LATIN SMALL LETTER DOTLESS I
    (#xBA #x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
    (#xBB #x011F) ;; LATIN SMALL LETTER G WITH BREVE
    (#xBC #x0135) ;; LATIN SMALL LETTER J WITH CIRCUMFLEX
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBF #x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
    (#xC6 #x0108) ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD1 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x011C) ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x016C) ;; LATIN CAPITAL LETTER U WITH BREVE
    (#xDE #x015C) ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
    (#xE6 #x0109) ;; LATIN SMALL LETTER C WITH CIRCUMFLEX
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF1 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x011D) ;; LATIN SMALL LETTER G WITH CIRCUMFLEX
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x016D) ;; LATIN SMALL LETTER U WITH BREVE
    (#xFE #x015D) ;; LATIN SMALL LETTER S WITH CIRCUMFLEX
    (#xFF #x02D9)) ;; DOT ABOVE
   mnemonic "Latin 3"
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

(make-coding-system
 'iso-8859-4 'fixed-width "ISO-8859-4 (Latin-4)"
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
    (#xA1 #x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
    (#xA2 #x0138) ;; LATIN SMALL LETTER KRA
    (#xA3 #x0156) ;; LATIN CAPITAL LETTER R WITH CEDILLA
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x0128) ;; LATIN CAPITAL LETTER I WITH TILDE
    (#xA6 #x013B) ;; LATIN CAPITAL LETTER L WITH CEDILLA
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#xAA #x0112) ;; LATIN CAPITAL LETTER E WITH MACRON
    (#xAB #x0122) ;; LATIN CAPITAL LETTER G WITH CEDILLA
    (#xAC #x0166) ;; LATIN CAPITAL LETTER T WITH STROKE
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#xAF #x00AF) ;; MACRON
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x0105) ;; LATIN SMALL LETTER A WITH OGONEK
    (#xB2 #x02DB) ;; OGONEK
    (#xB3 #x0157) ;; LATIN SMALL LETTER R WITH CEDILLA
    (#xB4 #x00B4) ;; ACUTE ACCENT
    (#xB5 #x0129) ;; LATIN SMALL LETTER I WITH TILDE
    (#xB6 #x013C) ;; LATIN SMALL LETTER L WITH CEDILLA
    (#xB7 #x02C7) ;; CARON
    (#xB8 #x00B8) ;; CEDILLA
    (#xB9 #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#xBA #x0113) ;; LATIN SMALL LETTER E WITH MACRON
    (#xBB #x0123) ;; LATIN SMALL LETTER G WITH CEDILLA
    (#xBC #x0167) ;; LATIN SMALL LETTER T WITH STROKE
    (#xBD #x014A) ;; LATIN CAPITAL LETTER ENG
    (#xBE #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#xBF #x014B) ;; LATIN SMALL LETTER ENG
    (#xC0 #x0100) ;; LATIN CAPITAL LETTER A WITH MACRON
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x012E) ;; LATIN CAPITAL LETTER I WITH OGONEK
    (#xC8 #x010C) ;; LATIN CAPITAL LETTER C WITH CARON
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x0116) ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x012A) ;; LATIN CAPITAL LETTER I WITH MACRON
    (#xD0 #x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
    (#xD1 #x0145) ;; LATIN CAPITAL LETTER N WITH CEDILLA
    (#xD2 #x014C) ;; LATIN CAPITAL LETTER O WITH MACRON
    (#xD3 #x0136) ;; LATIN CAPITAL LETTER K WITH CEDILLA
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xD9 #x0172) ;; LATIN CAPITAL LETTER U WITH OGONEK
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x0168) ;; LATIN CAPITAL LETTER U WITH TILDE
    (#xDE #x016A) ;; LATIN CAPITAL LETTER U WITH MACRON
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x0101) ;; LATIN SMALL LETTER A WITH MACRON
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x012F) ;; LATIN SMALL LETTER I WITH OGONEK
    (#xE8 #x010D) ;; LATIN SMALL LETTER C WITH CARON
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x0119) ;; LATIN SMALL LETTER E WITH OGONEK
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x0117) ;; LATIN SMALL LETTER E WITH DOT ABOVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x012B) ;; LATIN SMALL LETTER I WITH MACRON
    (#xF0 #x0111) ;; LATIN SMALL LETTER D WITH STROKE
    (#xF1 #x0146) ;; LATIN SMALL LETTER N WITH CEDILLA
    (#xF2 #x014D) ;; LATIN SMALL LETTER O WITH MACRON
    (#xF3 #x0137) ;; LATIN SMALL LETTER K WITH CEDILLA
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xF9 #x0173) ;; LATIN SMALL LETTER U WITH OGONEK
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x0169) ;; LATIN SMALL LETTER U WITH TILDE
    (#xFE #x016B) ;; LATIN SMALL LETTER U WITH MACRON
    (#xFF #x02D9)) ;; DOT ABOVE
   mnemonic "Latin 4"
   aliases (iso-latin-4 latin-4)
   documentation "Obsolete coding system for the Baltic rim.  "))


;; Latin-8 (ISO 8859-14) Celtic.

;; Never widely used. Current-orthography Gaelic, both Irish and Scots, is
;; easily written with Latin-1. Wikipedia says the same about Welsh.

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

(make-coding-system
 'iso-8859-14 'fixed-width "ISO-8859-14 (Latin-8)"
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
    (#xA1 #x1E02) ;; LATIN CAPITAL LETTER B WITH DOT ABOVE
    (#xA2 #x1E03) ;; LATIN SMALL LETTER B WITH DOT ABOVE
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x010A) ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
    (#xA5 #x010B) ;; LATIN SMALL LETTER C WITH DOT ABOVE
    (#xA6 #x1E0A) ;; LATIN CAPITAL LETTER D WITH DOT ABOVE
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x1E80) ;; LATIN CAPITAL LETTER W WITH GRAVE
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x1E82) ;; LATIN CAPITAL LETTER W WITH ACUTE
    (#xAB #x1E0B) ;; LATIN SMALL LETTER D WITH DOT ABOVE
    (#xAC #x1EF2) ;; LATIN CAPITAL LETTER Y WITH GRAVE
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xAF #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    (#xB0 #x1E1E) ;; LATIN CAPITAL LETTER F WITH DOT ABOVE
    (#xB1 #x1E1F) ;; LATIN SMALL LETTER F WITH DOT ABOVE
    (#xB2 #x0120) ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
    (#xB3 #x0121) ;; LATIN SMALL LETTER G WITH DOT ABOVE
    (#xB4 #x1E40) ;; LATIN CAPITAL LETTER M WITH DOT ABOVE
    (#xB5 #x1E41) ;; LATIN SMALL LETTER M WITH DOT ABOVE
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x1E56) ;; LATIN CAPITAL LETTER P WITH DOT ABOVE
    (#xB8 #x1E81) ;; LATIN SMALL LETTER W WITH GRAVE
    (#xB9 #x1E57) ;; LATIN SMALL LETTER P WITH DOT ABOVE
    (#xBA #x1E83) ;; LATIN SMALL LETTER W WITH ACUTE
    (#xBB #x1E60) ;; LATIN CAPITAL LETTER S WITH DOT ABOVE
    (#xBC #x1EF3) ;; LATIN SMALL LETTER Y WITH GRAVE
    (#xBD #x1E84) ;; LATIN CAPITAL LETTER W WITH DIAERESIS
    (#xBE #x1E85) ;; LATIN SMALL LETTER W WITH DIAERESIS
    (#xBF #x1E61) ;; LATIN SMALL LETTER S WITH DOT ABOVE
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD0 #x0174) ;; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (#xD1 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x1E6A) ;; LATIN CAPITAL LETTER T WITH DOT ABOVE
    (#xD8 #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
    (#xDE #x0176) ;; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 #x0175) ;; LATIN SMALL LETTER W WITH CIRCUMFLEX
    (#xF1 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x1E6B) ;; LATIN SMALL LETTER T WITH DOT ABOVE
    (#xF8 #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
    (#xFE #x0177) ;; LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (#xFF #x00FF)) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   mnemonic "Latin 8"
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

(make-coding-system
 'iso-8859-15 'fixed-width "ISO-8859-15 (Latin-9"
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
    (#xA1 #x00A1) ;; INVERTED EXCLAMATION MARK
    (#xA2 #x00A2) ;; CENT SIGN
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x20AC) ;; EURO SIGN
    (#xA5 #x00A5) ;; YEN SIGN
    (#xA6 #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x00AA) ;; FEMININE ORDINAL INDICATOR
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xAF #x00AF) ;; MACRON
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x00B3) ;; SUPERSCRIPT THREE
    (#xB4 #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#xB9 #x00B9) ;; SUPERSCRIPT ONE
    (#xBA #x00BA) ;; MASCULINE ORDINAL INDICATOR
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x0152) ;; LATIN CAPITAL LIGATURE OE
    (#xBD #x0153) ;; LATIN SMALL LIGATURE OE
    (#xBE #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    (#xBF #x00BF) ;; INVERTED QUESTION MARK
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD0 #x00D0) ;; LATIN CAPITAL LETTER ETH
    (#xD1 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
    (#xDE #x00DE) ;; LATIN CAPITAL LETTER THORN
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 #x00F0) ;; LATIN SMALL LETTER ETH
    (#xF1 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
    (#xFE #x00FE) ;; LATIN SMALL LETTER THORN
    (#xFF #x00FF)) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   documentation "ISO 4873 conforming 8-bit code.
(ASCII + Latin 9; aka Latin-1 with Euro)"
    mnemonic "Latin 9"
    aliases (iso-latin-9 latin-9 latin-0)))

;; end of ISO 8859-15. 

;;
;; Latin-10 (ISO 8859-16).
;;
;; "South-Eastern European." Not, to my knowledge, ever widely used. 

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
(make-coding-system
 'iso-8859-16 'fixed-width "ISO-8859-16 (Latin-10)"
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
    (#xA1 #x0104) ;; LATIN CAPITAL LETTER A WITH OGONEK
    (#xA2 #x0105) ;; LATIN SMALL LETTER A WITH OGONEK
    (#xA3 #x0141) ;; LATIN CAPITAL LETTER L WITH STROKE
    (#xA4 #x20AC) ;; EURO SIGN
    (#xA5 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#xA6 #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x0218) ;; LATIN CAPITAL LETTER S WITH COMMA BELOW
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x0179) ;; LATIN CAPITAL LETTER Z WITH ACUTE
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x017A) ;; LATIN SMALL LETTER Z WITH ACUTE
    (#xAF #x017B) ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x010C) ;; LATIN CAPITAL LETTER C WITH CARON
    (#xB3 #x0142) ;; LATIN SMALL LETTER L WITH STROKE
    (#xB4 #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#xB5 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#xB9 #x010D) ;; LATIN SMALL LETTER C WITH CARON
    (#xBA #x0219) ;; LATIN SMALL LETTER S WITH COMMA BELOW
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x0152) ;; LATIN CAPITAL LIGATURE OE
    (#xBD #x0153) ;; LATIN SMALL LIGATURE OE
    (#xBE #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    (#xBF #x017C) ;; LATIN SMALL LETTER Z WITH DOT ABOVE
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x0102) ;; LATIN CAPITAL LETTER A WITH BREVE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x0106) ;; LATIN CAPITAL LETTER C WITH ACUTE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD0 #x0110) ;; LATIN CAPITAL LETTER D WITH STROKE
    (#xD1 #x0143) ;; LATIN CAPITAL LETTER N WITH ACUTE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x0150) ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x015A) ;; LATIN CAPITAL LETTER S WITH ACUTE
    (#xD8 #x0170) ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x0118) ;; LATIN CAPITAL LETTER E WITH OGONEK
    (#xDE #x021A) ;; LATIN CAPITAL LETTER T WITH COMMA BELOW
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x0103) ;; LATIN SMALL LETTER A WITH BREVE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x0107) ;; LATIN SMALL LETTER C WITH ACUTE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 #x0111) ;; LATIN SMALL LETTER D WITH STROKE
    (#xF1 #x0144) ;; LATIN SMALL LETTER N WITH ACUTE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x0151) ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x015B) ;; LATIN SMALL LETTER S WITH ACUTE
    (#xF8 #x0171) ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x0119) ;; LATIN SMALL LETTER E WITH OGONEK
    (#xFE #x021B) ;; LATIN SMALL LETTER T WITH COMMA BELOW
    (#xFF #x00FF)) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   mnemonic "Latin 10"
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

(make-coding-system
 'iso-8859-9 'fixed-width "ISO-8859-9 (Latin-5)"
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
    (#xA1 #x00A1) ;; INVERTED EXCLAMATION MARK
    (#xA2 #x00A2) ;; CENT SIGN
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x00A5) ;; YEN SIGN
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x00AA) ;; FEMININE ORDINAL INDICATOR
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
    (#xBA #x00BA) ;; MASCULINE ORDINAL INDICATOR
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x00BC) ;; VULGAR FRACTION ONE QUARTER
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE #x00BE) ;; VULGAR FRACTION THREE QUARTERS
    (#xBF #x00BF) ;; INVERTED QUESTION MARK
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD0 #x011E) ;; LATIN CAPITAL LETTER G WITH BREVE
    (#xD1 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x0130) ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
    (#xDE #x015E) ;; LATIN CAPITAL LETTER S WITH CEDILLA
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 #x011F) ;; LATIN SMALL LETTER G WITH BREVE
    (#xF1 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x0131) ;; LATIN SMALL LETTER DOTLESS I
    (#xFE #x015F) ;; LATIN SMALL LETTER S WITH CEDILLA
    (#xFF #x00FF)) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   mnemonic "Latin 5"
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
 and Swedish." windows-1250) 
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
			 (format "Generic language environment for %s (%s)."
                                 nice-charset-1 nice-charset-2))))
   '("European"))
  (loop for (name locale tutorial sample-text input-method) in langenvs
    do
    (set-language-info-alist
     name
     `((charset ascii ,charset)
       (coding-system ,codesys)
       (coding-priority ,codesys)
       (native-coding-system ,codesys)
       (invalid-sequence-coding-system ,(or invalid-sequence-coding-system
                                          codesys))
       ,@(if locale `((locale . ,locale)))
       ,@(if tutorial `((tutorial . ,tutorial)
			(tutorial-coding-system . ,codesys)))
       ,@(if sample-text `((sample-text . ,sample-text)))
       (input-method . ,(or input-method default-input))
       (documentation . ,(format "This language environment supports %s. "
                                 name)))
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

(make-coding-system
 'macintosh 'fixed-width "MacRoman"
 '(unicode-map
   ((#x80 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#x81 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#x82 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#x83 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#x84 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#x85 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#x86 #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#x87 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#x88 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#x89 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#x8A #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#x8B #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#x8C #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#x8D #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#x8E #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#x8F #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#x90 #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#x91 #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#x92 #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#x93 #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#x94 #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#x95 #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#x96 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#x97 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#x98 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#x99 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#x9A #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#x9B #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#x9C #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#x9D #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#x9E #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#x9F #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xA0 #x2020) ;; DAGGER
    (#xA1 #x00B0) ;; DEGREE SIGN
    (#xA2 #x00A2) ;; CENT SIGN
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A7) ;; SECTION SIGN
    (#xA5 #x2022) ;; BULLET
    (#xA6 #x00B6) ;; PILCROW SIGN
    (#xA7 #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xA8 #x00AE) ;; REGISTERED SIGN
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x2122) ;; TRADE MARK SIGN
    (#xAB #x00B4) ;; ACUTE ACCENT
    (#xAC #x00A8) ;; DIAERESIS
    (#xAD #x2260) ;; NOT EQUAL TO
    (#xAE #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xAF #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xB0 #x221E) ;; INFINITY
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x2264) ;; LESS-THAN OR EQUAL TO
    (#xB3 #x2265) ;; GREATER-THAN OR EQUAL TO
    (#xB4 #x00A5) ;; YEN SIGN
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x2202) ;; PARTIAL DIFFERENTIAL
    (#xB7 #x2211) ;; N-ARY SUMMATION
    (#xB8 #x220F) ;; N-ARY PRODUCT
    (#xB9 #x03C0) ;; GREEK SMALL LETTER PI
    (#xBA #x222B) ;; INTEGRAL
    (#xBB #x00AA) ;; FEMININE ORDINAL INDICATOR
    (#xBC #x00BA) ;; MASCULINE ORDINAL INDICATOR
    (#xBD #x03A9) ;; GREEK CAPITAL LETTER OMEGA
    (#xBE #x00E6) ;; LATIN SMALL LETTER AE
    (#xBF #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xC0 #x00BF) ;; INVERTED QUESTION MARK
    (#xC1 #x00A1) ;; INVERTED EXCLAMATION MARK
    (#xC2 #x00AC) ;; NOT SIGN
    (#xC3 #x221A) ;; SQUARE ROOT
    (#xC4 #x0192) ;; LATIN SMALL LETTER F WITH HOOK
    (#xC5 #x2248) ;; ALMOST EQUAL TO
    (#xC6 #x2206) ;; INCREMENT
    (#xC7 #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xC8 #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xC9 #x2026) ;; HORIZONTAL ELLIPSIS
    (#xCA #x00A0) ;; NO-BREAK SPACE
    (#xCB #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xCC #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xCD #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xCE #x0152) ;; LATIN CAPITAL LIGATURE OE
    (#xCF #x0153) ;; LATIN SMALL LIGATURE OE
    (#xD0 #x2013) ;; EN DASH
    (#xD1 #x2014) ;; EM DASH
    (#xD2 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#xD3 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#xD4 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#xD5 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#xD6 #x00F7) ;; DIVISION SIGN
    (#xD7 #x25CA) ;; LOZENGE
    (#xD8 #x00FF) ;; LATIN SMALL LETTER Y WITH DIAERESIS
    (#xD9 #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    (#xDA #x2044) ;; FRACTION SLASH
    (#xDB #x20AC) ;; EURO SIGN
    (#xDC #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#xDD #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#xDE #xFB01) ;; LATIN SMALL LIGATURE FI
    (#xDF #xFB02) ;; LATIN SMALL LIGATURE FL
    (#xE0 #x2021) ;; DOUBLE DAGGER
    (#xE1 #x00B7) ;; MIDDLE DOT
    (#xE2 #x201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#xE3 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#xE4 #x2030) ;; PER MILLE SIGN
    (#xE5 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xE6 #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xE7 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xE8 #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xE9 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xEA #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xEB #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xEC #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xED #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xEE #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xEF #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xF0 #xF8FF) ;; Apple logo
    (#xF1 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xF2 #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xF3 #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xF4 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xF5 #x0131) ;; LATIN SMALL LETTER DOTLESS I
    (#xF6 #x02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    (#xF7 #x02DC) ;; SMALL TILDE
    (#xF8 #x00AF) ;; MACRON
    (#xF9 #x02D8) ;; BREVE
    (#xFA #x02D9) ;; DOT ABOVE
    (#xFB #x02DA) ;; RING ABOVE
    (#xFC #x00B8) ;; CEDILLA
    (#xFD #x02DD) ;; DOUBLE ACUTE ACCENT
    (#xFE #x02DB) ;; OGONEK
    (#xFF #x02C7)) ;; CARON
   mnemonic "MR"
   documentation "The Macintosh encoding for Western Europe and the Americas"
   aliases (cp10000 MacRoman)))
 
(make-coding-system
 'windows-1252 'fixed-width "Microsoft's CP1252"
 '(unicode-map
   ((#x80 #x20AC) ;; EURO SIGN
    (#x82 #x201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 #x0192) ;; LATIN SMALL LETTER F WITH HOOK
    (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 #x2026) ;; HORIZONTAL ELLIPSIS
    (#x86 #x2020) ;; DAGGER
    (#x87 #x2021) ;; DOUBLE DAGGER
    (#x88 #x02C6) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    (#x89 #x2030) ;; PER MILLE SIGN
    (#x8A #x0160) ;; LATIN CAPITAL LETTER S WITH CARON
    (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C #x0152) ;; LATIN CAPITAL LIGATURE OE
    (#x8E #x017D) ;; LATIN CAPITAL LETTER Z WITH CARON
    (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 #x2022) ;; BULLET
    (#x96 #x2013) ;; EN DASH
    (#x97 #x2014) ;; EM DASH
    (#x98 #x02DC) ;; SMALL TILDE
    (#x99 #x2122) ;; TRADE MARK SIGN
    (#x9A #x0161) ;; LATIN SMALL LETTER S WITH CARON
    (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C #x0153) ;; LATIN SMALL LIGATURE OE
    (#x9E #x017E) ;; LATIN SMALL LETTER Z WITH CARON
    (#x9F #x0178) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA1 #x00A1) ;; INVERTED EXCLAMATION MARK
    (#xA2 #x00A2) ;; CENT SIGN
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x00A5) ;; YEN SIGN
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x00AA) ;; FEMININE ORDINAL INDICATOR
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
    (#xBA #x00BA) ;; MASCULINE ORDINAL INDICATOR
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x00BC) ;; VULGAR FRACTION ONE QUARTER
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE #x00BE) ;; VULGAR FRACTION THREE QUARTERS
    (#xBF #x00BF) ;; INVERTED QUESTION MARK
    (#xC0 #x00C0) ;; LATIN CAPITAL LETTER A WITH GRAVE
    (#xC1 #x00C1) ;; LATIN CAPITAL LETTER A WITH ACUTE
    (#xC2 #x00C2) ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (#xC3 #x00C3) ;; LATIN CAPITAL LETTER A WITH TILDE
    (#xC4 #x00C4) ;; LATIN CAPITAL LETTER A WITH DIAERESIS
    (#xC5 #x00C5) ;; LATIN CAPITAL LETTER A WITH RING ABOVE
    (#xC6 #x00C6) ;; LATIN CAPITAL LETTER AE
    (#xC7 #x00C7) ;; LATIN CAPITAL LETTER C WITH CEDILLA
    (#xC8 #x00C8) ;; LATIN CAPITAL LETTER E WITH GRAVE
    (#xC9 #x00C9) ;; LATIN CAPITAL LETTER E WITH ACUTE
    (#xCA #x00CA) ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (#xCB #x00CB) ;; LATIN CAPITAL LETTER E WITH DIAERESIS
    (#xCC #x00CC) ;; LATIN CAPITAL LETTER I WITH GRAVE
    (#xCD #x00CD) ;; LATIN CAPITAL LETTER I WITH ACUTE
    (#xCE #x00CE) ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (#xCF #x00CF) ;; LATIN CAPITAL LETTER I WITH DIAERESIS
    (#xD0 #x00D0) ;; LATIN CAPITAL LETTER ETH
    (#xD1 #x00D1) ;; LATIN CAPITAL LETTER N WITH TILDE
    (#xD2 #x00D2) ;; LATIN CAPITAL LETTER O WITH GRAVE
    (#xD3 #x00D3) ;; LATIN CAPITAL LETTER O WITH ACUTE
    (#xD4 #x00D4) ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (#xD5 #x00D5) ;; LATIN CAPITAL LETTER O WITH TILDE
    (#xD6 #x00D6) ;; LATIN CAPITAL LETTER O WITH DIAERESIS
    (#xD7 #x00D7) ;; MULTIPLICATION SIGN
    (#xD8 #x00D8) ;; LATIN CAPITAL LETTER O WITH STROKE
    (#xD9 #x00D9) ;; LATIN CAPITAL LETTER U WITH GRAVE
    (#xDA #x00DA) ;; LATIN CAPITAL LETTER U WITH ACUTE
    (#xDB #x00DB) ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (#xDC #x00DC) ;; LATIN CAPITAL LETTER U WITH DIAERESIS
    (#xDD #x00DD) ;; LATIN CAPITAL LETTER Y WITH ACUTE
    (#xDE #x00DE) ;; LATIN CAPITAL LETTER THORN
    (#xDF #x00DF) ;; LATIN SMALL LETTER SHARP S
    (#xE0 #x00E0) ;; LATIN SMALL LETTER A WITH GRAVE
    (#xE1 #x00E1) ;; LATIN SMALL LETTER A WITH ACUTE
    (#xE2 #x00E2) ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
    (#xE3 #x00E3) ;; LATIN SMALL LETTER A WITH TILDE
    (#xE4 #x00E4) ;; LATIN SMALL LETTER A WITH DIAERESIS
    (#xE5 #x00E5) ;; LATIN SMALL LETTER A WITH RING ABOVE
    (#xE6 #x00E6) ;; LATIN SMALL LETTER AE
    (#xE7 #x00E7) ;; LATIN SMALL LETTER C WITH CEDILLA
    (#xE8 #x00E8) ;; LATIN SMALL LETTER E WITH GRAVE
    (#xE9 #x00E9) ;; LATIN SMALL LETTER E WITH ACUTE
    (#xEA #x00EA) ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
    (#xEB #x00EB) ;; LATIN SMALL LETTER E WITH DIAERESIS
    (#xEC #x00EC) ;; LATIN SMALL LETTER I WITH GRAVE
    (#xED #x00ED) ;; LATIN SMALL LETTER I WITH ACUTE
    (#xEE #x00EE) ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
    (#xEF #x00EF) ;; LATIN SMALL LETTER I WITH DIAERESIS
    (#xF0 #x00F0) ;; LATIN SMALL LETTER ETH
    (#xF1 #x00F1) ;; LATIN SMALL LETTER N WITH TILDE
    (#xF2 #x00F2) ;; LATIN SMALL LETTER O WITH GRAVE
    (#xF3 #x00F3) ;; LATIN SMALL LETTER O WITH ACUTE
    (#xF4 #x00F4) ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
    (#xF5 #x00F5) ;; LATIN SMALL LETTER O WITH TILDE
    (#xF6 #x00F6) ;; LATIN SMALL LETTER O WITH DIAERESIS
    (#xF7 #x00F7) ;; DIVISION SIGN
    (#xF8 #x00F8) ;; LATIN SMALL LETTER O WITH STROKE
    (#xF9 #x00F9) ;; LATIN SMALL LETTER U WITH GRAVE
    (#xFA #x00FA) ;; LATIN SMALL LETTER U WITH ACUTE
    (#xFB #x00FB) ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
    (#xFC #x00FC) ;; LATIN SMALL LETTER U WITH DIAERESIS
    (#xFD #x00FD) ;; LATIN SMALL LETTER Y WITH ACUTE
    (#xFE #x00FE) ;; LATIN SMALL LETTER THORN
    (#xFF #x00FF)) ;; LATIN SMALL LETTER Y WITH DIAERESIS
   documentation "Microsoft's extension of iso-8859-1 for Western Europe \
and the Americas.  "
   mnemonic "cp1252"
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
