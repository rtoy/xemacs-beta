;;; greek.el --- Support for Greek -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Greek, dumped

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

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

;; Case table:
(loop
  for (upper lower)
  in '((#xdb #xfb) ;; UPSILON WITH DIALYTIKA
       (#xda #xfa) ;; IOTA WITH DIALYTIKA
       (#xd9 #xf9) ;; OMEGA
       (#xd8 #xf8) ;; PSI
       (#xd7 #xf7) ;; CHI
       (#xd6 #xf6) ;; PHI
       (#xd5 #xf5) ;; UPSILON
       (#xd4 #xf4) ;; TAU
       (#xd3 #xf3) ;; SIGMA
       (#xd1 #xf1) ;; RHO
       (#xd0 #xf0) ;; PI
       (#xcf #xef) ;; OMICRON
       (#xce #xee) ;; XI
       (#xcd #xed) ;; NU
       (#xcc #xec) ;; MU
       (#xcb #xeb) ;; LAMDA
       (#xca #xea) ;; KAPPA
       (#xc9 #xe9) ;; IOTA
       (#xc8 #xe8) ;; THETA
       (#xc7 #xe7) ;; ETA
       (#xc6 #xe6) ;; ZETA
       (#xc5 #xe5) ;; EPSILON
       (#xc4 #xe4) ;; DELTA
       (#xc3 #xe3) ;; GAMMA
       (#xc2 #xe2) ;; BETA
       (#xc1 #xe1) ;; ALPHA
       (#xbf #xfe) ;; OMEGA WITH TONOS
       (#xbe #xfd) ;; UPSILON WITH TONOS
       (#xbc #xfc) ;; OMICRON WITH TONOS
       (#xba #xdf) ;; IOTA WITH TONOS
       (#xb9 #xde) ;; ETA WITH TONOS
       (#xb8 #xdd) ;; EPSILON WITH TONOS
       (#xb6 #xdc) ;; ALPHA WITH TONOS
       (#xD3 #xF2)) ;; FINAL SIGMA 
       ;; No case mappings for: 
       ;;
       ;; (#xE0 "GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS")
       ;; (#xC0 "GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS")
       ;;
  with case-table = (standard-case-table)
  do
  (put-case-table-pair (make-char 'greek-iso8859-7 upper)
                       (make-char 'greek-iso8859-7 lower) case-table))

;; Now, syntax. Copy from appropriate characters in Latin 1. 

;; This code requires that the guillemets not have parenthesis syntax.

(assert (not (memq (char-syntax (make-char 'latin-iso8859-1 #xAB)) '(?\( ?\))))
        t "This code assumes \xAB does not have parenthesis syntax.  ")

(assert (not (memq (char-syntax (make-char 'latin-iso8859-1 #xBB)) '(?\( ?\))))
        t "This code assumes \xBB does not have parenthesis syntax.  ")

(loop
  for (greek latin-1) 
  in '((#xA0 #xA0)  ;; NO BREAK SPACE
       (#xA1 #xAB)  ;; LEFT SINGLE QUOTATION MARK, LEFT DOUBLE ANGLE QUOTE
       (#xA2 #xBB)  ;; RIGHT SINGLE QUOTATION MARK, RIGHT DOUBLE ANGLE QUOTE
       (#xA3 #xA3)  ;; POUND SIGN
       (#xA4 #xA3)  ;; EURO SIGN, POUND SIGN
       (#xA5 #xA3)  ;; DRACHMA SIGN, POUND SIGN
       (#xA6 #xA6)  ;; BROKEN BAR
       (#xA7 #xA7)  ;; SECTION SIGN
       (#xA8 #xA8)  ;; DIAERESIS
       (#xA9 #xA9)  ;; COPYRIGHT SIGN
       (#xAA #xB4)  ;; GREEK YPOGEGRAMMENI (iota subscript), ACUTE ACCENT
       (#xAB #xAB)  ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (#xAC #xAC)  ;; NOT SIGN
       (#xAD #xAD)  ;; SOFT HYPHEN
       (#xAF #xA6)  ;; HORIZONTAL BAR, BROKEN BAR
       (#xB0 #xB0)  ;; DEGREE SIGN
       (#xB1 #xB1)  ;; PLUS-MINUS SIGN
       (#xB2 #xB2)  ;; SUPERSCRIPT TWO
       (#xB3 #xB3)  ;; SUPERSCRIPT THREE
       (#xB4 #xB4)  ;; GREEK TONOS, ACUTE ACCENT
       (#xB5 #xB4)  ;; GREEK DIALYTIKA TONOS, ACUTE ACCENT
       (#xB7 #xB7)  ;; MIDDLE DOT
       (#xBB #xBB)  ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (#xBD #xBD))  ;; VULGAR FRACTION ONE HALF
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'greek-iso8859-7 greek)
      (string (char-syntax (make-char 'latin-iso8859-1 latin-1)))
      syntax-table))

(make-coding-system
 'iso-8859-7 'fixed-width "ISO-8859-7 (Greek)"
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
    (#xA1 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#xA2 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#xA3 ?\u00A3) ;; POUND SIGN
    (#xA4 ?\u20AC) ;; EURO SIGN
    (#xA5 ?\u20AF) ;; DRACHMA SIGN
    (#xA6 ?\u00A6) ;; BROKEN BAR
    (#xA7 ?\u00A7) ;; SECTION SIGN
    (#xA8 ?\u00A8) ;; DIAERESIS
    (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
    (#xAA ?\u037A) ;; GREEK YPOGEGRAMMENI
    (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC ?\u00AC) ;; NOT SIGN
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xAF ?\u2015) ;; HORIZONTAL BAR
    (#xB0 ?\u00B0) ;; DEGREE SIGN
    (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
    (#xB2 ?\u00B2) ;; SUPERSCRIPT TWO
    (#xB3 ?\u00B3) ;; SUPERSCRIPT THREE
    (#xB4 ?\u0384) ;; GREEK TONOS
    (#xB5 ?\u0385) ;; GREEK DIALYTIKA TONOS
    (#xB6 ?\u0386) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
    (#xB7 ?\u00B7) ;; MIDDLE DOT
    (#xB8 ?\u0388) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
    (#xB9 ?\u0389) ;; GREEK CAPITAL LETTER ETA WITH TONOS
    (#xBA ?\u038A) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC ?\u038C) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
    (#xBD ?\u00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE ?\u038E) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
    (#xBF ?\u038F) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
    (#xC0 ?\u0390) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (#xC1 ?\u0391) ;; GREEK CAPITAL LETTER ALPHA
    (#xC2 ?\u0392) ;; GREEK CAPITAL LETTER BETA
    (#xC3 ?\u0393) ;; GREEK CAPITAL LETTER GAMMA
    (#xC4 ?\u0394) ;; GREEK CAPITAL LETTER DELTA
    (#xC5 ?\u0395) ;; GREEK CAPITAL LETTER EPSILON
    (#xC6 ?\u0396) ;; GREEK CAPITAL LETTER ZETA
    (#xC7 ?\u0397) ;; GREEK CAPITAL LETTER ETA
    (#xC8 ?\u0398) ;; GREEK CAPITAL LETTER THETA
    (#xC9 ?\u0399) ;; GREEK CAPITAL LETTER IOTA
    (#xCA ?\u039A) ;; GREEK CAPITAL LETTER KAPPA
    (#xCB ?\u039B) ;; GREEK CAPITAL LETTER LAMDA
    (#xCC ?\u039C) ;; GREEK CAPITAL LETTER MU
    (#xCD ?\u039D) ;; GREEK CAPITAL LETTER NU
    (#xCE ?\u039E) ;; GREEK CAPITAL LETTER XI
    (#xCF ?\u039F) ;; GREEK CAPITAL LETTER OMICRON
    (#xD0 ?\u03A0) ;; GREEK CAPITAL LETTER PI
    (#xD1 ?\u03A1) ;; GREEK CAPITAL LETTER RHO
    (#xD3 ?\u03A3) ;; GREEK CAPITAL LETTER SIGMA
    (#xD4 ?\u03A4) ;; GREEK CAPITAL LETTER TAU
    (#xD5 ?\u03A5) ;; GREEK CAPITAL LETTER UPSILON
    (#xD6 ?\u03A6) ;; GREEK CAPITAL LETTER PHI
    (#xD7 ?\u03A7) ;; GREEK CAPITAL LETTER CHI
    (#xD8 ?\u03A8) ;; GREEK CAPITAL LETTER PSI
    (#xD9 ?\u03A9) ;; GREEK CAPITAL LETTER OMEGA
    (#xDA ?\u03AA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (#xDB ?\u03AB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (#xDC ?\u03AC) ;; GREEK SMALL LETTER ALPHA WITH TONOS
    (#xDD ?\u03AD) ;; GREEK SMALL LETTER EPSILON WITH TONOS
    (#xDE ?\u03AE) ;; GREEK SMALL LETTER ETA WITH TONOS
    (#xDF ?\u03AF) ;; GREEK SMALL LETTER IOTA WITH TONOS
    (#xE0 ?\u03B0) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (#xE1 ?\u03B1) ;; GREEK SMALL LETTER ALPHA
    (#xE2 ?\u03B2) ;; GREEK SMALL LETTER BETA
    (#xE3 ?\u03B3) ;; GREEK SMALL LETTER GAMMA
    (#xE4 ?\u03B4) ;; GREEK SMALL LETTER DELTA
    (#xE5 ?\u03B5) ;; GREEK SMALL LETTER EPSILON
    (#xE6 ?\u03B6) ;; GREEK SMALL LETTER ZETA
    (#xE7 ?\u03B7) ;; GREEK SMALL LETTER ETA
    (#xE8 ?\u03B8) ;; GREEK SMALL LETTER THETA
    (#xE9 ?\u03B9) ;; GREEK SMALL LETTER IOTA
    (#xEA ?\u03BA) ;; GREEK SMALL LETTER KAPPA
    (#xEB ?\u03BB) ;; GREEK SMALL LETTER LAMDA
    (#xEC ?\u03BC) ;; GREEK SMALL LETTER MU
    (#xED ?\u03BD) ;; GREEK SMALL LETTER NU
    (#xEE ?\u03BE) ;; GREEK SMALL LETTER XI
    (#xEF ?\u03BF) ;; GREEK SMALL LETTER OMICRON
    (#xF0 ?\u03C0) ;; GREEK SMALL LETTER PI
    (#xF1 ?\u03C1) ;; GREEK SMALL LETTER RHO
    (#xF2 ?\u03C2) ;; GREEK SMALL LETTER FINAL SIGMA
    (#xF3 ?\u03C3) ;; GREEK SMALL LETTER SIGMA
    (#xF4 ?\u03C4) ;; GREEK SMALL LETTER TAU
    (#xF5 ?\u03C5) ;; GREEK SMALL LETTER UPSILON
    (#xF6 ?\u03C6) ;; GREEK SMALL LETTER PHI
    (#xF7 ?\u03C7) ;; GREEK SMALL LETTER CHI
    (#xF8 ?\u03C8) ;; GREEK SMALL LETTER PSI
    (#xF9 ?\u03C9) ;; GREEK SMALL LETTER OMEGA
    (#xFA ?\u03CA) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (#xFB ?\u03CB) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (#xFC ?\u03CC) ;; GREEK SMALL LETTER OMICRON WITH TONOS
    (#xFD ?\u03CD) ;; GREEK SMALL LETTER UPSILON WITH TONOS
    (#xFE ?\u03CE)) ;; GREEK SMALL LETTER OMEGA WITH TONOS
   mnemonic "Grk"
   aliases (greek-iso-8bit)))

(make-coding-system 
 'windows-1253 'fixed-width "Microsoft's CP1253"
 '(unicode-map
   ((#x80 ?\u20AC) ;; EURO SIGN
    (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 ?\u0192) ;; LATIN SMALL LETTER F WITH HOOK
    (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x86 ?\u2020) ;; DAGGER
    (#x87 ?\u2021) ;; DOUBLE DAGGER
    (#x89 ?\u2030) ;; PER MILLE SIGN
    (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#x99 ?\u2122) ;; TRADE MARK SIGN
    (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u0385) ;; GREEK DIALYTIKA TONOS
    (#xA2 ?\u0386) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
    (#xA3 ?\u00A3) ;; POUND SIGN
    (#xA4 ?\u00A4) ;; CURRENCY SIGN
    (#xA5 ?\u00A5) ;; YEN SIGN
    (#xA6 ?\u00A6) ;; BROKEN BAR
    (#xA7 ?\u00A7) ;; SECTION SIGN
    (#xA8 ?\u00A8) ;; DIAERESIS
    (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
    (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC ?\u00AC) ;; NOT SIGN
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xAE ?\u00AE) ;; REGISTERED SIGN
    (#xAF ?\u2015) ;; HORIZONTAL BAR
    (#xB0 ?\u00B0) ;; DEGREE SIGN
    (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
    (#xB2 ?\u00B2) ;; SUPERSCRIPT TWO
    (#xB3 ?\u00B3) ;; SUPERSCRIPT THREE
    (#xB4 ?\u0384) ;; GREEK TONOS
    (#xB5 ?\u00B5) ;; MICRO SIGN
    (#xB6 ?\u00B6) ;; PILCROW SIGN
    (#xB7 ?\u00B7) ;; MIDDLE DOT
    (#xB8 ?\u0388) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
    (#xB9 ?\u0389) ;; GREEK CAPITAL LETTER ETA WITH TONOS
    (#xBA ?\u038A) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC ?\u038C) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
    (#xBD ?\u00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE ?\u038E) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
    (#xBF ?\u038F) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
    (#xC0 ?\u0390) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (#xC1 ?\u0391) ;; GREEK CAPITAL LETTER ALPHA
    (#xC2 ?\u0392) ;; GREEK CAPITAL LETTER BETA
    (#xC3 ?\u0393) ;; GREEK CAPITAL LETTER GAMMA
    (#xC4 ?\u0394) ;; GREEK CAPITAL LETTER DELTA
    (#xC5 ?\u0395) ;; GREEK CAPITAL LETTER EPSILON
    (#xC6 ?\u0396) ;; GREEK CAPITAL LETTER ZETA
    (#xC7 ?\u0397) ;; GREEK CAPITAL LETTER ETA
    (#xC8 ?\u0398) ;; GREEK CAPITAL LETTER THETA
    (#xC9 ?\u0399) ;; GREEK CAPITAL LETTER IOTA
    (#xCA ?\u039A) ;; GREEK CAPITAL LETTER KAPPA
    (#xCB ?\u039B) ;; GREEK CAPITAL LETTER LAMDA
    (#xCC ?\u039C) ;; GREEK CAPITAL LETTER MU
    (#xCD ?\u039D) ;; GREEK CAPITAL LETTER NU
    (#xCE ?\u039E) ;; GREEK CAPITAL LETTER XI
    (#xCF ?\u039F) ;; GREEK CAPITAL LETTER OMICRON
    (#xD0 ?\u03A0) ;; GREEK CAPITAL LETTER PI
    (#xD1 ?\u03A1) ;; GREEK CAPITAL LETTER RHO
    (#xD3 ?\u03A3) ;; GREEK CAPITAL LETTER SIGMA
    (#xD4 ?\u03A4) ;; GREEK CAPITAL LETTER TAU
    (#xD5 ?\u03A5) ;; GREEK CAPITAL LETTER UPSILON
    (#xD6 ?\u03A6) ;; GREEK CAPITAL LETTER PHI
    (#xD7 ?\u03A7) ;; GREEK CAPITAL LETTER CHI
    (#xD8 ?\u03A8) ;; GREEK CAPITAL LETTER PSI
    (#xD9 ?\u03A9) ;; GREEK CAPITAL LETTER OMEGA
    (#xDA ?\u03AA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (#xDB ?\u03AB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (#xDC ?\u03AC) ;; GREEK SMALL LETTER ALPHA WITH TONOS
    (#xDD ?\u03AD) ;; GREEK SMALL LETTER EPSILON WITH TONOS
    (#xDE ?\u03AE) ;; GREEK SMALL LETTER ETA WITH TONOS
    (#xDF ?\u03AF) ;; GREEK SMALL LETTER IOTA WITH TONOS
    (#xE0 ?\u03B0) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (#xE1 ?\u03B1) ;; GREEK SMALL LETTER ALPHA
    (#xE2 ?\u03B2) ;; GREEK SMALL LETTER BETA
    (#xE3 ?\u03B3) ;; GREEK SMALL LETTER GAMMA
    (#xE4 ?\u03B4) ;; GREEK SMALL LETTER DELTA
    (#xE5 ?\u03B5) ;; GREEK SMALL LETTER EPSILON
    (#xE6 ?\u03B6) ;; GREEK SMALL LETTER ZETA
    (#xE7 ?\u03B7) ;; GREEK SMALL LETTER ETA
    (#xE8 ?\u03B8) ;; GREEK SMALL LETTER THETA
    (#xE9 ?\u03B9) ;; GREEK SMALL LETTER IOTA
    (#xEA ?\u03BA) ;; GREEK SMALL LETTER KAPPA
    (#xEB ?\u03BB) ;; GREEK SMALL LETTER LAMDA
    (#xEC ?\u03BC) ;; GREEK SMALL LETTER MU
    (#xED ?\u03BD) ;; GREEK SMALL LETTER NU
    (#xEE ?\u03BE) ;; GREEK SMALL LETTER XI
    (#xEF ?\u03BF) ;; GREEK SMALL LETTER OMICRON
    (#xF0 ?\u03C0) ;; GREEK SMALL LETTER PI
    (#xF1 ?\u03C1) ;; GREEK SMALL LETTER RHO
    (#xF2 ?\u03C2) ;; GREEK SMALL LETTER FINAL SIGMA
    (#xF3 ?\u03C3) ;; GREEK SMALL LETTER SIGMA
    (#xF4 ?\u03C4) ;; GREEK SMALL LETTER TAU
    (#xF5 ?\u03C5) ;; GREEK SMALL LETTER UPSILON
    (#xF6 ?\u03C6) ;; GREEK SMALL LETTER PHI
    (#xF7 ?\u03C7) ;; GREEK SMALL LETTER CHI
    (#xF8 ?\u03C8) ;; GREEK SMALL LETTER PSI
    (#xF9 ?\u03C9) ;; GREEK SMALL LETTER OMEGA
    (#xFA ?\u03CA) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (#xFB ?\u03CB) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (#xFC ?\u03CC) ;; GREEK SMALL LETTER OMICRON WITH TONOS
    (#xFD ?\u03CD) ;; GREEK SMALL LETTER UPSILON WITH TONOS
    (#xFE ?\u03CE)) ;; GREEK SMALL LETTER OMEGA WITH TONOS
   mnemonic "GrkW"
   documentation
   "Microsoft's Code Page 1253, for monotonic Greek.

This ASCII-compatible encoding is slightly incompatibile with
ISO-8859-7; it provides several widely-used punctuation marks in the C1
ISO-2022 area, which makes it incompatbile with the latter standard, but
that latter standard is not used in Greece,  "
   aliases (cp1253)))

(set-language-info-alist
 "Greek" '((charset greek-iso8859-7)
	   (coding-system iso-8859-7)
	   (coding-priority iso-8859-7)
	   (native-coding-system iso-8859-7)
	   (invalid-sequence-coding-system iso-8859-7)
	   (locale "el")
	   (input-method . "greek")
	   (sample-text . "Greek (,FGkk]mija(B)	,FCei\(B ,Fsar(B")
	   (documentation . t)))

;; Greek (WINDOWS-1253) will be generated automatically under Unix. 

;;; greek.el ends here
