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
    (#xA1 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#xA2 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x20AC) ;; EURO SIGN
    (#xA5 #x20AF) ;; DRACHMA SIGN
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAA #x037A) ;; GREEK YPOGEGRAMMENI
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAF #x2015) ;; HORIZONTAL BAR
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x00B3) ;; SUPERSCRIPT THREE
    (#xB4 #x0384) ;; GREEK TONOS
    (#xB5 #x0385) ;; GREEK DIALYTIKA TONOS
    (#xB6 #x0386) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x0388) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
    (#xB9 #x0389) ;; GREEK CAPITAL LETTER ETA WITH TONOS
    (#xBA #x038A) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x038C) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE #x038E) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
    (#xBF #x038F) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
    (#xC0 #x0390) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (#xC1 #x0391) ;; GREEK CAPITAL LETTER ALPHA
    (#xC2 #x0392) ;; GREEK CAPITAL LETTER BETA
    (#xC3 #x0393) ;; GREEK CAPITAL LETTER GAMMA
    (#xC4 #x0394) ;; GREEK CAPITAL LETTER DELTA
    (#xC5 #x0395) ;; GREEK CAPITAL LETTER EPSILON
    (#xC6 #x0396) ;; GREEK CAPITAL LETTER ZETA
    (#xC7 #x0397) ;; GREEK CAPITAL LETTER ETA
    (#xC8 #x0398) ;; GREEK CAPITAL LETTER THETA
    (#xC9 #x0399) ;; GREEK CAPITAL LETTER IOTA
    (#xCA #x039A) ;; GREEK CAPITAL LETTER KAPPA
    (#xCB #x039B) ;; GREEK CAPITAL LETTER LAMDA
    (#xCC #x039C) ;; GREEK CAPITAL LETTER MU
    (#xCD #x039D) ;; GREEK CAPITAL LETTER NU
    (#xCE #x039E) ;; GREEK CAPITAL LETTER XI
    (#xCF #x039F) ;; GREEK CAPITAL LETTER OMICRON
    (#xD0 #x03A0) ;; GREEK CAPITAL LETTER PI
    (#xD1 #x03A1) ;; GREEK CAPITAL LETTER RHO
    (#xD3 #x03A3) ;; GREEK CAPITAL LETTER SIGMA
    (#xD4 #x03A4) ;; GREEK CAPITAL LETTER TAU
    (#xD5 #x03A5) ;; GREEK CAPITAL LETTER UPSILON
    (#xD6 #x03A6) ;; GREEK CAPITAL LETTER PHI
    (#xD7 #x03A7) ;; GREEK CAPITAL LETTER CHI
    (#xD8 #x03A8) ;; GREEK CAPITAL LETTER PSI
    (#xD9 #x03A9) ;; GREEK CAPITAL LETTER OMEGA
    (#xDA #x03AA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (#xDB #x03AB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (#xDC #x03AC) ;; GREEK SMALL LETTER ALPHA WITH TONOS
    (#xDD #x03AD) ;; GREEK SMALL LETTER EPSILON WITH TONOS
    (#xDE #x03AE) ;; GREEK SMALL LETTER ETA WITH TONOS
    (#xDF #x03AF) ;; GREEK SMALL LETTER IOTA WITH TONOS
    (#xE0 #x03B0) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (#xE1 #x03B1) ;; GREEK SMALL LETTER ALPHA
    (#xE2 #x03B2) ;; GREEK SMALL LETTER BETA
    (#xE3 #x03B3) ;; GREEK SMALL LETTER GAMMA
    (#xE4 #x03B4) ;; GREEK SMALL LETTER DELTA
    (#xE5 #x03B5) ;; GREEK SMALL LETTER EPSILON
    (#xE6 #x03B6) ;; GREEK SMALL LETTER ZETA
    (#xE7 #x03B7) ;; GREEK SMALL LETTER ETA
    (#xE8 #x03B8) ;; GREEK SMALL LETTER THETA
    (#xE9 #x03B9) ;; GREEK SMALL LETTER IOTA
    (#xEA #x03BA) ;; GREEK SMALL LETTER KAPPA
    (#xEB #x03BB) ;; GREEK SMALL LETTER LAMDA
    (#xEC #x03BC) ;; GREEK SMALL LETTER MU
    (#xED #x03BD) ;; GREEK SMALL LETTER NU
    (#xEE #x03BE) ;; GREEK SMALL LETTER XI
    (#xEF #x03BF) ;; GREEK SMALL LETTER OMICRON
    (#xF0 #x03C0) ;; GREEK SMALL LETTER PI
    (#xF1 #x03C1) ;; GREEK SMALL LETTER RHO
    (#xF2 #x03C2) ;; GREEK SMALL LETTER FINAL SIGMA
    (#xF3 #x03C3) ;; GREEK SMALL LETTER SIGMA
    (#xF4 #x03C4) ;; GREEK SMALL LETTER TAU
    (#xF5 #x03C5) ;; GREEK SMALL LETTER UPSILON
    (#xF6 #x03C6) ;; GREEK SMALL LETTER PHI
    (#xF7 #x03C7) ;; GREEK SMALL LETTER CHI
    (#xF8 #x03C8) ;; GREEK SMALL LETTER PSI
    (#xF9 #x03C9) ;; GREEK SMALL LETTER OMEGA
    (#xFA #x03CA) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (#xFB #x03CB) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (#xFC #x03CC) ;; GREEK SMALL LETTER OMICRON WITH TONOS
    (#xFD #x03CD) ;; GREEK SMALL LETTER UPSILON WITH TONOS
    (#xFE #x03CE)) ;; GREEK SMALL LETTER OMEGA WITH TONOS
   mnemonic "Grk"
   aliases (greek-iso-8bit)))

(make-coding-system 
 'windows-1253 'fixed-width "Microsoft's CP1253"
 '(unicode-map
   ((#x80 #x20AC) ;; EURO SIGN
    (#x82 #x201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 #x0192) ;; LATIN SMALL LETTER F WITH HOOK
    (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 #x2026) ;; HORIZONTAL ELLIPSIS
    (#x86 #x2020) ;; DAGGER
    (#x87 #x2021) ;; DOUBLE DAGGER
    (#x89 #x2030) ;; PER MILLE SIGN
    (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 #x2022) ;; BULLET
    (#x96 #x2013) ;; EN DASH
    (#x97 #x2014) ;; EM DASH
    (#x99 #x2122) ;; TRADE MARK SIGN
    (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA1 #x0385) ;; GREEK DIALYTIKA TONOS
    (#xA2 #x0386) ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
    (#xA3 #x00A3) ;; POUND SIGN
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x00A5) ;; YEN SIGN
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xA8 #x00A8) ;; DIAERESIS
    (#xA9 #x00A9) ;; COPYRIGHT SIGN
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xAF #x2015) ;; HORIZONTAL BAR
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x00B3) ;; SUPERSCRIPT THREE
    (#xB4 #x0384) ;; GREEK TONOS
    (#xB5 #x00B5) ;; MICRO SIGN
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB8 #x0388) ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
    (#xB9 #x0389) ;; GREEK CAPITAL LETTER ETA WITH TONOS
    (#xBA #x038A) ;; GREEK CAPITAL LETTER IOTA WITH TONOS
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x038C) ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
    (#xBD #x00BD) ;; VULGAR FRACTION ONE HALF
    (#xBE #x038E) ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
    (#xBF #x038F) ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
    (#xC0 #x0390) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (#xC1 #x0391) ;; GREEK CAPITAL LETTER ALPHA
    (#xC2 #x0392) ;; GREEK CAPITAL LETTER BETA
    (#xC3 #x0393) ;; GREEK CAPITAL LETTER GAMMA
    (#xC4 #x0394) ;; GREEK CAPITAL LETTER DELTA
    (#xC5 #x0395) ;; GREEK CAPITAL LETTER EPSILON
    (#xC6 #x0396) ;; GREEK CAPITAL LETTER ZETA
    (#xC7 #x0397) ;; GREEK CAPITAL LETTER ETA
    (#xC8 #x0398) ;; GREEK CAPITAL LETTER THETA
    (#xC9 #x0399) ;; GREEK CAPITAL LETTER IOTA
    (#xCA #x039A) ;; GREEK CAPITAL LETTER KAPPA
    (#xCB #x039B) ;; GREEK CAPITAL LETTER LAMDA
    (#xCC #x039C) ;; GREEK CAPITAL LETTER MU
    (#xCD #x039D) ;; GREEK CAPITAL LETTER NU
    (#xCE #x039E) ;; GREEK CAPITAL LETTER XI
    (#xCF #x039F) ;; GREEK CAPITAL LETTER OMICRON
    (#xD0 #x03A0) ;; GREEK CAPITAL LETTER PI
    (#xD1 #x03A1) ;; GREEK CAPITAL LETTER RHO
    (#xD3 #x03A3) ;; GREEK CAPITAL LETTER SIGMA
    (#xD4 #x03A4) ;; GREEK CAPITAL LETTER TAU
    (#xD5 #x03A5) ;; GREEK CAPITAL LETTER UPSILON
    (#xD6 #x03A6) ;; GREEK CAPITAL LETTER PHI
    (#xD7 #x03A7) ;; GREEK CAPITAL LETTER CHI
    (#xD8 #x03A8) ;; GREEK CAPITAL LETTER PSI
    (#xD9 #x03A9) ;; GREEK CAPITAL LETTER OMEGA
    (#xDA #x03AA) ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (#xDB #x03AB) ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (#xDC #x03AC) ;; GREEK SMALL LETTER ALPHA WITH TONOS
    (#xDD #x03AD) ;; GREEK SMALL LETTER EPSILON WITH TONOS
    (#xDE #x03AE) ;; GREEK SMALL LETTER ETA WITH TONOS
    (#xDF #x03AF) ;; GREEK SMALL LETTER IOTA WITH TONOS
    (#xE0 #x03B0) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (#xE1 #x03B1) ;; GREEK SMALL LETTER ALPHA
    (#xE2 #x03B2) ;; GREEK SMALL LETTER BETA
    (#xE3 #x03B3) ;; GREEK SMALL LETTER GAMMA
    (#xE4 #x03B4) ;; GREEK SMALL LETTER DELTA
    (#xE5 #x03B5) ;; GREEK SMALL LETTER EPSILON
    (#xE6 #x03B6) ;; GREEK SMALL LETTER ZETA
    (#xE7 #x03B7) ;; GREEK SMALL LETTER ETA
    (#xE8 #x03B8) ;; GREEK SMALL LETTER THETA
    (#xE9 #x03B9) ;; GREEK SMALL LETTER IOTA
    (#xEA #x03BA) ;; GREEK SMALL LETTER KAPPA
    (#xEB #x03BB) ;; GREEK SMALL LETTER LAMDA
    (#xEC #x03BC) ;; GREEK SMALL LETTER MU
    (#xED #x03BD) ;; GREEK SMALL LETTER NU
    (#xEE #x03BE) ;; GREEK SMALL LETTER XI
    (#xEF #x03BF) ;; GREEK SMALL LETTER OMICRON
    (#xF0 #x03C0) ;; GREEK SMALL LETTER PI
    (#xF1 #x03C1) ;; GREEK SMALL LETTER RHO
    (#xF2 #x03C2) ;; GREEK SMALL LETTER FINAL SIGMA
    (#xF3 #x03C3) ;; GREEK SMALL LETTER SIGMA
    (#xF4 #x03C4) ;; GREEK SMALL LETTER TAU
    (#xF5 #x03C5) ;; GREEK SMALL LETTER UPSILON
    (#xF6 #x03C6) ;; GREEK SMALL LETTER PHI
    (#xF7 #x03C7) ;; GREEK SMALL LETTER CHI
    (#xF8 #x03C8) ;; GREEK SMALL LETTER PSI
    (#xF9 #x03C9) ;; GREEK SMALL LETTER OMEGA
    (#xFA #x03CA) ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (#xFB #x03CB) ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (#xFC #x03CC) ;; GREEK SMALL LETTER OMICRON WITH TONOS
    (#xFD #x03CD) ;; GREEK SMALL LETTER UPSILON WITH TONOS
    (#xFE #x03CE)) ;; GREEK SMALL LETTER OMEGA WITH TONOS
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
