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

(make-coding-system
 'iso-8859-2 'iso2022 "ISO-8859-2 (Latin-2)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"))


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
 'iso-8859-3 'iso2022 "ISO-8859-3 (Latin-3)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"))


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
 'iso-8859-4 'iso2022 "ISO-8859-4 (Latin-4)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"))


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
 'iso-8859-15 'iso2022
  "ISO 4873 conforming 8-bit code (ASCII + Latin 9; aka Latin-1 with Euro)"
  '(mnemonic "MIME/Ltn-9"		; bletch
    eol-type nil
    charset-g0 ascii
    charset-g1 latin-iso8859-15
    charset-g2 t
    charset-g3 t))

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
(make-coding-system
 'iso-8859-16 'iso2022 "MIME ISO-8859-16"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-16
   charset-g2 t			; grrr
   charset-g3 t			; grrr
   mnemonic "MIME/Ltn-10"))

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

;; Case. #### Bug: this doesn't handle I WITH DOT ABOVE. 
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

(make-coding-system
 'iso-8859-9 'iso2022 "ISO-8859-9 (Latin-5)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-9
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-5"))

;; end of ISO-8859-9

;; This is a utility function; we don't want it in the dumped XEmacs.

(fmakunbound 'setup-case-pairs)


;; Language environments. 
(loop 
  for ((charset codesys default-input nice-charset-1 nice-charset-2
                ;; supported-langs is a list if the doc string is replaced
                ;; entirely
                supported-langs) 
       langenvs) in
  '(((latin-iso8859-1 iso-8859-1 "latin-1-prefix" "Latin-1" "ISO-8859-1"
" Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.")
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
      ("Polish" "po" "TUTORIAL.pl")
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
       ,@(if tutorial `((tutorial . ,tutorial)))
       ,@(if sample-text `((sample-text . ,sample-text)))
       (input-method . ,(or input-method default-input))
       (documentation . ,(format "\
This language environment supports %s. " name)))
     '("European"))))

;;; latin.el ends here
