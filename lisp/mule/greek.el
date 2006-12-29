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
 'iso-8859-7 'iso2022 "ISO-8859-7 (Greek)"
 '(charset-g0 ascii
   charset-g1 greek-iso8859-7
   charset-g2 t
   charset-g3 t
   mnemonic "Grk"))

(set-language-info-alist
 "Greek" '((charset greek-iso8859-7)
	   (coding-system iso-8859-7)
	   (coding-priority iso-8859-7)
	   (locale "el_GR.iso88597" "el_GR.greek8" "el_GR" "greek" "el")
	   (input-method . "greek")
	   (sample-text . "Greek (,FGkk]mija(B)	,FCei\(B ,Fsar(B")
	   (documentation . t)))

;;; greek.el ends here
