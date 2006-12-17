;;; greek.el --- Support for Greek -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Greek

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

;; Now, syntax.
(dolist (code '(#xA1    ;; LEFT SINGLE QUOTATION MARK
                #xA2	;; RIGHT SINGLE QUOTATION MARK
                #xA3	;; POUND SIGN
                #xA6	;; BROKEN BAR
                #xA7	;; SECTION SIGN
                #xA8	;; DIAERESIS
                #xA9	;; COPYRIGHT SIGN
                #xAB	;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
                #xAC	;; NOT SIGN
                #xAD	;; SOFT HYPHEN
                #xAF	;; HORIZONTAL BAR
                #xB0	;; DEGREE SIGN
                #xB1	;; PLUS-MINUS SIGN
                #xB7	;; MIDDLE DOT
                #xBB))  ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  (modify-syntax-entry (make-char 'greek-iso8859-7 code) "."))

;; NO-BREAK SPACE
(modify-syntax-entry (make-char 'greek-iso8859-7 #xA0) " ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GREEK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
