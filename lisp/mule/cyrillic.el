;;; cyrillic.el --- Support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; Copyright (C) 2001, 2002, 2005 Ben Wing.
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
;; Copyright (C) 2001, 2002 Ben Wing.
=======
;; Copyright (C) 2001, 2002 Ben Wing.
;; Copyright (C) 2007 Free Software Foundation
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI

;; Keywords: multilingual, Cyrillic

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

;; The character sets ISO8859-5, KOI-8 and ALTERNATIVNYJ are supported.

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; Windows-1251 support in windows.el.

||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
;; Windows-1251 support deleted because XEmacs has automatic support.

=======
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI
;;; Code:

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; Cyrillic syntax
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xad) ".")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xf0) ".")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xfd) ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ISO-8859-5

||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
;; Cyrillic syntax
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ISO-8859-5

; (make-charset 'cyrillic-iso8859-5 
; 	      "Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144"
; 	      '(dimension
; 		1
; 		registry "ISO8859-5"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?L
; 		graphic 1
; 		short-name "RHP of ISO8859/5"
; 		long-name "RHP of Cyrillic (ISO 8859-5): ISO-IR-144"
; 		))

=======
;; Case table:
(loop
  for (upper lower)
  in '((#xcf #xef) ; YA
       (#xce #xee) ; YU
       (#xcd #xed) ; E
       (#xcc #xec) ; SOFT SIGN
       (#xcb #xeb) ; YERU
       (#xca #xea) ; HARD SIGN
       (#xc9 #xe9) ; SHCHA
       (#xc8 #xe8) ; SHA
       (#xc7 #xe7) ; CHE
       (#xc6 #xe6) ; TSE
       (#xc5 #xe5) ; HA
       (#xc4 #xe4) ; EF
       (#xc3 #xe3) ; U
       (#xc2 #xe2) ; TE
       (#xc1 #xe1) ; ES
       (#xc0 #xe0) ; ER
       (#xbf #xdf) ; PE
       (#xbe #xde) ; O
       (#xbd #xdd) ; EN
       (#xbc #xdc) ; EM
       (#xbb #xdb) ; EL
       (#xba #xda) ; KA
       (#xb9 #xd9) ; SHORT I
       (#xb8 #xd8) ; I
       (#xb7 #xd7) ; ZE
       (#xb6 #xd6) ; ZHE
       (#xb5 #xd5) ; IE
       (#xb4 #xd4) ; DE
       (#xb3 #xd3) ; GHE
       (#xb2 #xd2) ; VE
       (#xb1 #xd1) ; BE
       (#xb0 #xd0) ; A
       (#xaf #xff) ; DZHE
       (#xae #xfe) ; SHORT U
       (#xac #xfc) ; KJE
       (#xab #xfb) ; TSHE
       (#xaa #xfa) ; NJE
       (#xa9 #xf9) ; LJE
       (#xa8 #xf8) ; JE
       (#xa7 #xf7) ; YI
       (#xa6 #xf6) ; BYELORUSSIAN-UKRAINIAN I
       (#xa5 #xf5) ; DZE
       (#xa4 #xf4) ; UKRAINIAN IE
       (#xa3 #xf3) ; GJE
       (#xa2 #xf2) ; DJE
       (#xa1 #xf1)) ; IO
  with case-table = (standard-case-table)
  do
  (put-case-table-pair (make-char 'cyrillic-iso8859-5 upper)
                       (make-char 'cyrillic-iso8859-5 lower)
                       case-table))

;; The default character syntax is now word. Pay attention to the
;; exceptions in ISO-8859-5, copying them from ISO-8859-1. 
(loop
  for (latin-1 cyrillic) 
  in '((#xAD #xAD)  ;; SOFT HYPHEN
       (#xA7 #xFD)  ;; SECTION SIGN
       (#xA0 #xA0)) ;; NO BREAK SPACE
  with syntax-table = (standard-syntax-table)
  do (modify-syntax-entry
      (make-char 'cyrillic-iso8859-5 cyrillic)
      (string (char-syntax (make-char 'latin-iso8859-1 latin-1)))
      syntax-table))
  
;; Take NUMERO SIGN's syntax from #. 
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xF0)
                     (string (char-syntax ?\# (standard-syntax-table)))
                     (standard-syntax-table))

;; And create the coding system.
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI
(make-coding-system
 'iso-8859-5 'fixed-width "ISO-8859-5 (Cyrillic)"
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
    (#xA1 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xA2 ?\u0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#xA3 ?\u0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#xA4 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xA5 ?\u0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xA6 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xA8 ?\u0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xA9 ?\u0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#xAA ?\u040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#xAB ?\u040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#xAC ?\u040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xAE ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xAF ?\u040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#xB0 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xB1 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xB2 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xB3 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xB4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xB5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xB6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xB7 ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xB8 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xB9 ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xBA ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xBB ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xBC ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xBD ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xBE ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xBF ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xC0 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xC1 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xC2 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xC3 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xC4 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xC5 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xC6 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xC7 ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xC8 ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xC9 ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xCA ?\u042A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
    (#xCB ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xCC ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xCD ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xCE ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xCF ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xD0 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xD1 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xD2 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD3 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xD4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xD5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xD8 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xD9 ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xDA ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xDB ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xDC ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xDD ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xDE ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xDF ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xE0 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xE1 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xE2 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xE3 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xE4 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xE5 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xE6 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xE7 ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xE8 ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xE9 ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xEA ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xEB ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xEC ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xED ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xEE ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xEF ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xF0 ?\u2116) ;; NUMERO SIGN
    (#xF1 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xF2 ?\u0452) ;; CYRILLIC SMALL LETTER DJE
    (#xF3 ?\u0453) ;; CYRILLIC SMALL LETTER GJE
    (#xF4 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xF5 ?\u0455) ;; CYRILLIC SMALL LETTER DZE
    (#xF6 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xF7 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xF8 ?\u0458) ;; CYRILLIC SMALL LETTER JE
    (#xF9 ?\u0459) ;; CYRILLIC SMALL LETTER LJE
    (#xFA ?\u045A) ;; CYRILLIC SMALL LETTER NJE
    (#xFB ?\u045B) ;; CYRILLIC SMALL LETTER TSHE
    (#xFC ?\u045C) ;; CYRILLIC SMALL LETTER KJE
    (#xFD ?\u00A7) ;; SECTION SIGN
    (#xFE ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xFF ?\u045F)) ;; CYRILLIC SMALL LETTER DZHE
   mnemonic "ISO8/Cyr"
   documentation "The ISO standard for encoding Cyrillic. Not used in practice.
See `koi8-r' and `windows-1251'.  "
   aliases (cyrillic-iso-8bit)))

;; Provide this locale; but don't allow it to be picked up from the Unix
;; locale (it has no locale entry in the alist), we leave that to Russian.
(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
                  (tutorial . "TUTORIAL.ru")
                  (coding-system iso-8859-5)
                  (native-coding-system iso-8859-5)
                  (coding-priority iso-8859-5)
                  (input-method . "cyrillic-yawerty")
                  (features cyril-util)
                  (sample-text . "Russian (,L@caaZXY(B)     ,L7T`PRabRcYbU(B!")
                  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI8-R, the most common encoding for Cyrillic on Unix and Usenet.
(make-coding-system 
 'koi8-r 'fixed-width "KOI8-R (Cyrillic)"
 '(unicode-map
   ((#x80 ?\u2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 ?\u2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 ?\u250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 ?\u2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 ?\u2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 ?\u2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 ?\u251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 ?\u2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 ?\u252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 ?\u2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A ?\u253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B ?\u2580) ;; UPPER HALF BLOCK
    (#x8C ?\u2584) ;; LOWER HALF BLOCK
    (#x8D ?\u2588) ;; FULL BLOCK
    (#x8E ?\u258C) ;; LEFT HALF BLOCK
    (#x8F ?\u2590) ;; RIGHT HALF BLOCK
    (#x90 ?\u2591) ;; LIGHT SHADE
    (#x91 ?\u2592) ;; MEDIUM SHADE
    (#x92 ?\u2593) ;; DARK SHADE
    (#x93 ?\u2320) ;; TOP HALF INTEGRAL
    (#x94 ?\u25A0) ;; BLACK SQUARE
    (#x95 ?\u2219) ;; BULLET OPERATOR
    (#x96 ?\u221A) ;; SQUARE ROOT
    (#x97 ?\u2248) ;; ALMOST EQUAL TO
    (#x98 ?\u2264) ;; LESS-THAN OR EQUAL TO
    (#x99 ?\u2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A ?\u00A0) ;; NO-BREAK SPACE
    (#x9B ?\u2321) ;; BOTTOM HALF INTEGRAL
    (#x9C ?\u00B0) ;; DEGREE SIGN
    (#x9D ?\u00B2) ;; SUPERSCRIPT TWO
    (#x9E ?\u00B7) ;; MIDDLE DOT
    (#x9F ?\u00F7) ;; DIVISION SIGN
    (#xA0 ?\u2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 ?\u2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 ?\u2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u2553) ;; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    (#xA5 ?\u2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 ?\u2555) ;; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    (#xA7 ?\u2556) ;; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    (#xA8 ?\u2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 ?\u2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA ?\u2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB ?\u255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC ?\u255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD ?\u255C) ;; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    (#xAE ?\u255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xAF ?\u255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 ?\u255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 ?\u2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 ?\u2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 ?\u2562) ;; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    (#xB5 ?\u2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 ?\u2564) ;; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    (#xB7 ?\u2565) ;; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    (#xB8 ?\u2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 ?\u2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA ?\u2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB ?\u2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC ?\u256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD ?\u256B) ;; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    (#xBE ?\u256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xBF ?\u00A9) ;; COPYRIGHT SIGN
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic "KOI8"
   documentation
   "This is ,L:^T(B ,L>Q\U]P(B ,L8]d^`\PfXUY(B, 8 ,LQXb(B, the ASCII-compatible encoding
documented in RFC 1341. ,L:>8(B8-,L@(B is very common across the Cyrillic-using
internet and in Unix implementations; it shares the useful property with the
other ,L:>8(B8 encodings that when the high bit is stripped, encoded text
remains readable (Cyrillic characters get mapped to corresponding Roman
characters of the opposite case). "
   aliases (cyrillic-koi8 koi8 cp878)))

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
(make-coding-system
 'koi8-r 'mbcs
 "KOI8-R (Cyrillic)"
 '(charsets (ascii cyrillic-koi8-r)
   mnemonic "KOI8"))
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
(eval-and-compile

(defvar cyrillic-koi8-r-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?$B(!(B ?$B("(B ?$B(#(B ?$B($(B ?$B(&(B ?$B(%(B ?$B('(B ?$B()(B ?$B(((B ?$B(*(B ?$B(+(B 32  ?$(G#'(B ?$(G#+(B ?$(G#/(B 32
   32  ?$(C"F(B 32  32  ?$B"#(B 32  ?$B"e(B ?$A!V(B ?$A!\(B ?$A!](B ?,L (B  32  ?,A0(B  ?,A2(B  ?,A7(B  ?,Aw(B
   ?$(G#D(B 32  32  ?,Lq(B  32  32  32  32  32  32  32  32  32  32  32  ?$(G#E(B
   32  32  ?$(G#G(B ?,L!(B  32  32  32  32  32  32  32  32  ?$(G#F(B 32  32  ?,A)(B
   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B 
   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B 
   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B 
   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]
  "Cyrillic KOI8-R decoding table.")

(defvar cyrillic-koi8-r-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-koi8-r-decode-table i))
	     (split (split-char ch)))
	(cond ((eq (car split) 'cyrillic-iso8859-5)
	       (aset table (logior (nth 1 split) 128) i)
	       )
	      ((eq ch 32))
	      ((eq (car split) 'ascii)
	       (aset table ch i)
	       )))
      (setq i (1+ i)))
    table)
  "Cyrillic KOI8-R encoding table.")

)

(define-ccl-program ccl-decode-koi8
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-koi8-r-decode-table))))
  "CCL program to decode KOI8.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 , cyrillic-koi8-r-encode-table))))))
  "CCL program to encode KOI8.")
=======
;; Create a corresponding language environment. 
(set-language-info-alist
 "Russian" '((charset cyrillic-iso8859-5)
	     (coding-system koi8-r)
	     (native-coding-system koi8-r)
	     (coding-priority koi8-r)
	     (invalid-sequence-coding-system koi8-r)
	     (input-method . "cyrillic-yawerty")
	     (features cyril-util)
	     (locale "ru")
	     (mswindows-locale . "RUSSIAN")
	     (tutorial . "TUTORIAL.ru")
	     (sample-text . "Russian (,L@caaZXY(B)    ,L7T`PRabRcYbU(B!")
	     (documentation . "Support for Russian."))
 '("Cyrillic"))

;; Provide Cyrillic-KOI8 for old times' sake too, but don't allow it to be
;; selected by the Unix locale. A variant language environment called
;; "Cyrillic-KOI8 (UTF-8)" just looks too odd.
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI

(set-language-info-alist
 "Cyrillic-KOI8"
 (remassq 'locale (copy-list (cdr (assoc "Russian" language-info-alist))))
 '("Cyrillic"))

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; `iso-8-1' is not correct, but XEmacs doesn't have an `mbcs' category
(coding-system-put 'koi8-r 'category 'iso-8-1)

||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
(make-coding-system
 'koi8-r 'ccl
 "KOI8-R (Cyrillic)"
 '(decode ccl-decode-koi8
   encode ccl-encode-koi8
   mnemonic "KOI8"))

;; `iso-8-1' is not correct, but XEmacs doesn't have a `ccl' category
(coding-system-put 'koi8-r 'category 'iso-8-1)

;; (define-ccl-program ccl-encode-koi8-font
;;   `(0
;;     ((r1 |= 128)
;;      (r1 = r1 ,cyrillic-koi8-r-encode-table)))
;;   "CCL program to encode Cyrillic chars to KOI font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "koi8" ccl-encode-koi8-font) font-ccl-encoder-alist))

(defvar cyrillic-koi8-r-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length cyrillic-koi8-r-decode-table)))
    (while (< i len)
      (let ((ch (aref cyrillic-koi8-r-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their Koi8-R code.")

=======
;; KOI8-U, for Ukrainian.
(make-coding-system
 'koi8-u 'fixed-width "KOI8-U, Ukrainian"
 '(unicode-map
   ((#x80 ?\u2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 ?\u2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 ?\u250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 ?\u2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 ?\u2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 ?\u2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 ?\u251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 ?\u2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 ?\u252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 ?\u2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A ?\u253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B ?\u2580) ;; UPPER HALF BLOCK
    (#x8C ?\u2584) ;; LOWER HALF BLOCK
    (#x8D ?\u2588) ;; FULL BLOCK
    (#x8E ?\u258C) ;; LEFT HALF BLOCK
    (#x8F ?\u2590) ;; RIGHT HALF BLOCK
    (#x90 ?\u2591) ;; LIGHT SHADE
    (#x91 ?\u2592) ;; MEDIUM SHADE
    (#x92 ?\u2593) ;; DARK SHADE
    (#x93 ?\u2320) ;; TOP HALF INTEGRAL
    (#x94 ?\u25A0) ;; BLACK SQUARE
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u221A) ;; SQUARE ROOT
    (#x97 ?\u2248) ;; ALMOST EQUAL TO
    (#x98 ?\u2264) ;; LESS-THAN OR EQUAL TO
    (#x99 ?\u2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A ?\u00A0) ;; NO-BREAK SPACE
    (#x9B ?\u2321) ;; BOTTOM HALF INTEGRAL
    (#x9C ?\u00B0) ;; DEGREE SIGN
    (#x9D ?\u00B2) ;; SUPERSCRIPT TWO
    (#x9E ?\u00B7) ;; MIDDLE DOT
    (#x9F ?\u00F7) ;; DIVISION SIGN
    (#xA0 ?\u2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 ?\u2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 ?\u2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 ?\u2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 ?\u2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 ?\u2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA ?\u2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB ?\u255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC ?\u255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD ?\u0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE ?\u255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xAF ?\u255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 ?\u255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 ?\u2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 ?\u2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 ?\u2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 ?\u2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 ?\u2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA ?\u2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB ?\u2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC ?\u256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD ?\u0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE ?\u256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xBF ?\u00A9) ;; COPYRIGHT SIGN
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",L:>8(B8,LC(B"
   documentation
   "KOI8-U, a KOI-8-compatible encoding for Ukrainian, described in RFC2319.
This has GHE WITH UPTURN, BYELORUSSIAN-UKRAINIAN I, UKRAINIAN IE and
YI instead of some box-drawing characters.  Russian in this encoding
\(without using box-drawing characters) is bit-for-bit compatible with
Russian in KOI8-R.  "))

;; Case support, for the new characters.
(loop
  for (upper lower)
  in '((?\u0404 ?\u0454) ; UKRAINIAN IE
       (?\u0406 ?\u0456) ; BYELORUSSIAN-UKRAINIAN I
       (?\u0407 ?\u0457) ; YI
       (?\u0490 ?\u0491)); GHE WITH UPTURN
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI
(set-language-info-alist
 "Ukrainian" '((coding-system koi8-u)
               (coding-priority koi8-u)
               (locale "uk")
               (invalid-sequence-coding-system koi8-u)
               (input-method . "cyrillic-ukrainian")
               (documentation
                . "Support for Ukrainian."))
 '("Cyrillic"))

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; Windows-1251 support in windows.el.
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
;;; WINDOWS-1251 deleted; we support it automatically in XEmacs
=======
(make-coding-system 
 'windows-1251 'fixed-width "Microsoft's CP1251, Cyrillic."
 '(unicode-map
   ((#x80 ?\u0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#x81 ?\u0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 ?\u0453) ;; CYRILLIC SMALL LETTER GJE
    (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x86 ?\u2020) ;; DAGGER
    (#x87 ?\u2021) ;; DOUBLE DAGGER
    (#x88 ?\u20AC) ;; EURO SIGN
    (#x89 ?\u2030) ;; PER MILLE SIGN
    (#x8A ?\u0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C ?\u040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#x8D ?\u040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#x8E ?\u040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#x8F ?\u040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#x90 ?\u0452) ;; CYRILLIC SMALL LETTER DJE
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#x99 ?\u2122) ;; TRADE MARK SIGN
    (#x9A ?\u0459) ;; CYRILLIC SMALL LETTER LJE
    (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C ?\u045A) ;; CYRILLIC SMALL LETTER NJE
    (#x9D ?\u045C) ;; CYRILLIC SMALL LETTER KJE
    (#x9E ?\u045B) ;; CYRILLIC SMALL LETTER TSHE
    (#x9F ?\u045F) ;; CYRILLIC SMALL LETTER DZHE
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xA2 ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xA3 ?\u0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xA4 ?\u00A4) ;; CURRENCY SIGN
    (#xA5 ?\u0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xA6 ?\u00A6) ;; BROKEN BAR
    (#xA7 ?\u00A7) ;; SECTION SIGN
    (#xA8 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xA9 ?\u00A9) ;; COPYRIGHT SIGN
    (#xAA ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC ?\u00AC) ;; NOT SIGN
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xAE ?\u00AE) ;; REGISTERED SIGN
    (#xAF ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB0 ?\u00B0) ;; DEGREE SIGN
    (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
    (#xB2 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB3 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB4 ?\u0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xB5 ?\u00B5) ;; MICRO SIGN
    (#xB6 ?\u00B6) ;; PILCROW SIGN
    (#xB7 ?\u00B7) ;; MIDDLE DOT
    (#xB8 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xB9 ?\u2116) ;; NUMERO SIGN
    (#xBA ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC ?\u0458) ;; CYRILLIC SMALL LETTER JE
    (#xBD ?\u0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xBE ?\u0455) ;; CYRILLIC SMALL LETTER DZE
    (#xBF ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xC0 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xC1 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xC2 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xC3 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xC4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xC5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xC6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xC7 ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xC8 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xC9 ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xCA ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xCB ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xCC ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xCD ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xCE ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xCF ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xD0 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xD1 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xD2 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xD3 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xD4 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xD5 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xD6 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xD7 ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xD8 ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xD9 ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xDA ?\u042A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
    (#xDB ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xDC ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xDD ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xDE ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xDF ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xE0 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xE1 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xE2 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xE3 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xE4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xE5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xE6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xE7 ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xE8 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xE9 ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xEA ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xEB ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xEC ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xED ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xEE ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xEF ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xF0 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xF1 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xF2 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xF3 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xF4 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xF5 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xF6 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xF7 ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xF8 ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xF9 ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xFA ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xFB ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xFC ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xFD ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xFE ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xFF ?\u044F)) ;; CYRILLIC SMALL LETTER YA
   mnemonic "CyrW"
   documentation
   "This ASCII-compatible encoding is unfortunately not compatible at
the code point level with the KOI8 family of encodings, but it
provides several widely-used punctuation and quotation marks that
KOI-8R and its relatives don't, and has become widely used. 

It supports Russian, Bulgarian, Serbian and other languages written using
Cyrillic script.  "
   aliases (cp1251)))
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI

(set-language-info-alist
 "Bulgarian" '((coding-system windows-1251)
               (coding-priority windows-1251)
	       (invalid-sequence-coding-system windows-1251)
               (input-method . "bulgarian-bds")
               (locale "bg")
               (documentation
                . "Support for Bulgarian. ")
               (tutorial . "TUTORIAL.bg"))
 '("Cyrillic"))

(set-language-info-alist
 "Belarusian" '((coding-system windows-1251)
                (coding-priority windows-1251)
		(invalid-sequence-coding-system windows-1251)
                (locale "be")
                (input-method . "belarusian")
                (documentation
                 . "Support for Belarusian. \(The name Belarusian replaced\
Byelorussian in the early 1990s.)"))
 '("Cyrillic"))

<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
;; (define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
(eval-and-compile

(defvar cyrillic-alternativnyj-decode-table
  [
   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
   32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
   48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
   64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
   80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
   96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ?,L!(B  ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32  32 ?,Lp(B]
  "Cyrillic ALTERNATIVNYJ decoding table.")

(defvar cyrillic-alternativnyj-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-alternativnyj-decode-table i))
	     (split (split-char ch)))
	(if (eq (car split) 'cyrillic-iso8859-5)
	    (aset table (logior (nth 1 split) 128) i)
	  (if (/= ch 32)
	      (aset table ch i))))
      (setq i (1+ i)))
    table)
  "Cyrillic ALTERNATIVNYJ encoding table.")
  
)


(define-ccl-program ccl-decode-alternativnyj
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-alternativnyj-decode-table))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 ,cyrillic-alternativnyj-encode-table))))))
  "CCL program to encode Alternativnyj.")

;; (define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)
=======
;;; Alternativnyj
(make-coding-system
 'alternativnyj 'fixed-width "Microsoft's CP966, Cyrillic"
 '(unicode-map
   ((#x80 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#x81 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#x82 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#x83 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#x84 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#x85 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#x86 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#x87 ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#x88 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#x89 ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#x8A ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#x8B ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#x8C ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#x8D ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#x8E ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#x8F ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#x90 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#x91 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#x92 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#x93 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#x94 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#x95 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#x96 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#x97 ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#x98 ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#x99 ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#x9A ?\u042A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
    (#x9B ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#x9C ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#x9D ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#x9E ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#x9F ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xA0 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xA1 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xA2 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xA3 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xA4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xA5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xA6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xA7 ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xA8 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xA9 ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xAA ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xAB ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xAC ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xAD ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xAE ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xAF ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xB0 ?\u2591) ;; LIGHT SHADE
    (#xB1 ?\u2592) ;; MEDIUM SHADE
    (#xB2 ?\u2593) ;; DARK SHADE
    (#xB3 ?\u2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#xB4 ?\u2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#xB5 ?\u2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB6 ?\u2562) ;; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    (#xB7 ?\u2556) ;; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    (#xB8 ?\u2555) ;; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    (#xB9 ?\u2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xBA ?\u2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xBB ?\u2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xBC ?\u255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xBD ?\u255C) ;; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    (#xBE ?\u255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xBF ?\u2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#xC0 ?\u2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#xC1 ?\u2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#xC2 ?\u252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#xC3 ?\u251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#xC4 ?\u2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#xC5 ?\u253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#xC6 ?\u255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xC7 ?\u255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xC8 ?\u255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xC9 ?\u2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xCA ?\u2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xCB ?\u2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xCC ?\u2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xCD ?\u2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xCE ?\u256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xCF ?\u2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xD0 ?\u2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xD1 ?\u2564) ;; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    (#xD2 ?\u2565) ;; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    (#xD3 ?\u2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xD4 ?\u2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xD5 ?\u2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xD6 ?\u2553) ;; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    (#xD7 ?\u256B) ;; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    (#xD8 ?\u256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xD9 ?\u2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#xDA ?\u250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#xDB ?\u2588) ;; FULL BLOCK
    (#xDC ?\u2584) ;; LOWER HALF BLOCK
    (#xDD ?\u258C) ;; LEFT HALF BLOCK
    (#xDE ?\u2590) ;; RIGHT HALF BLOCK
    (#xDF ?\u2580) ;; UPPER HALF BLOCK
    (#xE0 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xE1 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xE2 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xE3 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xE4 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xE5 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xE6 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xE7 ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xE8 ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xE9 ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xEA ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xEB ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xEC ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xED ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xEE ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xEF ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xF0 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xF1 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xF2 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xF3 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xF4 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xF5 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xF6 ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xF7 ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xF8 ?\u00B0) ;; DEGREE SIGN
    (#xF9 ?\u2022) ;; BULLET
    (#xFA ?\u00B7) ;; MIDDLE DOT
    (#xFB ?\u221A) ;; SQUARE ROOT
    (#xFC ?\u2116) ;; NUMERO SIGN
    (#xFD ?\u00A4) ;; CURRENCY SIGN
    (#xFE ?\u25A0) ;; BLACK SQUARE
    (#xFF ?\u00A0)) ;; NO-BREAK SPACE
   mnemonic "Cy.Alt"
   aliases (cp866)))

(set-language-info-alist
 "Cyrillic-ALT" '((charset cyrillic-iso8859-5)
                  (coding-system alternativnyj)
                  (native-coding-system alternativnyj)
		  (invalid-sequence-coding-system alternativnyj)
                  (coding-priority alternativnyj)
                  (input-method . "cyrillic-yawerty")
                  (features cyril-util)
                  (tutorial . "TUTORIAL.ru")
                  (sample-text . "Russian (,L@caaZXY(B)     ,L7T`PRabRcYbU(B!")
                  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

(make-coding-system
 'koi8-ru 'fixed-width ",L:>8(B-8 (Russian, Ukrainian)"
 '(unicode-map
   ((#x80 ?\u2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 ?\u2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 ?\u250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 ?\u2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 ?\u2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 ?\u2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 ?\u251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 ?\u2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 ?\u252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 ?\u2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A ?\u253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B ?\u2580) ;; UPPER HALF BLOCK
    (#x8C ?\u2584) ;; LOWER HALF BLOCK
    (#x8D ?\u2588) ;; FULL BLOCK
    (#x8E ?\u258C) ;; LEFT HALF BLOCK
    (#x8F ?\u2590) ;; RIGHT HALF BLOCK
    (#x90 ?\u2591) ;; LIGHT SHADE
    (#x91 ?\u2592) ;; MEDIUM SHADE
    (#x92 ?\u2593) ;; DARK SHADE
    (#x93 ?\u2320) ;; TOP HALF INTEGRAL
    (#x94 ?\u25A0) ;; BLACK SQUARE
    (#x95 ?\u2219) ;; BULLET OPERATOR
    (#x96 ?\u221A) ;; SQUARE ROOT
    (#x97 ?\u2248) ;; ALMOST EQUAL TO
    (#x98 ?\u2264) ;; LESS-THAN OR EQUAL TO
    (#x99 ?\u2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A ?\u00A0) ;; NO-BREAK SPACE
    (#x9B ?\u2321) ;; BOTTOM HALF INTEGRAL
    (#x9C ?\u00B0) ;; DEGREE SIGN
    (#x9D ?\u00B2) ;; SUPERSCRIPT TWO
    (#x9E ?\u00B7) ;; MIDDLE DOT
    (#x9F ?\u00F7) ;; DIVISION SIGN
    (#xA0 ?\u2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 ?\u2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 ?\u2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 ?\u2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 ?\u2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 ?\u2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA ?\u2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB ?\u255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC ?\u255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD ?\u0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF ?\u255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 ?\u255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 ?\u2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 ?\u2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 ?\u2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 ?\u2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 ?\u2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA ?\u2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB ?\u2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC ?\u256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD ?\u0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF ?\u00A9) ;; COPYRIGHT SIGN
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",L@C(B"
   documentation "KOI8-RU, a mostly-compatible superset of KOI8-R. 
Also known as Windows code page 21866; has Ukrainian and Belarussian support. "
   aliases (cp21866)))

;; We should provide an input method and the corresponding language
;; environments for the next three coding systems. 

(make-coding-system
 'koi8-t 'fixed-width ",L:>8(B-8 for Tajik."
 '(unicode-map
   ((#x80 ?\u049B) ;; CYRILLIC SMALL LETTER KA WITH DESCENDER
    (#x81 ?\u0493) ;; CYRILLIC SMALL LETTER GHE WITH STROKE
    (#x82 ?\u201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 ?\u0492) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x86 ?\u2020) ;; DAGGER
    (#x87 ?\u2021) ;; DOUBLE DAGGER
    (#x89 ?\u2030) ;; PER MILLE SIGN
    (#x8A ?\u04B3) ;; CYRILLIC SMALL LETTER HA WITH DESCENDER
    (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C ?\u04B2) ;; CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (#x8D ?\u04B7) ;; CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (#x8E ?\u04B6) ;; CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (#x90 ?\u049A) ;; CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#x99 ?\u2122) ;; TRADE MARK SIGN
    (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#xA1 ?\u04EF) ;; CYRILLIC SMALL LETTER U WITH MACRON
    (#xA2 ?\u04EE) ;; CYRILLIC CAPITAL LETTER U WITH MACRON
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u00A4) ;; CURRENCY SIGN
    (#xA5 ?\u04E3) ;; CYRILLIC SMALL LETTER I WITH MACRON
    (#xA6 ?\u00A6) ;; BROKEN BAR
    (#xA7 ?\u00A7) ;; SECTION SIGN
    (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC ?\u00AC) ;; NOT SIGN
    (#xAD ?\u00AD) ;; SOFT HYPHEN
    (#xAE ?\u00AE) ;; REGISTERED SIGN
    (#xB0 ?\u00B0) ;; DEGREE SIGN
    (#xB1 ?\u00B1) ;; PLUS-MINUS SIGN
    (#xB2 ?\u00B2) ;; SUPERSCRIPT TWO
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB5 ?\u04E2) ;; CYRILLIC CAPITAL LETTER I WITH MACRON
    (#xB6 ?\u00B6) ;; PILCROW SIGN
    (#xB7 ?\u00B7) ;; MIDDLE DOT
    (#xB9 ?\u2116) ;; NUMERO SIGN
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBF ?\u00A9) ;; COPYRIGHT SIGN
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",LB%GҶ%@(B"
   documentation
   "Compatible in the alphabetic characters with KOI-8R for Russian,
this 8-bit Cyrillic coding system makes those characters available
that are necessary for writing Tajik, (,LWPQ^]%Gӣ%@(B ,Lb^%Gҷ%@XZ%Gӣ%@(B) the main
language of Tajikistan and a close relative of Persian.  "))

;; Case support, for the new characters.
(loop
  for (upper lower)
  in '((?\u04B6 ?\u04B7) ;; CHE WITH DESCENDER
       (?\u0492 ?\u0493) ;; GHE WITH STROKE
       (?\u04B2 ?\u04B3) ;; HA WITH DESCENDER
       (?\u04E2 ?\u04E3) ;; I WITH MACRON
       (?\u049A ?\u049B) ;; KA WITH DESCENDER
       (?\u04EE ?\u04EF)) ;; U WITH MACRON
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI

;; Support fot the languages of the Caucasus.
(make-coding-system
<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/cyrillic.el
 'alternativnyj 'mbcs
 "Alternativnyj (Cyrillic)"
 '(charsets (ascii cyrillic-alternativnyj)
   mnemonic "Cy.Alt"))

;; `iso-8-1' is not correct, but XEmacs doesn't have an `mbcs' category
(coding-system-put 'alternativnyj 'category 'iso-8-1)

(set-language-info-alist
 "Cyrillic-ALT" '((charset cyrillic-iso8859-5)
		  (coding-system alternativnyj)
		  (coding-priority alternativnyj)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (tutorial . "TUTORIAL.ru")
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~base.O_DTSO
 'alternativnyj 'ccl
 "Alternativnyj (Cyrillic)"
 '(decode ccl-decode-alternativnyj
   encode ccl-encode-alternativnyj
   mnemonic "Cy.Alt"))

;; `iso-8-1' is not correct, but XEmacs doesn't have `ccl' category
(coding-system-put 'alternativnyj 'category 'iso-8-1)

;; (define-ccl-program ccl-encode-alternativnyj-font
;;   '(0
;;     ((r1 |= 128)
;;      (r1 = r1 ,cyrillic-alternativnyj-encode-table)))
;;   "CCL program to encode Cyrillic chars to Alternativnyj font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "alternativnyj" ccl-encode-alternativnyj-font)
;;             font-ccl-encoder-alist))

(defvar cyrillic-alternativnyj-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length cyrillic-alternativnyj-decode-table)))
    (while (< i len)
      (let ((ch (aref cyrillic-alternativnyj-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their Alternativnyj code.")

(set-language-info-alist
 "Cyrillic-ALT" '((charset cyrillic-iso8859-5)
		  (coding-system alternativnyj)
		  (coding-priority alternativnyj)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (tutorial . "TUTORIAL.ru")
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))
=======
 'koi8-c 'fixed-width "KOI-8, Caucasus."
 '(unicode-map
   ((#x80 ?\u0493) ;; CYRILLIC SMALL LETTER GHE WITH STROKE
    (#x81 ?\u0497) ;; CYRILLIC SMALL LETTER ZHE WITH DESCENDER
    (#x82 ?\u049B) ;; CYRILLIC SMALL LETTER KA WITH DESCENDER
    (#x83 ?\u049D) ;; CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (#x84 ?\u04A3) ;; CYRILLIC SMALL LETTER EN WITH DESCENDER
    (#x85 ?\u04AF) ;; CYRILLIC SMALL LETTER STRAIGHT U
    (#x86 ?\u04B1) ;; CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
    (#x87 ?\u04B3) ;; CYRILLIC SMALL LETTER HA WITH DESCENDER
    (#x88 ?\u04B7) ;; CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (#x89 ?\u04B9) ;; CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
    (#x8A ?\u04BB) ;; CYRILLIC SMALL LETTER SHHA
    (#x8B ?\u2580) ;; UPPER HALF BLOCK
    (#x8C ?\u049D) ;; CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (#x8D ?\u04E3) ;; CYRILLIC SMALL LETTER I WITH MACRON
    (#x8E ?\u04E9) ;; CYRILLIC SMALL LETTER BARRED O
    (#x8F ?\u04EF) ;; CYRILLIC SMALL LETTER U WITH MACRON
    (#x90 ?\u0492) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (#x91 ?\u0496) ;; CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    (#x92 ?\u049A) ;; CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (#x93 ?\u049C) ;; CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    (#x94 ?\u04A2) ;; CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    (#x95 ?\u04AE) ;; CYRILLIC CAPITAL LETTER STRAIGHT U
    (#x96 ?\u04B0) ;; CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    (#x97 ?\u04B2) ;; CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (#x98 ?\u04B6) ;; CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (#x99 ?\u04B8) ;; CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    (#x9A ?\u04BA) ;; CYRILLIC CAPITAL LETTER SHHA
    (#x9B ?\u2321) ;; BOTTOM HALF INTEGRAL
    (#x9C ?\u04D8) ;; CYRILLIC CAPITAL LETTER SCHWA
    (#x9D ?\u04E2) ;; CYRILLIC CAPITAL LETTER I WITH MACRON
    (#x9E ?\u04E8) ;; CYRILLIC CAPITAL LETTER BARRED O
    (#x9F ?\u04EE) ;; CYRILLIC CAPITAL LETTER U WITH MACRON
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u0452) ;; CYRILLIC SMALL LETTER DJE
    (#xA2 ?\u0453) ;; CYRILLIC SMALL LETTER GJE
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 ?\u0455) ;; CYRILLIC SMALL LETTER DZE
    (#xA6 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 ?\u0458) ;; CYRILLIC SMALL LETTER JE
    (#xA9 ?\u0459) ;; CYRILLIC SMALL LETTER LJE
    (#xAA ?\u045A) ;; CYRILLIC SMALL LETTER NJE
    (#xAB ?\u045B) ;; CYRILLIC SMALL LETTER TSHE
    (#xAC ?\u045C) ;; CYRILLIC SMALL LETTER KJE
    (#xAD ?\u0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF ?\u045F) ;; CYRILLIC SMALL LETTER DZHE
    (#xB0 ?\u2116) ;; NUMERO SIGN
    (#xB1 ?\u0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#xB2 ?\u0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 ?\u0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xB6 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 ?\u0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xB9 ?\u0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#xBA ?\u040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#xBB ?\u040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#xBC ?\u040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#xBD ?\u0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF ?\u040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   documentation 
   "Note that this does not support old Russian orthography;
for that, see koi8-o. "))

(loop
  for (upper lower)
  in '((?\u04E9 ?\u04E8) ;; BARRED O
       (?\u04B9 ?\u04B8) ;; CHE WITH VERTICAL STROKE
       (?\u0452 ?\u0402) ;; DJE
       (?\u0455 ?\u0405) ;; DZE
       (?\u04A3 ?\u04A2) ;; EN WITH DESCENDER
       (?\u049D ?\u049C) ;; KA WITH VERTICAL STROKE
       (?\u04BB ?\u04BA) ;; SHHA
       (?\u04AF ?\u04AE) ;; STRAIGHT U
       (?\u04B1 ?\u04B0) ;; STRAIGHT U WITH STROKE
       (?\u0497 ?\u0496)) ;; ZHE WITH DESCENDER
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

;; Archaic Russian support.
(make-coding-system
 'koi8-o 'fixed-width "Old-orthography Russian"
 '(unicode-map
   ((#x80 ?\u0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#x81 ?\u0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#x82 ?\u00B8) ;; CEDILLA
    (#x83 ?\u0453) ;; CYRILLIC SMALL LETTER GJE
    (#x84 ?\u201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x86 ?\u2020) ;; DAGGER
    (#x87 ?\u00A7) ;; SECTION SIGN
    (#x88 ?\u20AC) ;; EURO SIGN
    (#x89 ?\u00A8) ;; DIAERESIS
    (#x8A ?\u0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#x8B ?\u2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C ?\u040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#x8D ?\u040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#x8E ?\u040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#x8F ?\u040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#x90 ?\u0452) ;; CYRILLIC SMALL LETTER DJE
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#x98 ?\u00A3) ;; POUND SIGN
    (#x99 ?\u00B7) ;; MIDDLE DOT
    (#x9A ?\u0459) ;; CYRILLIC SMALL LETTER LJE
    (#x9B ?\u203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C ?\u045A) ;; CYRILLIC SMALL LETTER NJE
    (#x9D ?\u045C) ;; CYRILLIC SMALL LETTER KJE
    (#x9E ?\u045B) ;; CYRILLIC SMALL LETTER TSHE
    (#x9F ?\u045F) ;; CYRILLIC SMALL LETTER DZHE
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u0475) ;; CYRILLIC SMALL LETTER IZHITSA
    (#xA2 ?\u0463) ;; CYRILLIC SMALL LETTER YAT
    (#xA3 ?\u0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 ?\u0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 ?\u0455) ;; CYRILLIC SMALL LETTER DZE
    (#xA6 ?\u0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 ?\u0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 ?\u0458) ;; CYRILLIC SMALL LETTER JE
    (#xA9 ?\u00AE) ;; REGISTERED SIGN
    (#xAA ?\u2122) ;; TRADE MARK SIGN
    (#xAB ?\u00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC ?\u0473) ;; CYRILLIC SMALL LETTER FITA
    (#xAD ?\u0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE ?\u045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF ?\u00B4) ;; ACUTE ACCENT
    (#xB0 ?\u00B0) ;; DEGREE SIGN
    (#xB1 ?\u0474) ;; CYRILLIC CAPITAL LETTER IZHITSA
    (#xB2 ?\u0462) ;; CYRILLIC CAPITAL LETTER YAT
    (#xB3 ?\u0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 ?\u0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 ?\u0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xB6 ?\u0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 ?\u0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 ?\u0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xB9 ?\u2116) ;; NUMERO SIGN
    (#xBA ?\u00A2) ;; CENT SIGN
    (#xBB ?\u00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC ?\u0472) ;; CYRILLIC CAPITAL LETTER FITA
    (#xBD ?\u0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE ?\u040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF ?\u00A9) ;; COPYRIGHT SIGN
    (#xC0 ?\u044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 ?\u0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 ?\u0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 ?\u0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 ?\u0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 ?\u0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 ?\u0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 ?\u0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 ?\u0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 ?\u0438) ;; CYRILLIC SMALL LETTER I
    (#xCA ?\u0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB ?\u043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC ?\u043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD ?\u043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE ?\u043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF ?\u043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 ?\u043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 ?\u044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 ?\u0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 ?\u0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 ?\u0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 ?\u0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 ?\u0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 ?\u0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 ?\u044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 ?\u044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA ?\u0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB ?\u0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC ?\u044D) ;; CYRILLIC SMALL LETTER E
    (#xDD ?\u0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE ?\u0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF ?\u044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 ?\u042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 ?\u0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 ?\u0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 ?\u0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 ?\u0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 ?\u0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 ?\u0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 ?\u0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 ?\u0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 ?\u0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA ?\u0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB ?\u041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC ?\u041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED ?\u041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE ?\u041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF ?\u041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 ?\u041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 ?\u042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 ?\u0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 ?\u0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 ?\u0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 ?\u0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 ?\u0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 ?\u0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 ?\u042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 ?\u042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA ?\u0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB ?\u0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC ?\u042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD ?\u0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE ?\u0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF ?\u042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",L:>(B"
   documentation
   "KOI-8 for old-orthography Russian; also known as KOI-C.

This is mostly compatible with KOI8-R in the alphabetic characters, and
provides ,L&v(B, %GѲѳ%@, %GѢѣ%@, and %GѴѵ%@ instead of some of the box-drawing characters.  "))

(loop
  for (upper lower)
  in '((?\u0472 ?\u0473) ;; FITA
       (?\u0474 ?\u0475) ;; IZHITSA
       (?\u0408 ?\u0458) ;; JE
       (?\u0462 ?\u0463)) ;; YAT
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

(provide 'cyrillic)
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/cyrillic.el~other.19aPPI

;;; cyrillic.el ends here
