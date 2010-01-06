;;; cyrillic.el --- Support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2002, 2005 Ben Wing.
;; Copyright (C) 2007 Free Software Foundation

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

;; Windows-1251 support in windows.el.

;;; Code:

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
(make-coding-system
 'iso-8859-5 'mbcs "ISO-8859-5 (Cyrillic)"
 '(charsets (ascii control-1 cyrillic-iso8859-5)
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

(make-charset 'cyrillic-koi8-r
	      "Cyrillic KOI8-R"
	      '(dimension
		1
		chars 128
		short-name "Cyrillic KOI8-R"
		long-name "Cyrillic KOI8-R"
		))

(make-charset 'cyrillic-alternativnyj
	      "Cyrillic Alternativnyj"
	      '(dimension
		1
		chars 128
		short-name "Cyrillic Alternativnyj"
		long-name "Cyrillic Alternativnyj"
		))

;; KOI8-R, the most common encoding for Cyrillic on Unix and Usenet.
(make-coding-system 
 'koi8-r 'fixed-width "KOI8-R (Cyrillic)"
 '(unicode-map
   ((#x80 #x2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 #x2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 #x250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 #x2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 #x2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 #x2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 #x251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 #x2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 #x252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 #x2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A #x253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B #x2580) ;; UPPER HALF BLOCK
    (#x8C #x2584) ;; LOWER HALF BLOCK
    (#x8D #x2588) ;; FULL BLOCK
    (#x8E #x258C) ;; LEFT HALF BLOCK
    (#x8F #x2590) ;; RIGHT HALF BLOCK
    (#x90 #x2591) ;; LIGHT SHADE
    (#x91 #x2592) ;; MEDIUM SHADE
    (#x92 #x2593) ;; DARK SHADE
    (#x93 #x2320) ;; TOP HALF INTEGRAL
    (#x94 #x25A0) ;; BLACK SQUARE
    (#x95 #x2219) ;; BULLET OPERATOR
    (#x96 #x221A) ;; SQUARE ROOT
    (#x97 #x2248) ;; ALMOST EQUAL TO
    (#x98 #x2264) ;; LESS-THAN OR EQUAL TO
    (#x99 #x2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A #x00A0) ;; NO-BREAK SPACE
    (#x9B #x2321) ;; BOTTOM HALF INTEGRAL
    (#x9C #x00B0) ;; DEGREE SIGN
    (#x9D #x00B2) ;; SUPERSCRIPT TWO
    (#x9E #x00B7) ;; MIDDLE DOT
    (#x9F #x00F7) ;; DIVISION SIGN
    (#xA0 #x2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 #x2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 #x2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x2553) ;; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    (#xA5 #x2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 #x2555) ;; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    (#xA7 #x2556) ;; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    (#xA8 #x2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 #x2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA #x2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB #x255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC #x255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD #x255C) ;; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    (#xAE #x255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xAF #x255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 #x255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 #x2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 #x2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 #x2562) ;; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    (#xB5 #x2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 #x2564) ;; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    (#xB7 #x2565) ;; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    (#xB8 #x2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 #x2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA #x2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB #x2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC #x256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD #x256B) ;; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    (#xBE #x256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xBF #x00A9) ;; COPYRIGHT SIGN
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic "KOI8"
   documentation
   "This is ,L:^T(B ,L>Q\U]P(B ,L8]d^`\PfXUY(B, 8 ,LQXb(B, the ASCII-compatible encoding
documented in RFC 1341. ,L:>8(B8-,L@(B is very common across the Cyrillic-using
internet and in Unix implementations; it shares the useful property with the
other ,L:>8(B8 encodings that when the high bit is stripped, encoded text
remains readable (Cyrillic characters get mapped to corresponding Roman
characters of the opposite case). "
   aliases (cyrillic-koi8 koi8 cp878)))

(make-coding-system
 'koi8-r 'mbcs
 "KOI8-R (Cyrillic)"
 '(charsets (ascii cyrillic-koi8-r)
   mnemonic "KOI8"))

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

(set-language-info-alist
 "Cyrillic-KOI8"
 (remassq 'locale (copy-list (cdr (assoc "Russian" language-info-alist))))
 '("Cyrillic"))

;; KOI8-U, for Ukrainian.
(make-coding-system
 'koi8-u 'fixed-width "KOI8-U, Ukrainian"
 '(unicode-map
   ((#x80 #x2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 #x2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 #x250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 #x2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 #x2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 #x2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 #x251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 #x2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 #x252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 #x2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A #x253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B #x2580) ;; UPPER HALF BLOCK
    (#x8C #x2584) ;; LOWER HALF BLOCK
    (#x8D #x2588) ;; FULL BLOCK
    (#x8E #x258C) ;; LEFT HALF BLOCK
    (#x8F #x2590) ;; RIGHT HALF BLOCK
    (#x90 #x2591) ;; LIGHT SHADE
    (#x91 #x2592) ;; MEDIUM SHADE
    (#x92 #x2593) ;; DARK SHADE
    (#x93 #x2320) ;; TOP HALF INTEGRAL
    (#x94 #x25A0) ;; BLACK SQUARE
    (#x95 #x2022) ;; BULLET
    (#x96 #x221A) ;; SQUARE ROOT
    (#x97 #x2248) ;; ALMOST EQUAL TO
    (#x98 #x2264) ;; LESS-THAN OR EQUAL TO
    (#x99 #x2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A #x00A0) ;; NO-BREAK SPACE
    (#x9B #x2321) ;; BOTTOM HALF INTEGRAL
    (#x9C #x00B0) ;; DEGREE SIGN
    (#x9D #x00B2) ;; SUPERSCRIPT TWO
    (#x9E #x00B7) ;; MIDDLE DOT
    (#x9F #x00F7) ;; DIVISION SIGN
    (#xA0 #x2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 #x2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 #x2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 #x2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 #x0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 #x0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 #x2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 #x2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA #x2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB #x255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC #x255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD #x0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE #x255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xAF #x255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 #x255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 #x2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 #x2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 #x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 #x2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 #x0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 #x0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 #x2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 #x2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA #x2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB #x2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC #x256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD #x0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE #x256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xBF #x00A9) ;; COPYRIGHT SIGN
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
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
  in '((#x0404 #x0454) ; UKRAINIAN IE
       (#x0406 #x0456) ; BYELORUSSIAN-UKRAINIAN I
       (#x0407 #x0457) ; YI
       (#x0490 #x0491)); GHE WITH UPTURN
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

(set-language-info-alist
 "Ukrainian" '((coding-system koi8-u)
               (coding-priority koi8-u)
               (locale "uk")
               (invalid-sequence-coding-system koi8-u)
               (input-method . "cyrillic-ukrainian")
               (documentation
                . "Support for Ukrainian."))
 '("Cyrillic"))

;; charset cyrillic-windows-1251 defined in windows.el.

(make-coding-system 
 'windows-1251 'mbcs "Microsoft's CP1251, Cyrillic."
 '(charsets (ascii cyrillic-windows-1251)
   mnemonic "CyrW"
   documentation
   "This ASCII-compatible encoding is unfortunately not compatible at
the code point level with the KOI8 family of encodings, but it
provides several widely-used punctuation and quotation marks that
KOI-8R and its relatives don't, and has become widely used. 

It supports Russian, Bulgarian, Serbian and other languages written using
Cyrillic script.  "
   aliases (cp1251)))

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

;;; Alternativnyj
(make-coding-system
 'alternativnyj 'fixed-width "Microsoft's CP966, Cyrillic"
 '(unicode-map
   ((#x80 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#x81 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#x82 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#x83 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#x84 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#x85 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#x86 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#x87 #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#x88 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#x89 #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#x8A #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#x8B #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#x8C #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#x8D #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#x8E #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#x8F #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#x90 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#x91 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#x92 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#x93 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#x94 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#x95 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#x96 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#x97 #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#x98 #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#x99 #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#x9A #x042A) ;; CYRILLIC CAPITAL LETTER HARD SIGN
    (#x9B #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#x9C #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#x9D #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#x9E #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#x9F #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xA0 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xA1 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xA2 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xA3 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xA4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xA5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xA6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xA7 #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xA8 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xA9 #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xAA #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xAB #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xAC #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xAD #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xAE #x043E) ;; CYRILLIC SMALL LETTER O
    (#xAF #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xB0 #x2591) ;; LIGHT SHADE
    (#xB1 #x2592) ;; MEDIUM SHADE
    (#xB2 #x2593) ;; DARK SHADE
    (#xB3 #x2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#xB4 #x2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#xB5 #x2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB6 #x2562) ;; BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    (#xB7 #x2556) ;; BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    (#xB8 #x2555) ;; BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    (#xB9 #x2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xBA #x2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xBB #x2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xBC #x255D) ;; BOX DRAWINGS DOUBLE UP AND LEFT
    (#xBD #x255C) ;; BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    (#xBE #x255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xBF #x2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#xC0 #x2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#xC1 #x2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#xC2 #x252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#xC3 #x251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#xC4 #x2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#xC5 #x253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#xC6 #x255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xC7 #x255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xC8 #x255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xC9 #x2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xCA #x2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xCB #x2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xCC #x2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xCD #x2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xCE #x256C) ;; BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    (#xCF #x2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xD0 #x2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xD1 #x2564) ;; BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    (#xD2 #x2565) ;; BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    (#xD3 #x2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xD4 #x2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xD5 #x2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xD6 #x2553) ;; BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    (#xD7 #x256B) ;; BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    (#xD8 #x256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xD9 #x2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#xDA #x250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#xDB #x2588) ;; FULL BLOCK
    (#xDC #x2584) ;; LOWER HALF BLOCK
    (#xDD #x258C) ;; LEFT HALF BLOCK
    (#xDE #x2590) ;; RIGHT HALF BLOCK
    (#xDF #x2580) ;; UPPER HALF BLOCK
    (#xE0 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xE1 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xE2 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xE3 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xE4 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xE5 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xE6 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xE7 #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xE8 #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xE9 #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xEA #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xEB #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xEC #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xED #x044D) ;; CYRILLIC SMALL LETTER E
    (#xEE #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xEF #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xF0 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xF1 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xF2 #x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xF3 #x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xF4 #x0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xF5 #x0457) ;; CYRILLIC SMALL LETTER YI
    (#xF6 #x040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xF7 #x045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xF8 #x00B0) ;; DEGREE SIGN
    (#xF9 #x2022) ;; BULLET
    (#xFA #x00B7) ;; MIDDLE DOT
    (#xFB #x221A) ;; SQUARE ROOT
    (#xFC #x2116) ;; NUMERO SIGN
    (#xFD #x00A4) ;; CURRENCY SIGN
    (#xFE #x25A0) ;; BLACK SQUARE
    (#xFF #x00A0)) ;; NO-BREAK SPACE
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
   ((#x80 #x2500) ;; BOX DRAWINGS LIGHT HORIZONTAL
    (#x81 #x2502) ;; BOX DRAWINGS LIGHT VERTICAL
    (#x82 #x250C) ;; BOX DRAWINGS LIGHT DOWN AND RIGHT
    (#x83 #x2510) ;; BOX DRAWINGS LIGHT DOWN AND LEFT
    (#x84 #x2514) ;; BOX DRAWINGS LIGHT UP AND RIGHT
    (#x85 #x2518) ;; BOX DRAWINGS LIGHT UP AND LEFT
    (#x86 #x251C) ;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    (#x87 #x2524) ;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
    (#x88 #x252C) ;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    (#x89 #x2534) ;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
    (#x8A #x253C) ;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    (#x8B #x2580) ;; UPPER HALF BLOCK
    (#x8C #x2584) ;; LOWER HALF BLOCK
    (#x8D #x2588) ;; FULL BLOCK
    (#x8E #x258C) ;; LEFT HALF BLOCK
    (#x8F #x2590) ;; RIGHT HALF BLOCK
    (#x90 #x2591) ;; LIGHT SHADE
    (#x91 #x2592) ;; MEDIUM SHADE
    (#x92 #x2593) ;; DARK SHADE
    (#x93 #x2320) ;; TOP HALF INTEGRAL
    (#x94 #x25A0) ;; BLACK SQUARE
    (#x95 #x2219) ;; BULLET OPERATOR
    (#x96 #x221A) ;; SQUARE ROOT
    (#x97 #x2248) ;; ALMOST EQUAL TO
    (#x98 #x2264) ;; LESS-THAN OR EQUAL TO
    (#x99 #x2265) ;; GREATER-THAN OR EQUAL TO
    (#x9A #x00A0) ;; NO-BREAK SPACE
    (#x9B #x2321) ;; BOTTOM HALF INTEGRAL
    (#x9C #x00B0) ;; DEGREE SIGN
    (#x9D #x00B2) ;; SUPERSCRIPT TWO
    (#x9E #x00B7) ;; MIDDLE DOT
    (#x9F #x00F7) ;; DIVISION SIGN
    (#xA0 #x2550) ;; BOX DRAWINGS DOUBLE HORIZONTAL
    (#xA1 #x2551) ;; BOX DRAWINGS DOUBLE VERTICAL
    (#xA2 #x2552) ;; BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 #x2554) ;; BOX DRAWINGS DOUBLE DOWN AND RIGHT
    (#xA6 #x0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 #x0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 #x2557) ;; BOX DRAWINGS DOUBLE DOWN AND LEFT
    (#xA9 #x2558) ;; BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    (#xAA #x2559) ;; BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    (#xAB #x255A) ;; BOX DRAWINGS DOUBLE UP AND RIGHT
    (#xAC #x255B) ;; BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    (#xAD #x0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE #x045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF #x255E) ;; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    (#xB0 #x255F) ;; BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    (#xB1 #x2560) ;; BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    (#xB2 #x2561) ;; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 #x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 #x2563) ;; BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    (#xB6 #x0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 #x0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 #x2566) ;; BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    (#xB9 #x2567) ;; BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    (#xBA #x2568) ;; BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    (#xBB #x2569) ;; BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    (#xBC #x256A) ;; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    (#xBD #x0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE #x040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF #x00A9) ;; COPYRIGHT SIGN
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",L@C(B"
   documentation "KOI8-RU, a mostly-compatible superset of KOI8-R. 
Also known as Windows code page 21866; has Ukrainian and Belarussian support. "
   aliases (cp21866)))

;; We should provide an input method and the corresponding language
;; environments for the next three coding systems. 

(make-coding-system
 'koi8-t 'fixed-width ",L:>8(B-8 for Tajik."
 '(unicode-map
   ((#x80 #x049B) ;; CYRILLIC SMALL LETTER KA WITH DESCENDER
    (#x81 #x0493) ;; CYRILLIC SMALL LETTER GHE WITH STROKE
    (#x82 #x201A) ;; SINGLE LOW-9 QUOTATION MARK
    (#x83 #x0492) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 #x2026) ;; HORIZONTAL ELLIPSIS
    (#x86 #x2020) ;; DAGGER
    (#x87 #x2021) ;; DOUBLE DAGGER
    (#x89 #x2030) ;; PER MILLE SIGN
    (#x8A #x04B3) ;; CYRILLIC SMALL LETTER HA WITH DESCENDER
    (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C #x04B2) ;; CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (#x8D #x04B7) ;; CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (#x8E #x04B6) ;; CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (#x90 #x049A) ;; CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 #x2022) ;; BULLET
    (#x96 #x2013) ;; EN DASH
    (#x97 #x2014) ;; EM DASH
    (#x99 #x2122) ;; TRADE MARK SIGN
    (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#xA1 #x04EF) ;; CYRILLIC SMALL LETTER U WITH MACRON
    (#xA2 #x04EE) ;; CYRILLIC CAPITAL LETTER U WITH MACRON
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x00A4) ;; CURRENCY SIGN
    (#xA5 #x04E3) ;; CYRILLIC SMALL LETTER I WITH MACRON
    (#xA6 #x00A6) ;; BROKEN BAR
    (#xA7 #x00A7) ;; SECTION SIGN
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x00AC) ;; NOT SIGN
    (#xAD #x00AD) ;; SOFT HYPHEN
    (#xAE #x00AE) ;; REGISTERED SIGN
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x00B1) ;; PLUS-MINUS SIGN
    (#xB2 #x00B2) ;; SUPERSCRIPT TWO
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB5 #x04E2) ;; CYRILLIC CAPITAL LETTER I WITH MACRON
    (#xB6 #x00B6) ;; PILCROW SIGN
    (#xB7 #x00B7) ;; MIDDLE DOT
    (#xB9 #x2116) ;; NUMERO SIGN
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBF #x00A9) ;; COPYRIGHT SIGN
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",LB%GҶ%@(B"
   documentation
   "Compatible in the alphabetic characters with KOI-8R for Russian,
this 8-bit Cyrillic coding system makes those characters available
that are necessary for writing Tajik, (,LWPQ^]%Gӣ%@(B ,Lb^%Gҷ%@XZ%Gӣ%@(B) the main
language of Tajikistan and a close relative of Persian.  "))

;; Case support, for the new characters.
(loop
  for (upper lower)
  in '((#x04B6 #x04B7) ;; CHE WITH DESCENDER
       (#x0492 #x0493) ;; GHE WITH STROKE
       (#x04B2 #x04B3) ;; HA WITH DESCENDER
       (#x04E2 #x04E3) ;; I WITH MACRON
       (#x049A #x049B) ;; KA WITH DESCENDER
       (#x04EE #x04EF)) ;; U WITH MACRON
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

;; Support fot the languages of the Caucasus.
(make-coding-system
 'koi8-c 'fixed-width "KOI-8, Caucasus."
 '(unicode-map
   ((#x80 #x0493) ;; CYRILLIC SMALL LETTER GHE WITH STROKE
    (#x81 #x0497) ;; CYRILLIC SMALL LETTER ZHE WITH DESCENDER
    (#x82 #x049B) ;; CYRILLIC SMALL LETTER KA WITH DESCENDER
    (#x83 #x049D) ;; CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (#x84 #x04A3) ;; CYRILLIC SMALL LETTER EN WITH DESCENDER
    (#x85 #x04AF) ;; CYRILLIC SMALL LETTER STRAIGHT U
    (#x86 #x04B1) ;; CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
    (#x87 #x04B3) ;; CYRILLIC SMALL LETTER HA WITH DESCENDER
    (#x88 #x04B7) ;; CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (#x89 #x04B9) ;; CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
    (#x8A #x04BB) ;; CYRILLIC SMALL LETTER SHHA
    (#x8B #x2580) ;; UPPER HALF BLOCK
    (#x8C #x049D) ;; CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (#x8D #x04E3) ;; CYRILLIC SMALL LETTER I WITH MACRON
    (#x8E #x04E9) ;; CYRILLIC SMALL LETTER BARRED O
    (#x8F #x04EF) ;; CYRILLIC SMALL LETTER U WITH MACRON
    (#x90 #x0492) ;; CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (#x91 #x0496) ;; CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    (#x92 #x049A) ;; CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (#x93 #x049C) ;; CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    (#x94 #x04A2) ;; CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    (#x95 #x04AE) ;; CYRILLIC CAPITAL LETTER STRAIGHT U
    (#x96 #x04B0) ;; CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    (#x97 #x04B2) ;; CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (#x98 #x04B6) ;; CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (#x99 #x04B8) ;; CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    (#x9A #x04BA) ;; CYRILLIC CAPITAL LETTER SHHA
    (#x9B #x2321) ;; BOTTOM HALF INTEGRAL
    (#x9C #x04D8) ;; CYRILLIC CAPITAL LETTER SCHWA
    (#x9D #x04E2) ;; CYRILLIC CAPITAL LETTER I WITH MACRON
    (#x9E #x04E8) ;; CYRILLIC CAPITAL LETTER BARRED O
    (#x9F #x04EE) ;; CYRILLIC CAPITAL LETTER U WITH MACRON
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA1 #x0452) ;; CYRILLIC SMALL LETTER DJE
    (#xA2 #x0453) ;; CYRILLIC SMALL LETTER GJE
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 #x0455) ;; CYRILLIC SMALL LETTER DZE
    (#xA6 #x0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 #x0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 #x0458) ;; CYRILLIC SMALL LETTER JE
    (#xA9 #x0459) ;; CYRILLIC SMALL LETTER LJE
    (#xAA #x045A) ;; CYRILLIC SMALL LETTER NJE
    (#xAB #x045B) ;; CYRILLIC SMALL LETTER TSHE
    (#xAC #x045C) ;; CYRILLIC SMALL LETTER KJE
    (#xAD #x0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE #x045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF #x045F) ;; CYRILLIC SMALL LETTER DZHE
    (#xB0 #x2116) ;; NUMERO SIGN
    (#xB1 #x0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#xB2 #x0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 #x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 #x0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xB6 #x0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 #x0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 #x0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xB9 #x0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#xBA #x040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#xBB #x040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#xBC #x040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#xBD #x0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE #x040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF #x040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   documentation 
   "Note that this does not support old Russian orthography;
for that, see koi8-o. "))

(loop
  for (upper lower)
  in '((#x04E9 #x04E8) ;; BARRED O
       (#x04B9 #x04B8) ;; CHE WITH VERTICAL STROKE
       (#x0452 #x0402) ;; DJE
       (#x0455 #x0405) ;; DZE
       (#x04A3 #x04A2) ;; EN WITH DESCENDER
       (#x049D #x049C) ;; KA WITH VERTICAL STROKE
       (#x04BB #x04BA) ;; SHHA
       (#x04AF #x04AE) ;; STRAIGHT U
       (#x04B1 #x04B0) ;; STRAIGHT U WITH STROKE
       (#x0497 #x0496)) ;; ZHE WITH DESCENDER
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

;; Archaic Russian support.
(make-coding-system
 'koi8-o 'fixed-width "Old-orthography Russian"
 '(unicode-map
   ((#x80 #x0402) ;; CYRILLIC CAPITAL LETTER DJE
    (#x81 #x0403) ;; CYRILLIC CAPITAL LETTER GJE
    (#x82 #x00B8) ;; CEDILLA
    (#x83 #x0453) ;; CYRILLIC SMALL LETTER GJE
    (#x84 #x201E) ;; DOUBLE LOW-9 QUOTATION MARK
    (#x85 #x2026) ;; HORIZONTAL ELLIPSIS
    (#x86 #x2020) ;; DAGGER
    (#x87 #x00A7) ;; SECTION SIGN
    (#x88 #x20AC) ;; EURO SIGN
    (#x89 #x00A8) ;; DIAERESIS
    (#x8A #x0409) ;; CYRILLIC CAPITAL LETTER LJE
    (#x8B #x2039) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (#x8C #x040A) ;; CYRILLIC CAPITAL LETTER NJE
    (#x8D #x040C) ;; CYRILLIC CAPITAL LETTER KJE
    (#x8E #x040B) ;; CYRILLIC CAPITAL LETTER TSHE
    (#x8F #x040F) ;; CYRILLIC CAPITAL LETTER DZHE
    (#x90 #x0452) ;; CYRILLIC SMALL LETTER DJE
    (#x91 #x2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 #x2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 #x201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 #x201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 #x2022) ;; BULLET
    (#x96 #x2013) ;; EN DASH
    (#x97 #x2014) ;; EM DASH
    (#x98 #x00A3) ;; POUND SIGN
    (#x99 #x00B7) ;; MIDDLE DOT
    (#x9A #x0459) ;; CYRILLIC SMALL LETTER LJE
    (#x9B #x203A) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (#x9C #x045A) ;; CYRILLIC SMALL LETTER NJE
    (#x9D #x045C) ;; CYRILLIC SMALL LETTER KJE
    (#x9E #x045B) ;; CYRILLIC SMALL LETTER TSHE
    (#x9F #x045F) ;; CYRILLIC SMALL LETTER DZHE
    (#xA0 #x00A0) ;; NO-BREAK SPACE
    (#xA1 #x0475) ;; CYRILLIC SMALL LETTER IZHITSA
    (#xA2 #x0463) ;; CYRILLIC SMALL LETTER YAT
    (#xA3 #x0451) ;; CYRILLIC SMALL LETTER IO
    (#xA4 #x0454) ;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (#xA5 #x0455) ;; CYRILLIC SMALL LETTER DZE
    (#xA6 #x0456) ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xA7 #x0457) ;; CYRILLIC SMALL LETTER YI
    (#xA8 #x0458) ;; CYRILLIC SMALL LETTER JE
    (#xA9 #x00AE) ;; REGISTERED SIGN
    (#xAA #x2122) ;; TRADE MARK SIGN
    (#xAB #x00AB) ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xAC #x0473) ;; CYRILLIC SMALL LETTER FITA
    (#xAD #x0491) ;; CYRILLIC SMALL LETTER GHE WITH UPTURN
    (#xAE #x045E) ;; CYRILLIC SMALL LETTER SHORT U
    (#xAF #x00B4) ;; ACUTE ACCENT
    (#xB0 #x00B0) ;; DEGREE SIGN
    (#xB1 #x0474) ;; CYRILLIC CAPITAL LETTER IZHITSA
    (#xB2 #x0462) ;; CYRILLIC CAPITAL LETTER YAT
    (#xB3 #x0401) ;; CYRILLIC CAPITAL LETTER IO
    (#xB4 #x0404) ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (#xB5 #x0405) ;; CYRILLIC CAPITAL LETTER DZE
    (#xB6 #x0406) ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (#xB7 #x0407) ;; CYRILLIC CAPITAL LETTER YI
    (#xB8 #x0408) ;; CYRILLIC CAPITAL LETTER JE
    (#xB9 #x2116) ;; NUMERO SIGN
    (#xBA #x00A2) ;; CENT SIGN
    (#xBB #x00BB) ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    (#xBC #x0472) ;; CYRILLIC CAPITAL LETTER FITA
    (#xBD #x0490) ;; CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (#xBE #x040E) ;; CYRILLIC CAPITAL LETTER SHORT U
    (#xBF #x00A9) ;; COPYRIGHT SIGN
    (#xC0 #x044E) ;; CYRILLIC SMALL LETTER YU
    (#xC1 #x0430) ;; CYRILLIC SMALL LETTER A
    (#xC2 #x0431) ;; CYRILLIC SMALL LETTER BE
    (#xC3 #x0446) ;; CYRILLIC SMALL LETTER TSE
    (#xC4 #x0434) ;; CYRILLIC SMALL LETTER DE
    (#xC5 #x0435) ;; CYRILLIC SMALL LETTER IE
    (#xC6 #x0444) ;; CYRILLIC SMALL LETTER EF
    (#xC7 #x0433) ;; CYRILLIC SMALL LETTER GHE
    (#xC8 #x0445) ;; CYRILLIC SMALL LETTER HA
    (#xC9 #x0438) ;; CYRILLIC SMALL LETTER I
    (#xCA #x0439) ;; CYRILLIC SMALL LETTER SHORT I
    (#xCB #x043A) ;; CYRILLIC SMALL LETTER KA
    (#xCC #x043B) ;; CYRILLIC SMALL LETTER EL
    (#xCD #x043C) ;; CYRILLIC SMALL LETTER EM
    (#xCE #x043D) ;; CYRILLIC SMALL LETTER EN
    (#xCF #x043E) ;; CYRILLIC SMALL LETTER O
    (#xD0 #x043F) ;; CYRILLIC SMALL LETTER PE
    (#xD1 #x044F) ;; CYRILLIC SMALL LETTER YA
    (#xD2 #x0440) ;; CYRILLIC SMALL LETTER ER
    (#xD3 #x0441) ;; CYRILLIC SMALL LETTER ES
    (#xD4 #x0442) ;; CYRILLIC SMALL LETTER TE
    (#xD5 #x0443) ;; CYRILLIC SMALL LETTER U
    (#xD6 #x0436) ;; CYRILLIC SMALL LETTER ZHE
    (#xD7 #x0432) ;; CYRILLIC SMALL LETTER VE
    (#xD8 #x044C) ;; CYRILLIC SMALL LETTER SOFT SIGN
    (#xD9 #x044B) ;; CYRILLIC SMALL LETTER YERU
    (#xDA #x0437) ;; CYRILLIC SMALL LETTER ZE
    (#xDB #x0448) ;; CYRILLIC SMALL LETTER SHA
    (#xDC #x044D) ;; CYRILLIC SMALL LETTER E
    (#xDD #x0449) ;; CYRILLIC SMALL LETTER SHCHA
    (#xDE #x0447) ;; CYRILLIC SMALL LETTER CHE
    (#xDF #x044A) ;; CYRILLIC SMALL LETTER HARD SIGN
    (#xE0 #x042E) ;; CYRILLIC CAPITAL LETTER YU
    (#xE1 #x0410) ;; CYRILLIC CAPITAL LETTER A
    (#xE2 #x0411) ;; CYRILLIC CAPITAL LETTER BE
    (#xE3 #x0426) ;; CYRILLIC CAPITAL LETTER TSE
    (#xE4 #x0414) ;; CYRILLIC CAPITAL LETTER DE
    (#xE5 #x0415) ;; CYRILLIC CAPITAL LETTER IE
    (#xE6 #x0424) ;; CYRILLIC CAPITAL LETTER EF
    (#xE7 #x0413) ;; CYRILLIC CAPITAL LETTER GHE
    (#xE8 #x0425) ;; CYRILLIC CAPITAL LETTER HA
    (#xE9 #x0418) ;; CYRILLIC CAPITAL LETTER I
    (#xEA #x0419) ;; CYRILLIC CAPITAL LETTER SHORT I
    (#xEB #x041A) ;; CYRILLIC CAPITAL LETTER KA
    (#xEC #x041B) ;; CYRILLIC CAPITAL LETTER EL
    (#xED #x041C) ;; CYRILLIC CAPITAL LETTER EM
    (#xEE #x041D) ;; CYRILLIC CAPITAL LETTER EN
    (#xEF #x041E) ;; CYRILLIC CAPITAL LETTER O
    (#xF0 #x041F) ;; CYRILLIC CAPITAL LETTER PE
    (#xF1 #x042F) ;; CYRILLIC CAPITAL LETTER YA
    (#xF2 #x0420) ;; CYRILLIC CAPITAL LETTER ER
    (#xF3 #x0421) ;; CYRILLIC CAPITAL LETTER ES
    (#xF4 #x0422) ;; CYRILLIC CAPITAL LETTER TE
    (#xF5 #x0423) ;; CYRILLIC CAPITAL LETTER U
    (#xF6 #x0416) ;; CYRILLIC CAPITAL LETTER ZHE
    (#xF7 #x0412) ;; CYRILLIC CAPITAL LETTER VE
    (#xF8 #x042C) ;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (#xF9 #x042B) ;; CYRILLIC CAPITAL LETTER YERU
    (#xFA #x0417) ;; CYRILLIC CAPITAL LETTER ZE
    (#xFB #x0428) ;; CYRILLIC CAPITAL LETTER SHA
    (#xFC #x042D) ;; CYRILLIC CAPITAL LETTER E
    (#xFD #x0429) ;; CYRILLIC CAPITAL LETTER SHCHA
    (#xFE #x0427) ;; CYRILLIC CAPITAL LETTER CHE
    (#xFF #x042A)) ;; CYRILLIC CAPITAL LETTER HARD SIGN
   mnemonic ",L:>(B"
   documentation
   "KOI-8 for old-orthography Russian; also known as KOI-C.

This is mostly compatible with KOI8-R in the alphabetic characters, and
provides ,L&v(B, %GѲѳ%@, %GѢѣ%@, and %GѴѵ%@ instead of some of the box-drawing characters.  "))

(loop
  for (upper lower)
  in '((#x0472 #x0473) ;; FITA
       (#x0474 #x0475) ;; IZHITSA
       (#x0408 #x0458) ;; JE
       (#x0462 #x0463)) ;; YAT
  with case-table = (standard-case-table)
  do
  (put-case-table-pair upper lower case-table))

(provide 'cyrillic)

;;; cyrillic.el ends here
