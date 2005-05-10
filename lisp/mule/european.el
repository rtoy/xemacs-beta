;;; european.el --- European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001 Ben Wing.
;; Copyright (C) 2002, 2005 Free Software Foundation

;; Keywords: multilingual, European

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

;; #### latin.el would be a better name for this file.

;;; Code:
; (make-charset 'latin-iso8859-1 
; 	      "Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100"
; 	      '(dimension
; 		1
; 		registry "ISO8859-1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?A
; 		graphic 1
; 		short-name "RHP of Latin-1"
; 		long-name "RHP of Latin-1 (ISO 8859-1): ISO-IR-100"
; 		))

; (make-charset 'latin-iso8859-2 
; 	      "Right-Hand Part of Latin Alphabet 2 (ISO/IEC 8859-2): ISO-IR-101"
; 	      '(dimension
; 		1
; 		registry "ISO8859-2"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?B
; 		graphic 1
; 		short-name "RHP of Latin-2"
; 		long-name "RHP of Latin-2 (ISO 8859-2): ISO-IR-101"
; 		))

; (make-charset 'latin-iso8859-3 
; 	      "Right-Hand Part of Latin Alphabet 3 (ISO/IEC 8859-3): ISO-IR-109"
; 	      '(dimension
; 		1
; 		registry "ISO8859-3"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?C
; 		graphic 1
; 		short-name "RHP of Latin-3"
; 		long-name "RHP of Latin-3 (ISO 8859-3): ISO-IR-109"
; 		))

; (make-charset 'latin-iso8859-4 
; 	      "Right-Hand Part of Latin Alphabet 4 (ISO/IEC 8859-4): ISO-IR-110"
; 	      '(dimension
; 		1
; 		registry "ISO8859-4"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?D
; 		graphic 1
; 		short-name "RHP of Latin-4"
; 		long-name "RHP of Latin-4 (ISO 8859-4): ISO-IR-110"
; 		))

; (make-charset 'latin-iso8859-9 
; 	      "Right-Hand Part of Latin Alphabet 5 (ISO/IEC 8859-9): ISO-IR-148"
; 	      '(dimension
; 		1
; 		registry "ISO8859-9"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?M
; 		graphic 1
; 		short-name "RHP of Latin-5"
; 		long-name "RHP of Latin-5 (ISO 8859-9): ISO-IR-148"
; 		))

; (make-charset 'latin-iso8859-15 
; 	      "Right-Hand Part of Latin Alphabet 9 (ISO/IEC 8859-15): ISO-IR-203"
; 	      '(dimension
; 		1
; 		registry "ISO8859-15"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?b
; 		graphic 1
; 		short-name "RHP of Latin-9"
; 		long-name "RHP of Latin-9 (ISO 8859-15): ISO-IR-203"
; 		))

(make-charset 'latin-iso8859-14 
	      "Right-Hand Part of Latin Alphabet 8 (ISO/IEC 8859-14)"
	      '(dimension
		1
		registry "ISO8859-14"
		chars 96
		columns 1
		direction l2r
		final ?_
		graphic 1
		short-name "RHP of Latin-8"
		long-name "RHP of Latin-8 (ISO 8859-14)"
		))

(make-charset 'latin-iso8859-16
	      "Right-Hand Part of Latin Alphabet 10 (ISO/IEC 8859-16)"
	      '(dimension
		1
		registry "ISO8859-16"
		chars 96
		columns 1
		direction l2r
		final ?f			; octet 06/06; cf ISO-IR 226
		graphic 1
		short-name "RHP of Latin-10"
		long-name "RHP of Latin-10 (ISO 8859-16)"
		))

;; Latin-1 is dealt with in iso8859-1.el, which see. 

;; ISO 8859-14. 
;; 
;; Initialise all characters to word syntax.
(loop for c from #xa0 to #xff
  do (modify-syntax-entry (make-char 'latin-iso8859-14 c) "w"))

;; Now, the exceptions. There's just punctuation in this character set. 
(dolist (code '(#xa0	;; NO BREAK SPACE
		#xa3	;; POUND SIGN
		#xa7	;; SECTION SIGN
		#xa9	;; COPYRIGHT
		#xad	;; SOFT HYPHEN
		#xae	;; REGISTERED
		#xb6))	;; PILCROW SIGN
  (modify-syntax-entry (make-char 'latin-iso8859-14 code) "_"))
;; end of ISO 8859-14.

;; ISO 8859-16.
;;
;; Initialise all of iso-8859-16 to word syntax. 
(loop for c from #xa0 to #xff
  do (modify-syntax-entry (make-char 'latin-iso8859-16 c) "w"))

;; And then do the exceptions. First, the punctuation (following the model
;; of Latin-1):
(dolist (code '(#xa0	;; NO BREAK SPACE
		#xa4	;; EURO SIGN
		#xa7	;; SECTION SIGN
		#xa9	;; COPYRIGHT
		#xad	;; SOFT HYPHEN
		#xb0	;; DEGREE
		#xb1	;; PLUS-MINUS SIGN
		#xb6	;; PILCROW SIGN
		#xb7)) ;; MIDDLE DOT 
  (modify-syntax-entry (make-char 'latin-iso8859-16 code) "_"))

;; Mark the DOUBLE LOW-9 QUOTATION MARK and its closing character as
;; quotation marks.
(modify-syntax-entry (make-char 'latin-iso8859-16 #xa5) "\"")
(modify-syntax-entry (make-char 'latin-iso8859-16 #xb5) "\"")

;; For some crazy reason--well, in truth, probably because Jamie never used
;; them in anger--the guillemets have open- and close-parenthesis syntax in
;; Latin 1. We will probably change that in the future; for the moment, I'm
;; preserving it.
(modify-syntax-entry (make-char 'latin-iso8859-16 #xab) 
		     (format "(%c" (make-char 'latin-iso8859-16 #xbb)))
(modify-syntax-entry (make-char 'latin-iso8859-16 #xbb) 
		     (format ")%c" (make-char 'latin-iso8859-16 #xab)))

;; end of ISO 8859-16. 

;; ISO 8859-15. 
;; 
;; Based on Latin-1 and differences therefrom.
;; 
;; First, initialise the syntax from the corresponding Latin-1 characters. 
(loop for c from #xa0 to #xff
      do (modify-syntax-entry 
	  (make-char 'latin-iso8859-15 c)
	  (string (char-syntax (make-char 'latin-iso8859-1 c)))))
;; Now, the exceptions
(loop for c in '(?,b&(B ?,b((B ?,b4(B ?,b8(B ?,b<(B ?,b=(B ?,b>(B)
      do (modify-syntax-entry c "w"))

;; Again, perpetuating insanity with the guillemets.
(modify-syntax-entry (make-char 'latin-iso8859-16 #xab) 
		     (format "(%c" (make-char 'latin-iso8859-16 #xbb)))
(modify-syntax-entry (make-char 'latin-iso8859-16 #xbb) 
		     (format ")%c" (make-char 'latin-iso8859-16 #xab)))
;; end of ISO 8859-15. 

;; For syntax of Latin-2
(loop for c in '(?,B!(B ?,B#(B ?,B%(B ?,B&(B ?,B)(B ?,B*(B ?,B+(B ?,B,(B ?,B.(B ?,B/(B ?,B1(B ?,B3(B ?,B5(B ?,B6(B ?,B9(B ?,B:(B ?,B;(B ?,B<(B)
      do (modify-syntax-entry c "w"))

(loop for c from 62 to 126
      do (modify-syntax-entry (make-char 'latin-iso8859-2 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-2 32) "w") ; no-break space
(modify-syntax-entry ?,BW(B ".")
(modify-syntax-entry ?,Bw(B ".")

;; For syntax of Latin-3
(loop for c in '(?,C!(B ?,C&(B ?,C)(B ?,C*(B ?,C+(B ?,C,(B ?,C/(B ?,C1(B ?,C5(B ?,C6(B ?,C:(B ?,C;(B ?,C<(B ?,C?(B)
  do (modify-syntax-entry c "w"))

(loop for c from 64 to 126
  do (modify-syntax-entry (make-char 'latin-iso8859-3 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-3 32) "w") ; no-break space
(modify-syntax-entry ?,CW(B ".")
(modify-syntax-entry ?,Cw(B ".")

;; For syntax of Latin-4
(loop for c in '(?,D!(B ?,D"(B ?,D#(B ?,D%(B ?,D&(B ?,D)(B ?,D*(B ?,D+(B ?,D,(B ?,D.(B ?,D1(B ?,D3(B ?,D5(B ?,D6(B ?,D9(B ?,D:(B ?,D;(B ?,D<(B ?,D=(B ?,D>(B ?,D?(B)
  do (modify-syntax-entry c "w"))

(loop for c from 64 to 126
  do (modify-syntax-entry (make-char 'latin-iso8859-4 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-4 32) "w") ; no-break space
(modify-syntax-entry ?,DW(B ".")
(modify-syntax-entry ?,Dw(B ".")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EUROPEANS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Latin-1 (ISO-8859-1)

;; (make-coding-system
;;  'iso-latin-1 2 ?1
;;  "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)"
;;  '(ascii latin-iso8859-1 nil nil
;;    nil nil nil nil nil nil nil nil nil nil nil nil t)
;;  '((safe-charsets ascii latin-iso8859-1)
;;    (mime-charset . iso-8859-1)))

;; (define-coding-system-alias 'iso-8859-1 'iso-latin-1)
;; (define-coding-system-alias 'latin-1 'iso-latin-1)

;; (make-coding-system
;;  'compound-text 2 ?1
;;  "ISO 2022 based encoding used in inter client communication of X"
;;  '((ascii t) (latin-iso8859-1 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil nil nil nil nil nil t)
;;  '((safe-charsets . t)))

;; (define-coding-system-alias 'ctext 'compound-text)

;; "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!"


;; Latin-9 (ISO-8859-15)
;; Latin-1 plus Euro, plus a few accented characters

;; (make-charset 'latin-iso8859-15
;;   "Latin-9, aka Latin-1 with Euro etc"
;;   '(short-name "Latin 9"
;;     long-name  "Latin-9 (typically GR of ISO 8859/15)"
;;     registry   "iso8859-15"
;;     dimension  1
;;     columns    1
;;     chars      96
;;     final      ?b                  ; ISO-IR-203
;;     graphic    1
;;     direction  l2r))

(make-coding-system
 'iso-8859-15 'iso2022
  "ISO 4873 conforming 8-bit code (ASCII + Latin 9; aka Latin-1 with Euro)"
  `(mnemonic "MIME/Ltn-9"		; bletch
    eol-type nil
    charset-g0 ascii
    charset-g1 latin-iso8859-15
    charset-g2 t
    charset-g3 t
    ))


;; Latin-2 (ISO-8859-2)

;; (make-coding-system
;;  'iso-latin-2 2 ?2
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-2)"
;;  '(ascii latin-iso8859-2 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-2)
;;    (mime-charset . iso-8859-2)))

;; (define-coding-system-alias 'iso-8859-2 'iso-latin-2)
;; (define-coding-system-alias 'latin-2 'iso-latin-2)

(make-coding-system
 'iso-8859-2 'iso2022 "ISO-8859-2 (Latin-2)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"
   ))

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


;; Latin-3 (ISO-8859-3)

;; (make-coding-system
;;  'iso-latin-3 2 ?3
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-3)"
;;  '(ascii latin-iso8859-3 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-3)
;;    (mime-charset . iso-8859-3)))

;; (define-coding-system-alias 'iso-8859-3 'iso-latin-3)
;; (define-coding-system-alias 'latin-3 'iso-latin-3)

(make-coding-system
 'iso-8859-3 'iso2022 "ISO-8859-3 (Latin-3)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"
   ))


;; Latin-4 (ISO-8859-4)

;; (make-coding-system
;;  'iso-latin-4 2 ?4
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-4)"
;;  '(ascii latin-iso8859-4 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-4)
;;    (mime-charset . iso-8895-4)))

;; (define-coding-system-alias 'iso-8859-4 'iso-latin-4)
;; (define-coding-system-alias 'latin-4 'iso-latin-4)

(make-coding-system
 'iso-8859-4 'iso2022 "ISO-8859-4 (Latin-4)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"
   ))


;; Latin-5 (ISO-8859-9)

;; (make-coding-system
;;  'iso-latin-5 2 ?9
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-9)"
;;  '(ascii latin-iso8859-9 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-9)
;;    (mime-charset . iso-8859-9)))

;; (define-coding-system-alias 'iso-8859-9 'iso-latin-5)
;; (define-coding-system-alias 'latin-5 'iso-latin-5)

(make-coding-system
 'iso-8859-9 'iso2022 "ISO-8859-9 (Latin-5)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-9
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-5"
   ))

;; Add a coding system for ISO 8859-16.
(make-coding-system
 'iso-8859-16 'iso2022 "MIME ISO-8859-16"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-16
   charset-g2 t			; grrr
   charset-g3 t			; grrr
   mnemonic "MIME/Ltn-10"))

(loop for ((charset codesys default-input nice-charset-1 nice-charset-2
		    supported-langs ;; a list if the doc string is replaced
				    ;; entirely
		    )
	   langenvs) in
  '(
    ((latin-iso8859-1 iso-8859-1 "latin-1-prefix" "Latin-1" "ISO-8859-1"
" Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.")
     (("Danish" "da")
      ("Dutch" "nl" "TUTORIAL.nl")
      ("Faeroese")
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
		       "Latin-9" "ISO-8859-15"
		       ("\
This language environment is a generic one for Latin-9 (ISO-8859-15)
character set which supports the Euro sign and the following languages
(they use the Latin-1 character set by default):
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
Each also has its own specific language environment."))
     ())
    ((latin-iso8859-2 iso-8859-2 "latin-2-prefix" "Latin-2" "ISO-8859-2"
" Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbian, Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish.")
     (("Albanian" nil)
      ("Croatian" ("hrvatski" "hr") "TUTORIAL.hr")
      ("Czech" ("cs" "cz") "TUTORIAL.cs" "P,Bx(Bejeme v,Ba(Bm hezk,B}(B den!"
       "latin-2-postfix")
      ("Hungarian" ("hungarian" "hu"))
      ("Polish" "po" "TUTORIAL.pl")
      ("Romanian" "ro" "TUTORIAL.ro" "Bun,Bc(B ziua, bine a,B~(Bi venit!"
       "latin-2-postfix")
      ("Serbian" "sr")
      ("Slovak" "sk" "TUTORIAL.sk" "Prajeme V,Ba(Bm pr,Bm(Bjemn,B}(B de,Br(B!"
       ;; !!#### FSF "slovak"
       "latin-2-postfix")
      ("Slovenian" "sl" "TUTORIAL.sl" ",B.(Belimo vam uspe,B9(Ben dan!"
       "latin-2-postfix")
      ("Sorbian" nil)))
    ((latin-iso8859-3 iso-8859-3 "latin-3-prefix" "Latin-3" "ISO-8859-3"
" Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish.")
     (("Afrikaans" "af")
      ("Catalan" ("catalan" "ca"))
      ("Esperanto")
      ("Galician")
      ("Maltese")))
    ((latin-iso8859-4 iso-8859-4 "latin-4-prefix" "Latin-4" "ISO-8859-4"
" Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian.")
     (("Estonian" "et")
      ("Greenlandic")
      ("Lappish")
      ("Latvian" "lv")
      ("Lithuanian" "li")))
    ((latin-iso8859-5 iso-8859-9 "latin-5-prefix" "Latin-5" "ISO-8859-9")
     (("Turkish" "tr"))))
  do
  (set-language-info-alist
   nice-charset-1
   `((charset ascii ,charset)
     (coding-system ,codesys)
     (coding-priority ,codesys)
     (documentation . ,(if (listp supported-langs) (car supported-langs)
			 (format "\
This language environment is a generic one for %s (%s)
character set which supports the following languages (not all of them may
use this character set by default):
%s
Each also has its own specific language environment."
				 nice-charset-1 nice-charset-2
				 supported-langs))))
   '("European"))
  (loop for (name locale tutorial sample-text input-method) in langenvs
    do
    (set-language-info-alist
     name
     `((charset ascii ,charset)
       (coding-system ,codesys)
       (coding-priority ,codesys)
       ,@(if locale `((locale . ,locale)))
       ,@(if tutorial `((tutorial . ,tutorial)))
       ,@(if sample-text `((sample-text . ,sample-text)))
       (input-method . ,(or input-method default-input))
       (documentation . ,(format "\
This language environment supports %s using the Latin-1 (ISO-8859-1)
character set.  Languages supported by Latin-1 are Danish, Dutch, English,
Faeroese, Finnish, French, German, Icelandic, Irish, Italian, Norwegian,
Portuguese, Spanish, and Swedish.  The various language environments for
these languages are similar to the Latin-1 environment, but typically have
their own locale specified (for subprocesses and for selection of the
correct language environment at startup), and may have their own tutorials
and/or a different input method."
			       name)))
     '("European"))
    ))

;;; european.el ends here
