;;; iso-with-esc.el --
;;; Provision of the hateful and never widely implemented Latin, Greek and
;;; Cyrillic variable-length ISO 2022 coding systems that passed for Latin
;;; 2, Latin 10, (etc) support in XEmacs for so long.
;;                      
;; Copyright (C) 2006 Free Software Foundation

;; Author: Aidan Kehoe

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; It is not particularly reasonable that iso-latin-1-with-esc has a
;; value of t for the safe-charsets property. We discourage its use,
;; though, and this behaviour is compatible with GNU.

;;;###autoload
(define-coding-system-alias 'iso-latin-1-with-esc 'iso-2022-8)

;;;###autoload
(make-coding-system
 'iso-latin-2-with-esc 'iso2022 "ISO-8859-2 (Latin-2)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii latin-iso8859-2)
   mnemonic "MIME/Ltn-2"))

;;;###autoload
(make-coding-system
 'iso-latin-3-with-esc 'iso2022 "ISO-8859-3 (Latin-3)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii latin-iso8859-3)
   mnemonic "MIME/Ltn-3"))

;;;###autoload
(make-coding-system
 'iso-latin-4-with-esc 'iso2022 "ISO-8859-4 (Latin-4)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii latin-iso8859-4)
   mnemonic "MIME/Ltn-4"))

;;;###autoload
(make-coding-system
 'iso-latin-9-with-esc 'iso2022
  "ISO 4873 conforming 8-bit code (ASCII + Latin 9; aka Latin-1 with Euro)"
  '(mnemonic "MIME/Ltn-9"		; bletch
    safe-charsets (ascii latin-iso8859-15)
    eol-type nil
    charset-g0 ascii
    charset-g1 latin-iso8859-15
    charset-g2 t
    charset-g3 t))

;;;###autoload
(make-coding-system
 'iso-latin-5-with-esc 'iso2022 "ISO-8859-9 (Latin-5)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-9
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii latin-iso8859-9)
   mnemonic "MIME/Ltn-5"))

;;;###autoload
(make-coding-system
 'cyrillic-iso-8bit-with-esc 'iso2022
 "ISO-8859-5 (Cyrillic)"
 '(charset-g0 ascii
   charset-g1 cyrillic-iso8859-5
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii cyrillic-iso8859-5)
   mnemonic "ISO8/Cyr"))

;;;###autoload
(make-coding-system
 'hebrew-iso-8bit-with-esc 'iso2022
 "ISO-8859-8 (Hebrew)"
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
  charset-g3 t
   no-iso6429 t
   safe-charsets (ascii hebrew-iso8859-8)
   mnemonic "MIME/Hbrw"))

;;;###autoload
(make-coding-system
 'greek-iso-8bit-with-esc 'iso2022 "MIME ISO-8859-7"
 '(charset-g0 ascii
   charset-g1 greek-iso8859-7
   charset-g2 t
   charset-g3 t
   safe-charsets (ascii greek-iso8859-7)
   mnemonic "Grk"))

;; ISO 8859-6 is such a useless character set that it seems a waste of
;; codespace to dump it. Let me count the ways: 
;; 
;; 1. It doesn't support Persian or Urdu, let alone Sinhalese, despite
;;    plenty of unallocated code points.
;;
;; 2. It doesn't encode all the vowel diacritics (the Harakaat) despite that
;;    they are necessary, even for the Arabs, for basic things like
;;    dictionary entries, children's books, and occasional disambiguation.
;;
;; 3. The Arabs don't use it, they use Windows-1256, which also supports
;;    Persian, at least, as well as the French characters necessary in
;;    Lebanon and North Africa.

(make-charset
 'arabic-iso8859-6 
 "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"
 '(dimension 1
   registry "ISO8859-6"
   chars 96
   columns 1
   direction r2l
   final ?G
   graphic 1
   short-name "RHP of ISO8859/6"
   long-name "RHP of Arabic (ISO 8859-6): ISO-IR-127"))

(loop
  for (iso8859-6 unicode)
  in '((#xA0 #x00A0) ;; NO-BREAK SPACE
       (#xA4 #x00A4) ;; CURRENCY SIGN
       (#xAC #x060C) ;; ARABIC COMMA
       (#xAD #x00AD) ;; SOFT HYPHEN
       (#xBB #x061B) ;; ARABIC SEMICOLON
       (#xBF #x061F) ;; ARABIC QUESTION MARK
       (#xC1 #x0621) ;; ARABIC LETTER HAMZA
       (#xC2 #x0622) ;; ARABIC LETTER ALEF WITH MADDA ABOVE
       (#xC3 #x0623) ;; ARABIC LETTER ALEF WITH HAMZA ABOVE
       (#xC4 #x0624) ;; ARABIC LETTER WAW WITH HAMZA ABOVE
       (#xC5 #x0625) ;; ARABIC LETTER ALEF WITH HAMZA BELOW
       (#xC6 #x0626) ;; ARABIC LETTER YEH WITH HAMZA ABOVE
       (#xC7 #x0627) ;; ARABIC LETTER ALEF
       (#xC8 #x0628) ;; ARABIC LETTER BEH
       (#xC9 #x0629) ;; ARABIC LETTER TEH MARBUTA
       (#xCA #x062A) ;; ARABIC LETTER TEH
       (#xCB #x062B) ;; ARABIC LETTER THEH
       (#xCC #x062C) ;; ARABIC LETTER JEEM
       (#xCD #x062D) ;; ARABIC LETTER HAH
       (#xCE #x062E) ;; ARABIC LETTER KHAH
       (#xCF #x062F) ;; ARABIC LETTER DAL
       (#xD0 #x0630) ;; ARABIC LETTER THAL
       (#xD1 #x0631) ;; ARABIC LETTER REH
       (#xD2 #x0632) ;; ARABIC LETTER ZAIN
       (#xD3 #x0633) ;; ARABIC LETTER SEEN
       (#xD4 #x0634) ;; ARABIC LETTER SHEEN
       (#xD5 #x0635) ;; ARABIC LETTER SAD
       (#xD6 #x0636) ;; ARABIC LETTER DAD
       (#xD7 #x0637) ;; ARABIC LETTER TAH
       (#xD8 #x0638) ;; ARABIC LETTER ZAH
       (#xD9 #x0639) ;; ARABIC LETTER AIN
       (#xDA #x063A) ;; ARABIC LETTER GHAIN
       (#xE0 #x0640) ;; ARABIC TATWEEL
       (#xE1 #x0641) ;; ARABIC LETTER FEH
       (#xE2 #x0642) ;; ARABIC LETTER QAF
       (#xE3 #x0643) ;; ARABIC LETTER KAF
       (#xE4 #x0644) ;; ARABIC LETTER LAM
       (#xE5 #x0645) ;; ARABIC LETTER MEEM
       (#xE6 #x0646) ;; ARABIC LETTER NOON
       (#xE7 #x0647) ;; ARABIC LETTER HEH
       (#xE8 #x0648) ;; ARABIC LETTER WAW
       (#xE9 #x0649) ;; ARABIC LETTER ALEF MAKSURA
       (#xEA #x064A) ;; ARABIC LETTER YEH
       (#xEB #x064B) ;; ARABIC FATHATAN
       (#xEC #x064C) ;; ARABIC DAMMATAN
       (#xED #x064D) ;; ARABIC KASRATAN
       (#xEE #x064E) ;; ARABIC FATHA
       (#xEF #x064F) ;; ARABIC DAMMA
       (#xF0 #x0650) ;; ARABIC KASRA
       (#xF1 #x0651) ;; ARABIC SHADDA
       (#xF2 #x0652));; ARABIC SUKUN
  do (set-unicode-conversion (make-char 'arabic-iso8859-6 iso8859-6)
                             unicode))

;;;###autoload
(make-coding-system
 'arabic-iso-8bit-with-esc 'iso2022  ;; GNU's iso-8859-6 is
                                     ;; iso2022-compatible.
 "ISO-8859-6 (Arabic)"
 '(charset-g0 ascii
   charset-g1 arabic-iso8859-6
   charset-g2 t
   charset-g3 t
   no-iso6429 t
   safe-charsets (ascii arabic-iso8859-6)
   mnemonic "MIME/Arbc"))

