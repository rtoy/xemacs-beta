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

