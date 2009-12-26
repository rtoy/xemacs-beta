;;; cyrillic.el --- Support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2002, 2005 Ben Wing.

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

;; Cyrillic syntax
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xad) ".")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xf0) ".")
(modify-syntax-entry (make-char 'cyrillic-iso8859-5 #xfd) ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ISO-8859-5

(make-coding-system
 'iso-8859-5 'iso2022
 "ISO-8859-5 (Cyrillic)"
 '(charset-g0 ascii
   charset-g1 cyrillic-iso8859-5
   charset-g2 t
   charset-g3 t
   mnemonic "ISO8/Cyr"
   ))

(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
		  (tutorial . "TUTORIAL.ru")
		  (coding-system iso-8859-5)
		  (coding-priority iso-8859-5)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8

(make-coding-system
 'koi8-r 'mbcs
 "KOI8-R (Cyrillic)"
 '(charsets (ascii cyrillic-koi8-r)
   mnemonic "KOI8"))

;; (define-coding-system-alias 'koi8-r 'cyrillic-koi8)
;; (define-coding-system-alias 'koi8 'cyrillic-koi8)

;; `iso-8-1' is not correct, but XEmacs doesn't have an `mbcs' category
(coding-system-put 'koi8-r 'category 'iso-8-1)

(set-language-info-alist
 "Cyrillic-KOI8" '((charset cyrillic-iso8859-5)
		   (coding-system koi8-r)
		   (coding-priority koi8-r)
		   (input-method . "cyrillic-yawerty")
		   (features cyril-util)
		   (locale "ru")
		   (mswindows-locale . "RUSSIAN")
		   (tutorial . "TUTORIAL.ru")
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
 '("Cyrillic"))

;; Windows-1251 support in windows.el.

;;; ALTERNATIVNYJ

;; (define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

(make-coding-system
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

;;; cyrillic.el ends here
