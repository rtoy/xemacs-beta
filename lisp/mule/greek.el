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

; (make-charset 'greek-iso8859-7 
; 	      "Right-Hand Part of Latin/Greek Alphabet (ISO/IEC 8859-7): ISO-IR-126"
; 	      '(dimension
; 		1
; 		registry "ISO8859-7"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?F
; 		graphic 1
; 		short-name "RHP of ISO8859/7"
; 		long-name "RHP of Greek (ISO 8859-7): ISO-IR-126"
; 		))

;; For syntax of Greek
(loop for c from 54 to 126
      do (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w"))
(modify-syntax-entry (make-char 'greek-iso8859-7 32) "w") ; no-break space
(modify-syntax-entry ?,F7(B ".")
(modify-syntax-entry ?,F;(B ".")
(modify-syntax-entry ?,F=(B ".")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GREEK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (make-coding-system
;;  'greek-iso-8bit 2 ?7
;;  "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)"
;;  '(ascii greek-iso8859-7 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii greek-iso8859-7)
;;    (mime-charset . iso-8859-7)))

;; (define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(make-coding-system
 'iso-8859-7 'iso2022 "ISO-8859-7 (Greek)"
 '(charset-g0 ascii
   charset-g1 greek-iso8859-7
   charset-g2 t
   charset-g3 t
   mnemonic "Grk"
   ))

(set-language-info-alist
 "Greek" '((charset greek-iso8859-7)
	   (coding-system iso-8859-7)
	   (coding-priority iso-8859-7)
	   (locale "el_GR.iso88597" "el_GR.greek8" "el_GR" "greek" "el")
	   (input-method . "greek")
	   (sample-text . "Greek (,FGkk]mija(B)	,FCei\(B ,Fsar(B")
	   (documentation . t)))

;;; greek.el ends here
