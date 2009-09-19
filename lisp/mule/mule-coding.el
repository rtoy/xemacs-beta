;;; mule-coding.el --- Coding-system functions for Mule. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001 Ben Wing.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; split off of mule.el and mostly moved to coding.el

;;; Code:

(defun coding-system-force-on-output (coding-system register)
  "Return the 'force-on-output property of CODING-SYSTEM for the specified REGISTER."
  (check-type register integer)
  (coding-system-property
   coding-system
   (case register
     (0 'force-g0-on-output)
     (1 'force-g1-on-output)
     (2 'force-g2-on-output)
     (3 'force-g3-on-output)
     (t (signal 'args-out-of-range (list register 0 3))))))

(defun coding-system-short (coding-system)
  "Return the 'short property of CODING-SYSTEM."
  (coding-system-property coding-system 'short))

(defun coding-system-no-ascii-eol (coding-system)
  "Return the 'no-ascii-eol property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-ascii-eol))

(defun coding-system-no-ascii-cntl (coding-system)
  "Return the 'no-ascii-cntl property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-ascii-cntl))

(defun coding-system-seven (coding-system)
  "Return the 'seven property of CODING-SYSTEM."
  (coding-system-property coding-system 'seven))

(defun coding-system-lock-shift (coding-system)
  "Return the 'lock-shift property of CODING-SYSTEM."
  (coding-system-property coding-system 'lock-shift))

;;(defun coding-system-use-japanese-jisx0201-roman (coding-system)
;;  "Return the 'use-japanese-jisx0201-roman property of CODING-SYSTEM."
;;  (coding-system-property coding-system 'use-japanese-jisx0201-roman))

;;(defun coding-system-use-japanese-jisx0208-1978 (coding-system)
;;  "Return the 'use-japanese-jisx0208-1978 property of CODING-SYSTEM."
;;  (coding-system-property coding-system 'use-japanese-jisx0208-2978))

(defun coding-system-no-iso6429 (coding-system)
  "Return the 'no-iso6429 property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-iso6429))

(defun coding-system-ccl-encode (coding-system)
  "Return the CCL 'encode property of CODING-SYSTEM."
  (coding-system-property coding-system 'encode))

(defun coding-system-ccl-decode (coding-system)
  "Return the CCL 'decode property of CODING-SYSTEM."
  (coding-system-property coding-system 'decode))

(defun coding-system-iso2022-charset (coding-system register)
"Return the charset initially designated to REGISTER in CODING-SYSTEM.
The allowable range of REGISTER is 0 through 3."
  (if (or (< register 0) (> register 3))
      (error 'args-out-of-range "coding-system-charset REGISTER" register 0 3))
  (coding-system-property coding-system (nth register '(charset-g0
							charset-g1
							charset-g2
							charset-g3))))


;;;; Definitions of predefined coding systems

(make-coding-system
 'ctext 'iso2022
 "Compound Text"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   eol-type nil
   safe-charsets t ;; Reasonable
   mnemonic "CText"))

(make-coding-system
 'iso-2022-8bit-ss2 'iso2022
 "ISO-2022 8-bit w/SS2"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   charset-g2 t ;; unspecified but can be used later.
   short t
   safe-charsets (ascii katakana-jisx0201 japanese-jisx0208-1978
                  japanese-jisx0208 japanese-jisx0212 japanese-jisx0213-1
                  japanese-jisx0213-2)
   mnemonic "ISO8/SS"
   documentation "ISO 2022 based 8-bit encoding using SS2 for 96-charset"
   ))

(make-coding-system
 'iso-2022-7bit-ss2 'iso2022
 "ISO-2022 7-bit w/SS2"
 '(charset-g0 ascii
   charset-g2 t ;; unspecified but can be used later.
   seven t
   short t
   safe-charsets t
   mnemonic "ISO7/SS"
   documentation "ISO 2022 based 7-bit encoding using SS2 for 96-charset"
   eol-type nil))

;; (copy-coding-system 'iso-2022-7bit-ss2 'iso-2022-jp-2)
(make-coding-system
 'iso-2022-jp-2 'iso2022
 "ISO-2022-JP-2"
 '(charset-g0 ascii
   charset-g2 t ;; unspecified but can be used later.
   seven t
   short t
   safe-charsets t
   mnemonic "ISO7/SS"
   eol-type nil))

(make-coding-system
 'iso-2022-7bit 'iso2022
 "ISO 2022 7-bit"
 '(charset-g0 ascii
   seven t
   short t
   safe-charsets t
   mnemonic "ISO7"
   documentation "ISO-2022-based 7-bit encoding using only G0"
   ))

;; compatibility for old XEmacsen
(define-coding-system-alias 'iso-2022-7 'iso-2022-7bit)

(make-coding-system
 'iso-2022-8 'iso2022
 "ISO-2022 8-bit"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   short t
   safe-charsets t
   mnemonic "ISO8"
   documentation "ISO-2022 eight-bit coding system.  No single-shift or locking-shift."
   ))

(make-coding-system
 'escape-quoted 'iso2022
 "Escape-Quoted (for .ELC files)"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   eol-type lf
   escape-quoted t
   safe-charsets t
   mnemonic "ESC/Quot"
   documentation "ISO-2022 eight-bit coding system with escape quoting; used for .ELC files."
   ))

(make-coding-system
 'iso-2022-lock 'iso2022
 "ISO-2022 w/locking-shift"
 '(charset-g0 ascii
   charset-g1 t ;; unspecified but can be used later.
   seven t
   lock-shift t
   safe-charsets t
   mnemonic "ISO7/Lock"
   documentation "ISO-2022 coding system using Locking-Shift for 96-charset."
   ))


;; This is used by people writing CCL programs, but is called at runtime.
(defun define-translation-hash-table (symbol table)
  "Define SYMBOL as the name of the hash translation TABLE for use in CCL.

Analogous to `define-translation-table', but updates
`translation-hash-table-vector' and the table is for use in the CCL
`lookup-integer' and `lookup-character' functions."
  (check-argument-type #'symbolp symbol)
  (check-argument-type #'hash-table-p table)
  (let ((len (length translation-hash-table-vector))
	(id 0)
	done)
    (put symbol 'translation-hash-table table)
    (while (not done)
      (if (>= id len)
	  (setq translation-hash-table-vector
		(vconcat translation-hash-table-vector [nil])))
      (let ((slot (aref translation-hash-table-vector id)))
	(if (or (not slot)
		(eq (car slot) symbol))
	    (progn
	      (aset translation-hash-table-vector id (cons symbol table))
	      (setq done t))
	  (setq id (1+ id)))))
    (put symbol 'translation-hash-table-id id)
    id))

;; Ideally this would be in latin.el, but code-init.el uses it.
(make-coding-system
 'iso-8859-1 
 'fixed-width
 "ISO-8859-1 (Latin-1)"
 (eval-when-compile
   `(unicode-map 
     ,(loop
        for i from #x80 to #xff
        collect (list i (int-char i))) ;; Identical to Latin-1.
     mnemonic "Latin 1"
     documentation "The most used encoding of Western Europe and the Americas."
     aliases (iso-latin-1 latin-1))))
