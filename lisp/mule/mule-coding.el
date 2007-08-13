;;; mule-coding.el --- Coding-system functions for Mule.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; split off of mule.el.

;;; Code:

(defun set-terminal-coding-system (coding-system)
  "Set the coding system used for TTY display output."
  (interactive "zterminal-coding-system: ")
  (get-coding-system coding-system) ;; correctness check
  (setq terminal-coding-system coding-system)
  (redraw-modeline t))

(defun set-pathname-coding-system (coding-system)
  "Set the coding system used for file system path names."
  (interactive "zPathname-coding-system: ")
  (get-coding-system coding-system) ;; correctness check
  (setq pathname-coding-system coding-system))

(defun what-coding-system (start end &optional arg)
  "Show the encoding of text in the region.
With prefix arg, show all possible coding systems.
This function is meant to be called interactively;
from a Lisp program, use `detect-coding-region' instead."
  (interactive "r\nP")
  (let ((codings (detect-coding-region start end)))
    (message "%s" (if (or arg (symbolp codings)) codings (car codings)))))

(defmacro with-string-as-buffer-contents (str &rest body)
  "With the contents of the current buffer being STR, run BODY.
Returns the new contents of the buffer, as modified by BODY.
The original current buffer is restored afterwards."
  `(let ((curbuf (current-buffer))
         (tempbuf (get-buffer-create " *string-as-buffer-contents*")))
     (unwind-protect
         (progn
           (set-buffer tempbuf)
           (buffer-disable-undo (current-buffer))
           (erase-buffer)
           (insert ,str)
           ,@body
           (buffer-string))
       (erase-buffer tempbuf)
       (set-buffer curbuf))))

(defun decode-coding-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
Does not modify STR.  Returns the decoded string on successful conversion."
  (with-string-as-buffer-contents
   str (decode-coding-region (point-min) (point-max) coding-system)))

(defun encode-coding-string (str coding-system)
  "Encode the string STR using CODING-SYSTEM.
Does not modify STR.  Returns the encoded string on successful conversion."
  (with-string-as-buffer-contents
   str (encode-coding-region (point-min) (point-max) coding-system)))


;;;; Coding system accessors

(defun coding-system-mnemonic (coding-system)
  "Return the 'mnemonic property of CODING-SYSTEM."
  (coding-system-property coding-system 'mnemonic))

(defun coding-system-eol-type (coding-system)
  "Return the 'eol-type property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-type))

(defun coding-system-eol-lf (coding-system)
  "Return the 'eol-lf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-lf))

(defun coding-system-eol-crlf (coding-system)
  "Return the 'eol-crlf property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-crlf))

(defun coding-system-eol-cr (coding-system)
  "Return the 'eol-cr property of CODING-SYSTEM."
  (coding-system-property coding-system 'eol-cr))

(defun coding-system-post-read-conversion (coding-system)
  "Return the 'post-read-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'post-read-conversion))

(defun coding-system-pre-write-conversion (coding-system)
  "Return the 'pre-write-conversion property of CODING-SYSTEM."
  (coding-system-property coding-system 'pre-write-conversion))

(defun coding-system-charset (coding-system register)
  "Return the 'charset property of CODING-SYSTEM for the specified REGISTER."
  (cond ((not (integerp register))
	 (signal 'wrong-type-argument (list 'integerp register)))
	((= register 0)
	 (coding-system-property coding-system 'charset-g0))
	((= register 1)
	 (coding-system-property coding-system 'charset-g1))
	((= register 2)
	 (coding-system-property coding-system 'charset-g2))
	((= register 3)
	 (coding-system-property coding-system 'charset-g3))
	(t
	 (signal 'args-out-of-range (list register 0 3)))))

(defun coding-system-force-on-output (coding-system register)
  "Return the 'force-on-output property of CODING-SYSTEM for the specified REGISTER."
  (cond ((not (integerp register))
	 (signal 'wrong-type-argument (list 'integerp register)))
	((= register 0)
	 (coding-system-property coding-system 'force-g0-on-output))
	((= register 1)
	 (coding-system-property coding-system 'force-g1-on-output))
	((= register 2)
	 (coding-system-property coding-system 'force-g2-on-output))
	((= register 3)
	 (coding-system-property coding-system 'force-g3-on-output))
	(t
	 (signal 'args-out-of-range (list register 0 3)))))

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

(defun coding-system-use-japanese-jisx0201-roman (coding-system)
  "Return the 'use-japanese-jisx0201-roman property of CODING-SYSTEM."
  (coding-system-property coding-system 'use-japanese-jisx0201-roman))

(defun coding-system-use-japanese-jisx0208-1978 (coding-system)
  "Return the 'use-japanese-jisx0208-1978 property of CODING-SYSTEM."
  (coding-system-property coding-system 'use-japanese-jisx0208-2978))

(defun coding-system-no-iso6429 (coding-system)
  "Return the 'no-iso6429 property of CODING-SYSTEM."
  (coding-system-property coding-system 'no-iso6429))

(defun coding-system-ccl-encode (coding-system)
  "Return the CCL 'encode property of CODING-SYSTEM."
  (coding-system-property coding-system 'encode))

(defun coding-system-ccl-decode (coding-system)
  "Return the CCL 'decode property of CODING-SYSTEM."
  (coding-system-property coding-system 'decode))


;;;; Definitions of predefined coding systems

(make-coding-system
 'autodetect 'autodetect
 "Automatic conversion."
 '(mnemonic "Auto"))

(make-coding-system
 'ctext 'iso2022
 "Coding-system used in X as Compound Text Encoding."
 '(charset-g0 ascii
   charset-g1 latin-1
   eol-type lf
   mnemonic "CText"
   ))

(make-coding-system
 'iso-2022-ss2-8 'iso2022
 "ISO-2022 coding system using SS2 for 96-charset in 8-bit code."
 '(charset-g0 ascii
   charset-g1 latin-1
   charset-g2 t ;; unspecified but can be used later.
   short t
   mnemonic "ISO8/SS"
   ))

(make-coding-system
 'iso-2022-ss2-7 'iso2022
 "ISO-2022 coding system using SS2 for 96-charset in 7-bit code."
 '(charset-g0 ascii
   charset-g2 t ;; unspecified but can be used later.
   seven t
   short t
   mnemonic "ISO7/SS"
   ))

(make-coding-system
 'iso-2022-7 'iso2022
 "ISO-2022 seven-bit coding system.  No single-shift or locking-shift."
 '(charset-g0 ascii
   seven t
   short t
   mnemonic "ISO7"
   ))

(make-coding-system
 'iso-2022-8 'iso2022
 "ISO-2022 eight-bit coding system.  No single-shift or locking-shift."
 '(charset-g0 ascii
   charset-g1 latin-1
   short t
   mnemonic "ISO8"
   ))

(make-coding-system
 'escape-quoted 'iso2022
 "ISO-2022 eight-bit coding system with escape quoting; used for .ELC files."
 '(charset-g0 ascii
   charset-g1 latin-1
   eol-type lf
   escape-quoted t
   mnemonic "ESC/Quot"
   ))

(make-coding-system
 'iso-2022-lock 'iso2022
 "ISO-2022 coding system using Locking-Shift for 96-charset."
 '(charset-g0 ascii
   charset-g1 t ;; unspecified but can be used later.
   seven t
   lock-shift t
   mnemonic "ISO7/Lock"
   ))

;; initialize the coding categories to something semi-reasonable
;; so that the remaining Lisp files can contain extended characters.
;; (They will be in ISO-7 format)

(set-coding-priority-list '(iso-8-2 shift-jis iso-8-designate iso-8-1 big5
			    iso-7 iso-lock-shift no-conversion))

(set-coding-category-system 'iso-7 'iso-2022-7)
(set-coding-category-system 'iso-8-designate 'ctext)
(set-coding-category-system 'iso-8-1 'ctext)
(set-coding-category-system 'iso-lock-shift 'iso-2022-lock)
(set-coding-category-system 'no-conversion 'no-conversion)
