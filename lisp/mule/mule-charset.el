;;; mule-charset.el --- Charset functions for Mule. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1992, 2001 Free Software Foundation, Inc.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.
;; Copyright (C) 2002 Ben Wing.

;; Author: Unknown
;; Keywords: i18n, mule, internal

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

;;; Synched up with: Not synched.  API at source level synched with FSF 20.3.9.

;;; Commentary:

;; These functions are not compatible at the bytecode level with Emacs/Mule,
;; and they never will be.  -sb [1999-05-26]

;;; Code:

;;;; Classifying text according to charsets

;; the old version was broken in a couple of ways
;; this is one of several versions, I tried a hash as well as the
;; `prev-charset' cache used in the old version, but this was definitely
;; faster than the hash version and marginally faster than the prev-charset
;; version
;; #### this really needs to be moved into C
(defun charsets-in-region (start end &optional buffer)
  "Return a list of the charsets in the region between START and END.
BUFFER defaults to the current buffer if omitted."
  (let (list)
    (save-excursion
      (if buffer
	  (set-buffer buffer))
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (not (eobp))
	  ;; the first test will usually succeed on testing the
	  ;; car of the list; don't waste time let-binding.
	  (or (memq (char-charset (char-after (point))) list)
	      (setq list (cons (char-charset (char-after (point))) list)))
	  (forward-char))))
    list))

(defun charsets-in-string (string)
  "Return a list of the charsets in STRING."
  (let (list)
    (mapc (lambda (ch)
	    ;; the first test will usually succeed on testing the
	    ;; car of the list; don't waste time let-binding.
	    (or (memq (char-charset ch) list)
		(setq list (cons (char-charset ch) list))))
	  string)
    list))

(defalias 'find-charset-string 'charsets-in-string)
(defalias 'find-charset-region 'charsets-in-region)


;;;; Charset accessors

(defun charset-iso-graphic-plane (charset)
  "Return the `graphic' property of CHARSET.
See `make-charset'."
  (charset-property charset 'graphic))

(defun charset-iso-final-char (charset)
  "Return the final byte of the ISO 2022 escape sequence designating CHARSET."
  (charset-property charset 'final))

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (charset-property charset 'chars))

(defun charset-width (charset)
  "Return the number of display columns per character of CHARSET.
This only applies to TTY mode (under X, the actual display width can
be automatically determined)."
  (charset-property charset 'columns))

;; #### FSFmacs returns 0
(defun charset-direction (charset)
  "Return the display direction (0 for `l2r' or 1 for `r2l') of CHARSET.
Only left-to-right is currently implemented."
  (if (eq (charset-property charset 'direction) 'l2r)
      0
    1))

;; Not in Emacs/Mule
(defun charset-registry (charset)
  "Return the registry of CHARSET.
This is a regular expression matching the registry field of fonts
that can display the characters in CHARSET."
  (charset-property charset 'registry))

(defun charset-ccl-program (charset)
  "Return the CCL program of CHARSET.
See `make-charset'."
  (charset-property charset 'ccl-program))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(define-obsolete-function-alias 'charset-columns 'charset-width) ;; 19990409
(define-obsolete-function-alias 'charset-final 'charset-iso-final-char) ;; 19990409
(define-obsolete-function-alias 'charset-graphic 'charset-iso-graphic-plane) ;; 19990409
(define-obsolete-function-alias 'charset-doc-string 'charset-description) ;; 19990409

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
(defsetf charset-ccl-program set-charset-ccl-program)

;;; FSF compatibility functions
(defun charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil."
  (when (null pos)
    (setq pos (point)))
  (check-argument-type 'integerp pos)
  (unless (or (< pos (point-min))
	      (> pos (point-max)))
    (char-charset (char-after pos))))

;; Yuck!
;; We're not going to support these.
;(defun charset-info (charset) [incredibly broken function with random vectors]
;(defun define-charset (...) [incredibly broken function with random vectors]

;;; Charset property

(defalias 'get-charset-property 'get)
(defalias 'put-charset-property 'put)
(defalias 'charset-plist 'object-plist)
(defalias 'set-charset-plist 'setplist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                          translation tables                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (translation-table (:constructor internal-make-translation-table))
  forward
  reverse)

(defun make-translation-table (&rest args)
  "Make a translation table from arguments.
A translation table is a char table intended for for character
translation in CCL programs.

Each argument is a list of elemnts of the form (FROM . TO), where FROM
is a character to be translated to TO.

FROM can be a generic character (see `make-char').  In this case, TO is
a generic character containing the same number of characters, or a
ordinary character.  If FROM and TO are both generic characters, all
characters belonging to FROM are translated to characters belonging to TO
without changing their position code(s).

The arguments and forms in each argument are processed in the given
order, and if a previous form already translates TO to some other
character, say TO-ALT, FROM is also translated to TO-ALT."
  (let ((table (internal-make-translation-table
		:forward (make-char-table 'generic)))
	revlist)
    (while args
      (let ((elts (car args)))
	(while elts
	  (let* ((from (car (car elts)))
		 (from-i 0)		; degree of freedom of FROM
		 (from-rev (nreverse (split-char from)))
		 (to (cdr (car elts)))
		 (to-i 0)		; degree of freedom of TO
		 (to-rev (nreverse (split-char to))))
	    ;; Check numbers of heading 0s in FROM-REV and TO-REV.
	    (while (eq (car from-rev) 0)
	      (setq from-i (1+ from-i) from-rev (cdr from-rev)))
	    (while (eq (car to-rev) 0)
	      (setq to-i (1+ to-i) to-rev (cdr to-rev)))
	    (if (and (/= from-i to-i) (/= to-i 0))
		(error "Invalid character pair (%d . %d)" from to))
	    ;; If we have already translated TO to TO-ALT, FROM should
	    ;; also be translated to TO-ALT.  But, this is only if TO
	    ;; is a generic character or TO-ALT is not a generic
	    ;; character.
	    (let ((to-alt (get-char-table to table)))
	      (if (and to-alt
		       (or (> to-i 0) (not (find-charset to-alt))))
		  (setq to to-alt)))
	    (if (> from-i 0)
		(set-char-table-default table from to)
	      (put-char-table from to table))
	    ;; If we have already translated some chars to FROM, they
	    ;; should also be translated to TO.
	    (let ((l (assq from revlist)))
	      (if l
		  (let ((ch (car l)))
		    (setcar l to)
		    (setq l (cdr l))
		    (while l
		      (put-char-table ch to table)
		      (setq l (cdr l)) ))))
	    ;; Now update REVLIST.
	    (let ((l (assq to revlist)))
	      (if l
		  (setcdr l (cons from (cdr l)))
		(setq revlist (cons (list to from) revlist)))))
	  (setq elts (cdr elts))))
      (setq args (cdr args)))
    ;; Return TABLE just created.
    table))

;; Do we really need this?
; (defun make-translation-table-from-vector (vec)
;   "Make translation table from decoding vector VEC.
; VEC is an array of 256 elements to map unibyte codes to multibyte characters.
; See also the variable `nonascii-translation-table'."
;   (let ((table (make-char-table 'translation-table))
; 	(rev-table (make-char-table 'translation-table))
; 	(i 0)
; 	ch)
;     (while (< i 256)
;       (setq ch (aref vec i))
;       (aset table i ch)
;       (if (>= ch 256)
; 	  (aset rev-table ch i))
;       (setq i (1+ i)))
;     (set-char-table-extra-slot table 0 rev-table)
;     table))

(defvar named-translation-table-hash-table (make-hash-table))

(defun define-translation-table (symbol &rest args)
  "Define SYMBOL as the name of translation table made by ARGS.
This sets up information so that the table can be used for
translations in a CCL program.

If the first element of ARGS is a translation table, just define SYMBOL to
name it.  (Note that this function does not bind SYMBOL.)

Any other ARGS should be suitable as arguments of the function
`make-translation-table' (which see).

Look up a named translation table using `find-translation-table' or
`get-translation-table'."
  (let ((table (if (translation-table-p (car args))
		   (car args)
		 (apply 'make-translation-table args))))
    (puthash symbol table named-translation-table-hash-table)))

(defun find-translation-table (table-or-name)
  "Retrieve the translation table of the given name.
If TABLE-OR-NAME is a translation table object, it is simply returned.
Otherwise, TABLE-OR-NAME should be a symbol.  If there is no such
translation table, nil is returned.  Otherwise the associated translation
table object is returned."
  (if (translation-table-p table-or-name)
      table-or-name
    (check-argument-type 'symbolp table-or-name)
    (gethash table-or-name named-translation-table-hash-table)))

(defun get-translation-table (table-or-name)
  "Retrieve the translation table of the given name.
Same as `find-translation-table' except an error is signalled if there is
no such translation table instead of returning nil."
  (or (find-translation-table table-or-name)
      (error 'invalid-argument "No such translation table" table-or-name)))


;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.
(let ((l '(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312 chinese-big5-1 chinese-big5-2)))
  (while l
    (put-char-table (car l) t auto-fill-chars)
    (setq l (cdr l))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                charsets                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Synched up with: FSF 21.1.

;; All FSF charset definitions are in mule-conf.el.  I copied the relevant
;; part of that file below, then converted all charset definitions using
;; the macro below, then globally replaced 'direction 0' with 'direction
;; l2r' and 'direction 1' with 'direction r2l', then commented everything
;; out.  Copy the definitions as necessary to individual files.

;; Kbd macro to convert from FSF-style define-charset to our make-charset.

; (setq last-kbd-macro (read-kbd-macro
; "<right> M-d make <M-right> M-d <home> <down> TAB '[dimension DEL SPC <M-right> RET TAB chars SPC <M-right> RET TAB columns SPC <M-right> RET TAB direction SPC <M-right> RET TAB final SPC <M-right> RET TAB graphic SPC <M-right> RET TAB short- name SPC <M-right> RET TAB long- name SPC <M-right> RET TAB <S-M-right> <f2> DEL TAB <end> ] <M-left> <end> SPC <f4> 3*<M-left> <left> <M-right> RET <down>"))

;; Kbd macro to take one registry entry from the list of registry entries,
;; find the appropriate make-charset call, and add the appropriate registry
;; property.

; (setq last-kbd-macro (read-kbd-macro
; "3*<right> <S-M-right> C-x x 1 <right> <S-M-right> C-x x 2 <home> C-x r m foo RET <M-down> M-x sear TAB for TAB RET C-x g 1 RET C-s dimen RET <end> RET TAB 3*<backspace> registry SPC C-x g 2 C-x r b RET <down>"))

;; List from FSF international/fontset.el of registries for charsets.

;; latin-iso8859-1 "ISO8859-1"
;; latin-iso8859-2 "ISO8859-2"
;; latin-iso8859-3 "ISO8859-3"
;; latin-iso8859-4 "ISO8859-4"
;; thai-tis620 "TIS620"
;; greek-iso8859-7 "ISO8859-7"
;; arabic-iso8859-6 "ISO8859-6"
;; hebrew-iso8859-8 "ISO8859-8"
;; katakana-jisx0201 "JISX0201"
;; latin-jisx0201 "JISX0201"
;; cyrillic-iso8859-5 "ISO8859-5"
;; latin-iso8859-9 "ISO8859-9"
;; japanese-jisx0208-1978 "JISX0208.1978"
;; chinese-gb2312 "GB2312.1980"
;; japanese-jisx0208 "JISX0208.1990"
;; korean-ksc5601 "KSC5601.1989"
;; japanese-jisx0212 "JISX0212"
;; chinese-cns11643-1 "CNS11643.1992-1"
;; chinese-cns11643-2 "CNS11643.1992-2"
;; chinese-cns11643-3 "CNS11643.1992-3"
;; chinese-cns11643-4 "CNS11643.1992-4"
;; chinese-cns11643-5 "CNS11643.1992-5"
;; chinese-cns11643-6 "CNS11643.1992-6"
;; chinese-cns11643-7 "CNS11643.1992-7"
;; chinese-big5-1 "Big5"
;; chinese-big5-2 "Big5"
;; chinese-sisheng "sisheng_cwnn"
;; vietnamese-viscii-lower "VISCII1.1"
;; vietnamese-viscii-upper "VISCII1.1"
;; arabic-digit "MuleArabic-0"
;; arabic-1-column "MuleArabic-1"
;; arabic-2-column "MuleArabic-2"
;; ipa "MuleIPA"
;; ethiopic "Ethiopic-Unicode"
;; ascii-right-to-left "ISO8859-1"
;; indian-is13194 "IS13194-Devanagari"
;; indian-2-column "MuleIndian-2"
;; indian-1-column "MuleIndian-1"
;; lao "MuleLao-1"
;; tibetan "MuleTibetan-2"
;; tibetan-1-column "MuleTibetan-1"
;; latin-iso8859-14 "ISO8859-14"
;; latin-iso8859-15 "ISO8859-15"
;; mule-unicode-0100-24ff "ISO10646-1"
;; mule-unicode-2500-33ff "ISO10646-1"
;; mule-unicode-e000-ffff "ISO10646-1"
;; japanese-jisx0213-1 "JISX0213.2000-1"
;; japanese-jisx0213-2 "JISX0213.2000-2"

;;; Begin stuff from international/mule-conf.el.

; ;;; Definitions of character sets.

; ;; Basic (official) character sets.  These character sets are treated
; ;; efficiently with respect to buffer memory.

; ;; Syntax:
; ;; (define-charset CHARSET-ID CHARSET
; ;;   [ DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
; ;;     SHORT-NAME LONG-NAME DESCRIPTION ])
; ;; ASCII charset is defined in src/charset.c as below.
; ;; (define-charset 0 ascii
; ;;    [1 94 1 0 ?B 0 "ASCII" "ASCII" "ASCII (ISO646 IRV)"])

; ;; 1-byte charsets.  Valid range of CHARSET-ID is 128..143.

; ;; CHARSET-ID 128 is not used.

; ; An extra level of commenting means an official (done in C) charset.
; ; (make-charset 'latin-iso8859-1 
; ; 	      "Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-1"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?A
; ; 		graphic 1
; ; 		short-name "RHP of Latin-1"
; ; 		long-name "RHP of Latin-1 (ISO 8859-1): ISO-IR-100"
; ; 		))

; ; (make-charset 'latin-iso8859-2 
; ; 	      "Right-Hand Part of Latin Alphabet 2 (ISO/IEC 8859-2): ISO-IR-101"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-2"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?B
; ; 		graphic 1
; ; 		short-name "RHP of Latin-2"
; ; 		long-name "RHP of Latin-2 (ISO 8859-2): ISO-IR-101"
; ; 		))

; ; (make-charset 'latin-iso8859-3 
; ; 	      "Right-Hand Part of Latin Alphabet 3 (ISO/IEC 8859-3): ISO-IR-109"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-3"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?C
; ; 		graphic 1
; ; 		short-name "RHP of Latin-3"
; ; 		long-name "RHP of Latin-3 (ISO 8859-3): ISO-IR-109"
; ; 		))

; ; (make-charset 'latin-iso8859-4 
; ; 	      "Right-Hand Part of Latin Alphabet 4 (ISO/IEC 8859-4): ISO-IR-110"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-4"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?D
; ; 		graphic 1
; ; 		short-name "RHP of Latin-4"
; ; 		long-name "RHP of Latin-4 (ISO 8859-4): ISO-IR-110"
; ; 		))

; ; (make-charset 'thai-tis620 
; ; 	      "Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166"
; ; 	      '(dimension
; ; 		1
; ; 		registry "TIS620"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?T
; ; 		graphic 1
; ; 		short-name "RHP of TIS620"
; ; 		long-name "RHP of Thai (TIS620): ISO-IR-166"
; ; 		))

; ; (make-charset 'greek-iso8859-7 
; ; 	      "Right-Hand Part of Latin/Greek Alphabet (ISO/IEC 8859-7): ISO-IR-126"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-7"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?F
; ; 		graphic 1
; ; 		short-name "RHP of ISO8859/7"
; ; 		long-name "RHP of Greek (ISO 8859-7): ISO-IR-126"
; ; 		))

; ; (make-charset 'arabic-iso8859-6 
; ; 	      "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-6"
; ; 		chars 96
; ; 		columns 1
; ; 		direction r2l
; ; 		final ?G
; ; 		graphic 1
; ; 		short-name "RHP of ISO8859/6"
; ; 		long-name "RHP of Arabic (ISO 8859-6): ISO-IR-127"
; ; 		))

; ; (make-charset 'hebrew-iso8859-8 
; ; 	      "Right-Hand Part of Latin/Hebrew Alphabet (ISO/IEC 8859-8): ISO-IR-138"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-8"
; ; 		chars 96
; ; 		columns 1
; ; 		direction r2l
; ; 		final ?H
; ; 		graphic 1
; ; 		short-name "RHP of ISO8859/8"
; ; 		long-name "RHP of Hebrew (ISO 8859-8): ISO-IR-138"
; ; 		))

; ; (make-charset 'katakana-jisx0201 
; ; 	      "Katakana Part of JISX0201.1976"
; ; 	      '(dimension
; ; 		1
; ; 		registry "JISX0201"
; ; 		chars 94
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?I
; ; 		graphic 1
; ; 		short-name "JISX0201 Katakana"
; ; 		long-name "Japanese Katakana (JISX0201.1976)"
; ; 		))

; ; (make-charset 'latin-jisx0201 
; ; 	      "Roman Part of JISX0201.1976"
; ; 	      '(dimension
; ; 		1
; ; 		registry "JISX0201"
; ; 		chars 94
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?J
; ; 		graphic 0
; ; 		short-name "JISX0201 Roman"
; ; 		long-name "Japanese Roman (JISX0201.1976)"
; ; 		))


; ;; CHARSET-ID is not used 139.

; ; (make-charset 'cyrillic-iso8859-5 
; ; 	      "Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-5"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?L
; ; 		graphic 1
; ; 		short-name "RHP of ISO8859/5"
; ; 		long-name "RHP of Cyrillic (ISO 8859-5): ISO-IR-144"
; ; 		))

; ; (make-charset 'latin-iso8859-9 
; ; 	      "Right-Hand Part of Latin Alphabet 5 (ISO/IEC 8859-9): ISO-IR-148"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-9"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?M
; ; 		graphic 1
; ; 		short-name "RHP of Latin-5"
; ; 		long-name "RHP of Latin-5 (ISO 8859-9): ISO-IR-148"
; ; 		))

; ; (make-charset 'latin-iso8859-15 
; ; 	      "Right-Hand Part of Latin Alphabet 9 (ISO/IEC 8859-15): ISO-IR-203"
; ; 	      '(dimension
; ; 		1
; ; 		registry "ISO8859-15"
; ; 		chars 96
; ; 		columns 1
; ; 		direction l2r
; ; 		final ?b
; ; 		graphic 1
; ; 		short-name "RHP of Latin-9"
; ; 		long-name "RHP of Latin-9 (ISO 8859-15): ISO-IR-203"
; ; 		))

; (make-charset 'latin-iso8859-14 
; 	      "Right-Hand Part of Latin Alphabet 8 (ISO/IEC 8859-14)"
; 	      '(dimension
; 		1
; 		registry "ISO8859-14"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?_
; 		graphic 1
; 		short-name "RHP of Latin-8"
; 		long-name "RHP of Latin-8 (ISO 8859-14)"
; 		))


; ;; 2-byte charsets.  Valid range of CHARSET-ID is 144..153.

; ; (make-charset 'japanese-jisx0208-1978 
; ; 	      "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
; ; 	      '(dimension
; ; 		2
; ; 		registry "JISX0208.1990"
; ; 		registry "JISX0208.1978"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?@
; ; 		graphic 0
; ; 		short-name "JISX0208.1978"
; ; 		long-name "JISX0208.1978 (Japanese): ISO-IR-42"
; ; 		))

; ; (make-charset 'chinese-gb2312 
; ; 	      "GB2312 Chinese simplified: ISO-IR-58"
; ; 	      '(dimension
; ; 		2
; ; 		registry "GB2312.1980"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?A
; ; 		graphic 0
; ; 		short-name "GB2312"
; ; 		long-name "GB2312: ISO-IR-58"
; ; 		))

; ; (make-charset 'japanese-jisx0208 
; ; 	      "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
; ; 	      '(dimension
; ; 		2
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?B
; ; 		graphic 0
; ; 		short-name "JISX0208"
; ; 		long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
; ; 		))

; ; (make-charset 'korean-ksc5601 
; ; 	      "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
; ; 	      '(dimension
; ; 		2
; ; 		registry "KSC5601.1989"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?C
; ; 		graphic 0
; ; 		short-name "KSC5601"
; ; 		long-name "KSC5601 (Korean): ISO-IR-149"
; ; 		))

; ; (make-charset 'japanese-jisx0212 
; ; 	      "JISX0212 Japanese supplement: ISO-IR-159"
; ; 	      '(dimension
; ; 		2
; ; 		registry "JISX0212"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?D
; ; 		graphic 0
; ; 		short-name "JISX0212"
; ; 		long-name "JISX0212 (Japanese): ISO-IR-159"
; ; 		))

; ; (make-charset 'chinese-cns11643-1 
; ; 	      "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
; ; 	      '(dimension
; ; 		2
; ; 		registry "CNS11643.1992-1"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?G
; ; 		graphic 0
; ; 		short-name "CNS11643-1"
; ; 		long-name "CNS11643-1 (Chinese traditional): ISO-IR-171"
; ; 		))

; ; (make-charset 'chinese-cns11643-2 
; ; 	      "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
; ; 	      '(dimension
; ; 		2
; ; 		registry "CNS11643.1992-2"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?H
; ; 		graphic 0
; ; 		short-name "CNS11643-2"
; ; 		long-name "CNS11643-2 (Chinese traditional): ISO-IR-172"
; ; 		))

; (make-charset 'japanese-jisx0213-1 "JISX0213 Plane 1 (Japanese)"
; 	      '(dimension
; 		2
; 		registry "JISX0213.2000-1"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?O
; 		graphic 0
; 		short-name "JISX0213-1"
; 		long-name "JISX0213-1"
; 		))

; ; (make-charset 'chinese-big5-1 
; ; 	      "Frequently used part (A141-C67F) of Big5 (Chinese traditional)"
; ; 	      '(dimension
; ; 		2
; ; 		registry "Big5"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?0
; ; 		graphic 0
; ; 		short-name "Big5 (Level-1)"
; ; 		long-name "Big5 (Level-1) A141-C67F"
; ; 		))

; ; (make-charset 'chinese-big5-2 
; ; 	      "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
; ; 	      '(dimension
; ; 		2
; ; 		registry "Big5"
; ; 		chars 94
; ; 		columns 2
; ; 		direction l2r
; ; 		final ?1
; ; 		graphic 0
; ; 		short-name "Big5 (Level-2)"
; ; 		long-name "Big5 (Level-2) C940-FEFE"
; ; 		))


; ;; Additional (private) character sets.  These character sets are
; ;; treated less space-efficiently in the buffer.

; ;; Syntax:
; ;; (define-charset CHARSET-ID CHARSET
; ;;   [ DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
; ;;     SHORT-NAME LONG-NAME DESCRIPTION ])

; ;; ISO-2022 allows a use of character sets not registered in ISO with
; ;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
; ;; reserves `0' through `9' to support several private character sets.
; ;; The remaining final characters `:' through `?' are for users.

; ;; 1-byte 1-column charsets.  Valid range of CHARSET-ID is 160..223.

; (make-charset 'chinese-sisheng 
; 	      "SiSheng characters for PinYin/ZhuYin"
; 	      '(dimension
; 		1
; 		registry "sisheng_cwnn"
; 		chars 94
; 		columns 1
; 		direction l2r
; 		final ?0
; 		graphic 0
; 		short-name "SiSheng"
; 		long-name "SiSheng (PinYin/ZhuYin)"
; 		))


; ;; IPA characters for phonetic symbols.
; (make-charset 'ipa "IPA (International Phonetic Association)"
; 	      '(dimension
; 		1
; 		registry "MuleIPA"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?0
; 		graphic 1
; 		short-name "IPA"
; 		long-name "IPA"
; 		))


; ;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
; ;; more than 96 characters.  Since Emacs can't handle it as one
; ;; character set, it is divided into two: lower case letters and upper
; ;; case letters.
; (make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case"
; 	      '(dimension
; 		1
; 		registry "VISCII1.1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?1
; 		graphic 1
; 		short-name "VISCII lower"
; 		long-name "VISCII lower-case"
; 		))

; (make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case"
; 	      '(dimension
; 		1
; 		registry "VISCII1.1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?2
; 		graphic 1
; 		short-name "VISCII upper"
; 		long-name "VISCII upper-case"
; 		))


; ;; For Arabic, we need three different types of character sets.
; ;; Digits are of direction left-to-right and of width 1-column.
; ;; Others are of direction right-to-left and of width 1-column or
; ;; 2-column.
; (make-charset 'arabic-digit "Arabic digit"
; 	      '(dimension
; 		1
; 		registry "MuleArabic-0"
; 		chars 94
; 		columns 1
; 		direction l2r
; 		final ?2
; 		graphic 0
; 		short-name "Arabic digit"
; 		long-name "Arabic digit"
; 		))

; (make-charset 'arabic-1-column "Arabic 1-column"
; 	      '(dimension
; 		1
; 		registry "MuleArabic-1"
; 		chars 94
; 		columns 1
; 		direction r2l
; 		final ?3
; 		graphic 0
; 		short-name "Arabic 1-col"
; 		long-name "Arabic 1-column"
; 		))


; ;; ASCII with right-to-left direction.
; (make-charset 'ascii-right-to-left 
; 	      "ASCII (left half of ISO 8859-1) with right-to-left direction"
; 	      '(dimension
; 		1
; 		registry "ISO8859-1"
; 		chars 94
; 		columns 1
; 		direction r2l
; 		final ?B
; 		graphic 0
; 		short-name "rev ASCII"
; 		long-name "ASCII with right-to-left direction"
; 		))


; ;; Lao script.
; ;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
; (make-charset 'lao "Lao characters (ISO10646 0E80..0EDF)"
; 	      '(dimension
; 		1
; 		registry "MuleLao-1"
; 		chars 94
; 		columns 1
; 		direction l2r
; 		final ?1
; 		graphic 0
; 		short-name "Lao"
; 		long-name "Lao"
; 		))


; ;; CHARSET-IDs 168..223 are not used.

; ;; 1-byte 2-column charsets.  Valid range of CHARSET-ID is 224..239.

; (make-charset 'arabic-2-column "Arabic 2-column"
; 	      '(dimension
; 		1
; 		registry "MuleArabic-2"
; 		chars 94
; 		columns 2
; 		direction r2l
; 		final ?4
; 		graphic 0
; 		short-name "Arabic 2-col"
; 		long-name "Arabic 2-column"
; 		))


; ;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
; ;; not assigned.  They are automatically converted to each Indian
; ;; script which IS-13194 supports.

; (make-charset 'indian-is13194 
; 	      "Generic Indian charset for data exchange with IS 13194"
; 	      '(dimension
; 		1
; 		registry "IS13194-Devanagari"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?5
; 		graphic 1
; 		short-name "IS 13194"
; 		long-name "Indian IS 13194"
; 		))


; ;; CHARSET-IDs 226..239 are not used.

; ;; 2-byte 1-column charsets.  Valid range of CHARSET-ID is 240..244.

; ;; Actual Glyph for 1-column width.
; (make-charset 'indian-1-column 
; 	      "Indian charset for 2-column width glyphs"
; 	      '(dimension
; 		2
; 		registry "MuleIndian-1"
; 		chars 94
; 		columns 1
; 		direction l2r
; 		final ?6
; 		graphic 0
; 		short-name "Indian 1-col"
; 		long-name "Indian 1 Column"
; 		))


; (make-charset 'tibetan-1-column "Tibetan 1 column glyph"
; 	      '(dimension
; 		2
; 		registry "MuleTibetan-1"
; 		chars 94
; 		columns 1
; 		direction l2r
; 		final ?8
; 		graphic 0
; 		short-name "Tibetan 1-col"
; 		long-name "Tibetan 1 column"
; 		))


; ;; Subsets of Unicode.

; (make-charset 'mule-unicode-2500-33ff 
; 	      "Unicode characters of the range U+2500..U+33FF."
; 	      '(dimension
; 		2
; 		registry "ISO10646-1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?2
; 		graphic 0
; 		short-name "Unicode subset 2"
; 		long-name "Unicode subset (U+2500..U+33FF)"
; 		))


; (make-charset 'mule-unicode-e000-ffff 
; 	      "Unicode characters of the range U+E000..U+FFFF."
; 	      '(dimension
; 		2
; 		registry "ISO10646-1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?3
; 		graphic 0
; 		short-name "Unicode subset 3"
; 		long-name "Unicode subset (U+E000+FFFF)"
; 		))


; (make-charset 'mule-unicode-0100-24ff 
; 	      "Unicode characters of the range U+0100..U+24FF."
; 	      '(dimension
; 		2
; 		registry "ISO10646-1"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?1
; 		graphic 0
; 		short-name "Unicode subset"
; 		long-name "Unicode subset (U+0100..U+24FF)"
; 		))


; ;; 2-byte 2-column charsets.  Valid range of CHARSET-ID is 245..254.

; ;; Ethiopic characters (Amahric and Tigrigna).
; (make-charset 'ethiopic "Ethiopic characters"
; 	      '(dimension
; 		2
; 		registry "Ethiopic-Unicode"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?3
; 		graphic 0
; 		short-name "Ethiopic"
; 		long-name "Ethiopic characters"
; 		))


; ;; Chinese CNS11643 Plane3 thru Plane7.  Although these are official
; ;; character sets, the use is rare and don't have to be treated
; ;; space-efficiently in the buffer.
; (make-charset 'chinese-cns11643-3 
; 	      "CNS11643 Plane 3 Chinese Traditional: ISO-IR-183"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-3"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?I
; 		graphic 0
; 		short-name "CNS11643-3"
; 		long-name "CNS11643-3 (Chinese traditional): ISO-IR-183"
; 		))

; (make-charset 'chinese-cns11643-4 
; 	      "CNS11643 Plane 4 Chinese Traditional: ISO-IR-184"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-4"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?J
; 		graphic 0
; 		short-name "CNS11643-4"
; 		long-name "CNS11643-4 (Chinese traditional): ISO-IR-184"
; 		))

; (make-charset 'chinese-cns11643-5 
; 	      "CNS11643 Plane 5 Chinese Traditional: ISO-IR-185"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-5"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?K
; 		graphic 0
; 		short-name "CNS11643-5"
; 		long-name "CNS11643-5 (Chinese traditional): ISO-IR-185"
; 		))

; (make-charset 'chinese-cns11643-6 
; 	      "CNS11643 Plane 6 Chinese Traditional: ISO-IR-186"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-6"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?L
; 		graphic 0
; 		short-name "CNS11643-6"
; 		long-name "CNS11643-6 (Chinese traditional): ISO-IR-186"
; 		))

; (make-charset 'chinese-cns11643-7 
; 	      "CNS11643 Plane 7 Chinese Traditional: ISO-IR-187"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-7"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?M
; 		graphic 0
; 		short-name "CNS11643-7"
; 		long-name "CNS11643-7 (Chinese traditional): ISO-IR-187"
; 		))


; ;; Actual Glyph for 2-column width.
; (make-charset 'indian-2-column 
; 	      "Indian charset for 2-column width glyphs"
; 	      '(dimension
; 		2
; 		registry "MuleIndian-2"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?5
; 		graphic 0
; 		short-name "Indian 2-col"
; 		long-name "Indian 2 Column"
; 		))


; ;; Tibetan script.
; (make-charset 'tibetan "Tibetan characters"
; 	      '(dimension
; 		2
; 		registry "MuleTibetan-2"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?7
; 		graphic 0
; 		short-name "Tibetan 2-col"
; 		long-name "Tibetan 2 column"
; 		))


; ;; CHARSET-ID 253 is not used.

; ;; JISX0213 Plane 2
; (make-charset 'japanese-jisx0213-2 "JISX0213 Plane 2 (Japanese)"
; 	      '(dimension
; 		2
; 		registry "JISX0213.2000-2"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?P
; 		graphic 0
; 		short-name "JISX0213-2"
; 		long-name "JISX0213-2"
; 		))

;;; mule-charset.el ends here

