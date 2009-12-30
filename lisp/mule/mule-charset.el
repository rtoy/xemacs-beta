;;; mule-charset.el --- Charset functions for Mule.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1992, 2001 Free Software Foundation, Inc.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.
;; Copyright (C) 2002, 2005 Ben Wing.

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

(defun charsets-in-string (string)
  "Return a list of the charsets in STRING."
  (let (res)
    (with-string-as-buffer-contents string
      ;; charsets-in-region now in C. 
      (setq res (charsets-in-region (point-min) (point-max))))
    res))

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

;; Not in GNU Emacs/Mule
(defun charset-registry (charset)
  "Obsolete; use charset-registries instead. "
  (lwarn 'xintl 'warning 
    "charset-registry is obsolete--use charset-registries instead. ")
  (when (charset-property charset 'registries)
    (elt (charset-property charset 'registries) 0)))

(make-obsolete 'charset-registry 'charset-registries)

(defun charset-registries (charset)
  "Return the registries of CHARSET."
  (charset-property charset 'registries))

(defun set-charset-registry (charset registry)
  "Obsolete; use set-charset-registries instead. "
  (check-argument-type 'stringp registry)
  (check-argument-type 'charsetp (find-charset charset))
  (unless (equal registry (regexp-quote registry))
    (lwarn 'xintl 'warning
      "Regexps no longer allowed for charset-registry. Treating %s%s"
      registry " as a string."))
  (set-charset-registries 
   charset 
   (apply 'vector registry (append (charset-registries charset) nil))))

(make-obsolete 'set-charset-registry 'set-charset-registries)

(when (featurep 'ccl)
  (defun charset-ccl-program (charset)
    "Return the CCL program of CHARSET.
See `make-charset'."
    (charset-property charset 'ccl-program)))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(defun charset-skip-chars-string (charset)
  "Given  CHARSET, return a string suitable for for `skip-chars-forward'.
Passing the string to `skip-chars-forward' will cause it to skip all
characters in CHARSET."
  (setq charset (get-charset charset))
  (cond 
   ;; Aargh, the general algorithm doesn't work for these charsets, because
   ;; make-char strips the high bit. Hard code them.
   ((eq (find-charset 'ascii) charset) "\x00-\x7f")
   ((eq (find-charset 'control-1) charset) "\x80-\x9f")
   (t 
    (let (charset-lower charset-upper row-upper row-lower)
      (if (= 1 (charset-dimension charset))
          (condition-case args-out-of-range
              (make-char charset #x100)
            (args-out-of-range 
             (setq charset-lower (third args-out-of-range)
                   charset-upper (fourth args-out-of-range))
             (format "%c-%c"
                     (make-char charset charset-lower)
                     (make-char charset charset-upper))))
        (condition-case args-out-of-range
            (make-char charset #x100 #x22)
          (args-out-of-range
           (setq row-lower (third args-out-of-range)
                 row-upper (fourth args-out-of-range))))
        (condition-case args-out-of-range
            (make-char charset #x22 #x100)
          (args-out-of-range
           (setq charset-lower (third args-out-of-range)
                 charset-upper (fourth args-out-of-range))))
        (format "%c-%c"
                (make-char charset row-lower charset-lower)
                (make-char charset row-upper charset-upper)))))))
;; From GNU. 
(defun map-charset-chars (func charset)
  "Use FUNC to map over all characters in CHARSET for side effects.
FUNC is a function of two args, the start and end (inclusive) of a
character code range.  Thus FUNC should iterate over [START, END]."
  (check-argument-type #'functionp func)
  (check-argument-type #'charsetp (setq charset (find-charset charset)))
  (let* ((dim (charset-dimension charset))
	 (chars (charset-chars charset))
	 (start (if (= chars 94)
		    33
		  32)))
    (if (= dim 1)
        (cond 
         ((eq (find-charset 'ascii) charset) (funcall func ?\x00 ?\x7f))
         ((eq (find-charset 'control-1) charset) (funcall func ?\x80 ?\x9f))
         (t 
          (funcall func
                   (make-char charset start)
                   (make-char charset (+ start chars -1)))))
      (dotimes (i chars)
	(funcall func
		 (make-char charset (+ i start) start)
		 (make-char charset (+ i start) (+ start chars -1)))))))

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
<<<<<<< /xemacs/hg-unicode-premerge-merge-2009/lisp/mule/mule-charset.el
(when (featurep 'ccl)
  (defsetf charset-ccl-program set-charset-ccl-program))
||||||| /DOCUME~1/Ben/LOCALS~2/Temp/mule-charset.el~base.iQ2YHE
(defsetf charset-ccl-program set-charset-ccl-program)
=======
(defsetf charset-ccl-program set-charset-ccl-program)
(defsetf charset-registries  set-charset-registries)
>>>>>>> /DOCUME~1/Ben/LOCALS~2/Temp/mule-charset.el~other.kh3W3H

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
A translation table is a char table intended for character translation
in CCL programs.

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


;; @@####
;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLINE are already set.
(let ((l `(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312
	   ,@(if (find-charset 'chinese-big5-1)
		 '(chinese-big5-1 chinese-big5-2)
	       '(chinese-big5)))))
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

;;; In international/mule-conf.el in GNU Emacs.

;;; Definitions of character sets.  We must put them here, rather than
;;; in the individual files devoted to particular languages (as we did
;;; before), because we need to load the Unicode tables for them
;;; *before* loading any files containing characters from these
;;; character sets. (If/when these files are converted to UTF-8, the
;;; problem will conceivably go away, at least for Unicode-internal --
;;; but then the opposite problem would exist for old-Mule, if this is
;;; still being preserved.)

; #### No equivalent of the following charset from FSF

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


; ;; ISO-2022 allows a use of character sets not registered in ISO with
; ;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
; ;; reserves `0' through `9' to support several private character sets.
; ;; The remaining final characters `:' through `?' are for users.

; (make-charset 'latin-iso8859-1 
; 	      "Right-Hand Part of Latin Alphabet 1 (ISO/IEC 8859-1): ISO-IR-100"
; 	      '(dimension
; 		1
; 		registry "ISO8859-1"
; 		chars 96
; 		final ?A
; 		graphic 1
; 		short-name "Latin-1"
; 		long-name "RHP of Latin-1 (ISO 8859-1): ISO-IR-100"
; 		))

; (make-charset 'latin-iso8859-2 
; 	      "Right-Hand Part of Latin Alphabet 2 (ISO/IEC 8859-2): ISO-IR-101"
; 	      '(dimension
; 		1
; 		registry "ISO8859-2"
; 		chars 96
; 		final ?B
; 		graphic 1
; 		short-name "Latin-2"
; 		long-name "RHP of Latin-2 (ISO 8859-2): ISO-IR-101"
; 		))

; (make-charset 'latin-iso8859-3 
; 	      "Right-Hand Part of Latin Alphabet 3 (ISO/IEC 8859-3): ISO-IR-109"
; 	      '(dimension
; 		1
; 		registry "ISO8859-3"
; 		chars 96
; 		final ?C
; 		graphic 1
; 		short-name "Latin-3"
; 		long-name "RHP of Latin-3 (ISO 8859-3): ISO-IR-109"
; 		))

; (make-charset 'latin-iso8859-4 
; 	      "Right-Hand Part of Latin Alphabet 4 (ISO/IEC 8859-4): ISO-IR-110"
; 	      '(dimension
; 		1
; 		registry "ISO8859-4"
; 		chars 96
; 		final ?D
; 		graphic 1
; 		short-name "Latin-4"
; 		long-name "RHP of Latin-4 (ISO 8859-4): ISO-IR-110"
; 		))

; (make-charset 'latin-iso8859-9 
; 	      "Right-Hand Part of Latin Alphabet 5 (ISO/IEC 8859-9): ISO-IR-148"
; 	      '(dimension
; 		1
; 		registry "ISO8859-9"
; 		chars 96
; 		final ?M
; 		graphic 1
; 		short-name "Latin-5"
; 		long-name "RHP of Latin-5 (ISO 8859-9): ISO-IR-148"
; 		))

(make-charset 'latin-iso8859-10
	      "Supplementary Set for Latin Alphabet No. 6 (ISO/IEC 8859-10): ISO-IR-157
\"This set is intended for a version of ISO 4873 using the coding method of
ISO 8859 and requiring the character repertoires of the languages used in
Northern Europe.\""
	      '(dimension
		1
		registry "ISO8859-10"
		chars 96
		final ?V ;; 0x56 aka octet 5/6
		graphic 1
		short-name "Latin-6 (Northern Europe)"
		long-name "RHP of Latin-6 (Northern Europe) (ISO 8859-10): ISO-IR-157"
		))

(make-charset 'latin-iso8859-13
	      "Baltic Rim Supplementary Set (Latin-7) (ISO/IEC 8859-13): ISO-IR-179"
	      '(dimension
		1
		registry "ISO8859-13"
		chars 96
		final ?Y ;; 0x59 aka octet 5/9
		graphic 1
		short-name "Latin-7 (Baltic Rim)"
		long-name "RHP of Latin-7 (Baltic Rim) (ISO 8859-13): ISO-IR-179"
		))

(make-charset 'latin-iso8859-14 
	      "Celtic Supplementary Latin Set (Latin-8) (ISO/IEC 8859-14): ISO-IR-199
FIELD OF UTILIZATION: \"Communication and processing of text in the Celtic
languages, especially Welsh and Irish Gaelic. The set also provides for the
languages enumerated in ISO/IEC 8859-1 (though French is not fully
covered).\""
	      '(dimension
		1
		registry "ISO8859-14"
		chars 96
		final ?_
		graphic 1
		short-name "Latin-8 (Celtic)"
		long-name "RHP of Latin-8 (Celtic) (ISO 8859-14): ISO-IR-199"
		))

; (make-charset 'latin-iso8859-15 
; 	      "European Supplementary Latin Set (\"Latin 9\") (Euro Sign) (ISO/IEC 8859-15): ISO-IR-203
;FIELD OF UTILIZATION: \"Communication and processing of text in European
;languages. The set provides for the languages enumerated in ISO/IEC
;8859-1. In addition, it contains the EURO SIGN and provides support for the
;French, and Finnish languages in addition.\""
; 	      '(dimension
; 		1
; 		registry "ISO8859-15"
; 		chars 96
; 		final ?b
; 		graphic 1
; 		short-name "Latin-9 (Euro Sign)"
; 		long-name "RHP of Latin-9 (Euro Sign) (ISO 8859-15): ISO-IR-203"
; 		))

(make-charset 'latin-iso8859-16
	      "Romanian Character Set for Information Interchange (Latin-10) (ISO/IEC 8859-16): ISO-IR-226
FIELD OF UTILIZATION: \"Communication, processing, transfer of text in the
Romanian language\""
	      '(dimension
		1
		registry "ISO8859-16"
		chars 96
		final ?f			; octet 06/06; cf ISO-IR 226
		graphic 1
		short-name "Latin-10 (Romanian)"
		long-name "RHP of Latin-10 (Romanian) (ISO 8859-16): ISO-IR-226"
		))

; (make-charset 'chinese-gb2312 
; 	      "GB2312 Chinese simplified: ISO-IR-58"
; 	      '(dimension
; 		2
; 		registry "GB2312.1980"
; 		chars 94
; 		final ?A
; 		graphic 0
; 		short-name "GB2312"
; 		long-name "GB2312: ISO-IR-58"
; 		))

; (make-charset 'chinese-cns11643-1 
; 	      "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-1"
; 		chars 94
; 		final ?G
; 		graphic 0
; 		short-name "CNS11643-1"
; 		long-name "CNS11643-1 (Chinese traditional): ISO-IR-171"
; 		))

; (make-charset 'chinese-cns11643-2 
; 	      "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
; 	      '(dimension
; 		2
; 		registry "CNS11643.1992-2"
; 		chars 94
; 		final ?H
; 		graphic 0
; 		short-name "CNS11643-2"
; 		long-name "CNS11643-2 (Chinese traditional): ISO-IR-172"
; 		))

; (make-charset 'chinese-big5-1 
; 	      "Frequently used part (A141-C67F) of Big5 (Chinese traditional)"
; 	      '(dimension
; 		2
; 		registry "Big5"
; 		chars 94
; 		final ?0
; 		graphic 0
; 		short-name "Big5 (Level-1)"
; 		long-name "Big5 (Level-1) A141-C67F"
; 		))

; (make-charset 'chinese-big5-2 
; 	      "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
; 	      '(dimension
; 		2
; 		registry "Big5"
; 		chars 94
; 		final ?1
; 		graphic 0
; 		short-name "Big5 (Level-2)"
; 		long-name "Big5 (Level-2) C940-FEFE"
; 		))

; ;; PinYin-ZhuYin
; (make-charset 'chinese-sisheng 
; 	      "SiSheng characters for PinYin/ZhuYin"
; 	      '(dimension
; 		1
; 		;; XEmacs addition: second half of registry spec
; 		registry "sisheng_cwnn\\|OMRON_UDC_ZH"
; 		chars 94
; 		final ?0
; 		graphic 0
; 		short-name "SiSheng"
; 		long-name "SiSheng (PinYin/ZhuYin)"
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

;; CNS11643 Plane3 thru Plane7
;; These represent more and more obscure Chinese characters.
;; By the time you get to Plane 7, we're talking about characters
;; that appear once in some ancient manuscript and whose meaning
;; is unknown.

(flet
    ((make-chinese-cns11643-charset
      (name plane final)
      (make-charset
       name (concat "CNS 11643 Plane " plane " (Chinese traditional)")
       `(registry 
         ,(concat "CNS11643[.-]\\(.*[.-]\\)?" plane "$")
         dimension 2
         chars 94
         final ,final
         graphic 0
	 short-name ,(concat "CNS11643-" plane)
	 long-name ,(format "CNS11643-%s (Chinese traditional): ISO-IR-183"
			    plane)))
      ))
  (make-chinese-cns11643-charset 'chinese-cns11643-3 "3" ?I)
  (make-chinese-cns11643-charset 'chinese-cns11643-4 "4" ?J)
  (make-chinese-cns11643-charset 'chinese-cns11643-5 "5" ?K)
  (make-chinese-cns11643-charset 'chinese-cns11643-6 "6" ?L)
  (make-chinese-cns11643-charset 'chinese-cns11643-7 "7" ?M)
  )

;; ISO-IR-165 (CCITT Extended GB)
;;    It is based on CCITT Recommendation T.101, includes GB 2312-80 +
;;    GB 8565-88 table A4 + 293 characters.
(make-charset ;; not in FSF 21.1
 'chinese-isoir165
 "ISO-IR-165 (CCITT Extended GB; Chinese simplified)"
 `(registry "isoir165"
   dimension 2
   chars 94
   final ?E
   graphic 0
   short-name "ISO-IR-165"
   long-name "ISO-IR-165 (CCITT Extended GB; Chinese simplified)"))

; (make-charset 'katakana-jisx0201 
; 	      "Katakana Part of JISX0201.1976"
; 	      '(dimension
; 		1
; 		registry "JISX0201"
; 		chars 94
; 		final ?I
; 		graphic 1
; 		short-name "JISX0201 Katakana"
; 		long-name "Japanese Katakana (JISX0201.1976)"
; 		))

; (make-charset 'latin-jisx0201 
; 	      "Roman Part of JISX0201.1976"
; 	      '(dimension
; 		1
; 		registry "JISX0201"
; 		chars 94
; 		final ?J
; 		graphic 0
; 		short-name "JISX0201 Roman"
; 		long-name "Japanese Roman (JISX0201.1976)"
; 		))

; (make-charset 'japanese-jisx0208-1978 
; 	      "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
; 	      '(dimension
; 		2
; 		registry "JISX0208.1990"
; 		registry "JISX0208.1978"
; 		chars 94
; 		final ?@
; 		graphic 0
; 		short-name "JISX0208.1978"
; 		long-name "JISX0208.1978 (Japanese): ISO-IR-42"
; 		))

; (make-charset 'japanese-jisx0208 
; 	      "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
; 	      '(dimension
; 		2
; 		chars 94
; 		final ?B
; 		graphic 0
; 		short-name "JISX0208"
; 		long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
; 		))

; (make-charset 'japanese-jisx0212 
; 	      "JISX0212 Japanese supplement: ISO-IR-159"
; 	      '(dimension
; 		2
; 		registry "JISX0212"
; 		chars 94
; 		final ?D
; 		graphic 0
; 		short-name "JISX0212"
; 		long-name "JISX0212 (Japanese): ISO-IR-159"
; 		))

(make-charset 'japanese-jisx0213-1 "JISX0213 Plane 1 (Japanese)"
	      '(dimension
		2
		registry "JISX0213.2000-1"
		chars 94
		final ?O
		graphic 0
		short-name "JISX0213-1"
		long-name "JISX0213-1"
		))

;; JISX0213 Plane 2
(make-charset 'japanese-jisx0213-2 "JISX0213 Plane 2 (Japanese)"
	      '(dimension
		2
		registry "JISX0213.2000-2"
		chars 94
		final ?P
		graphic 0
		short-name "JISX0213-2"
		long-name "JISX0213-2"
		))

; (make-charset 'korean-ksc5601 
; 	      "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
; 	      '(dimension
; 		2
; 		registry "KSC5601.1989"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?C
; 		graphic 0
; 		short-name "KSC5601"
; 		long-name "KSC5601 (Korean): ISO-IR-149"
; 		))

;; See comments in mule-coding.c.
;; Hangul uses the range [84 - D3], [41 - 7E, 81 - FE]
;; Symbols and Hanja use [D8 - DE, E0 - F9], [31 - 7E, 91 - FE]
;; So for our purposes, this is [84 - F9], [31 - FE] */
(make-charset 'korean-johab
	      "Johab (Korean)"
	      '(dimension
		2
		registry "johab" ;; @@#### FIXME
		chars (118 206)
		offset (#x84 #x31)
		short-name "Johab"
		long-name "Johab (Korean)"
		))

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(make-charset 'vietnamese-viscii-lower "VISCII1.1 lower-case"
	      '(dimension
		1
		registry "VISCII1.1"
		chars 96
		final ?1
		graphic 1
		short-name "VISCII lower"
		long-name "VISCII lower-case"
		))

(make-charset 'vietnamese-viscii-upper "VISCII1.1 upper-case"
	      '(dimension
		1
		registry "VISCII1.1"
		chars 96
		final ?2
		graphic 1
		short-name "VISCII upper"
		long-name "VISCII upper-case"
		))

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

; (make-charset 'cyrillic-iso8859-5 
; 	      "Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144"
; 	      '(dimension
; 		1
; 		registry "ISO8859-5"
; 		chars 96
; 		final ?L
; 		graphic 1
; 		short-name "RHP of ISO8859/5"
; 		long-name "RHP of Cyrillic (ISO 8859-5): ISO-IR-144"
; 		))

(make-charset 'cyrillic-koi8-r
	      "Cyrillic KOI8-R"
	      '(dimension
		1
		chars 256
		short-name "Cyrillic KOI8-R"
		long-name "Cyrillic KOI8-R"
		))

(make-charset 'cyrillic-alternativnyj
	      "Cyrillic Alternativnyj"
	      '(dimension
		1
		chars 256
		short-name "Cyrillic Alternativnyj"
		long-name "Cyrillic Alternativnyj"
		))

; (make-charset 'hebrew-iso8859-8 
; 	      "Right-Hand Part of Latin/Hebrew Alphabet (ISO/IEC 8859-8): ISO-IR-138"
; 	      '(dimension
; 		1
; 		registry "ISO8859-8"
; 		chars 96
; 		columns 1
; 		direction r2l
; 		final ?H
; 		graphic 1
; 		short-name "RHP of ISO8859/8"
; 		long-name "RHP of Hebrew (ISO 8859-8): ISO-IR-138"
; 		))

; (make-charset 'arabic-iso8859-6 
; 	      "Right-Hand Part of Latin/Arabic Alphabet (ISO/IEC 8859-6): ISO-IR-127"
; 	      '(dimension
; 		1
; 		registry "ISO8859-6"
; 		chars 96
; 		columns 1
; 		direction r2l
; 		final ?G
; 		graphic 1
; 		short-name "RHP of ISO8859/6"
; 		long-name "RHP of Arabic (ISO 8859-6): ISO-IR-127"
; 		))

;; For Arabic, we need three different types of character sets.
;; Digits are of direction left-to-right and of width 1-column.
;; Others are of direction right-to-left and of width 1-column or
;; 2-column.
(make-charset 'arabic-digit "Arabic digit"
	      '(dimension
		1
		registry "MuleArabic-0"
		chars 94
		columns 1
		direction l2r
		final ?2
		graphic 0
		short-name "Arabic digit"
		long-name "Arabic digit"
		))

(make-charset 'arabic-1-column "Arabic 1-column"
	      '(dimension
		1
		registry "MuleArabic-1"
		chars 94
		columns 1
		direction r2l
		final ?3
		graphic 0
		short-name "Arabic 1-col"
		long-name "Arabic 1-column"
		))

(make-charset 'arabic-2-column "Arabic 2-column"
	      '(dimension
		1
		registry "MuleArabic-2"
		chars 94
		columns 2
		direction r2l
		final ?4
		graphic 0
		short-name "Arabic 2-col"
		long-name "Arabic 2-column"
		))

; (make-charset 'thai-tis620 
; 	      "Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166"
; 	      '(dimension
; 		1
; 		registry "TIS620"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?T
; 		graphic 1
; 		short-name "RHP of TIS620"
; 		long-name "RHP of Thai (TIS620): ISO-IR-166"
; 		))

(make-charset 'thai-xtis "Precomposed Thai (XTIS by Virach)."
	      '(registry "xtis-0"
			 dimension 2
			 columns 1
			 chars 94
			 final ??
			 graphic 0))

; ;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
; ;; not assigned.  They are automatically converted to each Indian
; ;; script which IS-13194 supports.

(make-charset 'indian-is13194 
	      "Generic Indian charset for data exchange with IS 13194"
	      '(dimension
		1
		registry "IS13194-Devanagari"
		chars 94
		columns 2
		direction l2r
		final ?5
		graphic 1
		short-name "IS 13194"
		long-name "Indian IS 13194"
		))

;; Actual Glyph for 1-column width.
(make-charset 'indian-1-column 
	      "Indian charset for 2-column width glyphs"
	      '(dimension
		2
		registry "MuleIndian-1"
		chars 94
		columns 1
		direction l2r
		final ?6
		graphic 0
		short-name "Indian 1-col"
		long-name "Indian 1 Column"
		))

;; Actual Glyph for 2-column width.
(make-charset 'indian-2-column 
	      "Indian charset for 2-column width glyphs"
	      '(dimension
		2
		registry "MuleIndian-2"
		chars 94
		columns 2
		direction l2r
		final ?5
		graphic 0
		short-name "Indian 2-col"
		long-name "Indian 2 Column"
		))

;; Lao script.
;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
(make-charset 'lao "Lao characters (ISO10646 0E80..0EDF)"
	      '(dimension
		1
		registry "MuleLao-1"
		chars 94
		columns 1
		direction l2r
		final ?1
		graphic 0
		short-name "Lao"
		long-name "Lao"
		))

;; Ethiopic characters (Amahric and Tigrigna).
(make-charset 'ethiopic "Ethiopic characters"
	      '(dimension
		2
		registry "Ethiopic-Unicode"
		chars 94
		final ?3
		graphic 0
		short-name "Ethiopic"
		long-name "Ethiopic characters"
		))

(make-charset 'tibetan-1-column "Tibetan 1 column glyph"
	      '(dimension
		2
		registry "MuleTibetan-1"
		chars 94
		columns 1
		direction l2r
		final ?8
		graphic 0
		short-name "Tibetan 1-col"
		long-name "Tibetan 1 column"
		))

;; Tibetan script.
(make-charset 'tibetan "Tibetan characters"
	      '(dimension
		2
		registry "MuleTibetan-2"
		chars 94
		columns 2
		direction l2r
		final ?7
		graphic 0
		short-name "Tibetan 2-col"
		long-name "Tibetan 2 column"
		))

;; IPA characters for phonetic symbols.
(make-charset 'ipa "IPA (International Phonetic Association)"
	      '(dimension
		1
		registry "MuleIPA"
		chars 96
		columns 1
		direction l2r
		final ?0
		graphic 1
		short-name "IPA"
		long-name "IPA"
		))

; ;; Subsets of Unicode.

; #### what is this bogosity ... "chars 96, final ?2" !!?!
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

(let ((charsets '((874 thai "Thai")
		  (1250 latin "Eastern Europe")
		  (1251 cyrillic "Cyrillic")
		  (1252 latin "ANSI")
		  (1253 greek "Greek")
		  (1254 latin "Turkish")
		  (1255 hebrew "Hebrew")
		  (1256 arabic "Arabic")
		  (1257 latin "Baltic Rim")
		  (1258 latin "Vietnamese"))))
  (loop for (num script name) in charsets do
    (make-charset (intern (format "%s-windows-%s" script num))
		  (format "Windows code page %s (%s)" num name)
		  `(dimension
		    1
		    chars 256
		    short-name ,(format "Windows %s (%s)" num name)
		    long-name ,(format "Windows code page %s (%s)" num name)
		    ))))

(let ((charsets '((932 japanese "Japanese" #x81 #x40 #xfe #xfe)
		  (936 chinese "Simplified Chinese" #x81 #x40 #xfe #xfe)
		  (949 korean "Korean" #x81 #x41 #xfe #xfe)
		  (950 chinese "Traditional Chinese" #xa1 #x40 #xfe #xfe)
		  )))
  (loop for (num script name l1 l2 h1 h2) in charsets do
    (make-charset (intern (format "%s-windows-%s" script num))
		  (format "Windows code page %s (%s)" num name)
		  `(dimension
		    2
		    chars (,(1+ (- h1 l1)) ,(1+ (- h2 l2)))
		    offset (,l1 ,l2)
		    short-name ,(format "Windows %s (%s)" num name)
		    long-name ,(format "Windows code page %s (%s)" num name)
		    ))))

;;; mule-charset.el ends here

