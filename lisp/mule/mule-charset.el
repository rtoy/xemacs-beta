;;; mule-charset.el --- Charset functions for Mule.

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1992, 2001 Free Software Foundation, Inc.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.
;; Copyright (C) 2002, 2005, 2010 Ben Wing.

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

(defun charset-offset (charset)
  "Return the minimum index per dimension of CHARSET."
  (charset-property charset 'offset))

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

(defun charset-ccl-program (charset)
  "Return the CCL program of CHARSET.
See `make-charset'."
  (charset-property charset 'ccl-program))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(defun charset-skip-chars-string (charset)
  "Given CHARSET, return a string suitable for for `skip-chars-forward'.
Passing the string to `skip-chars-forward' will cause it to skip all
characters in CHARSET."
  (setq charset (get-charset charset))
  (let* ((dim (charset-dimension charset))
	 (chars (charset-chars charset))
	 (offset (charset-offset charset))
	 (lowchar (if (= dim 1)
		      (make-char charset offset)
		    (make-char charset
			       (first offset)
			       (second offset))))
	 (highchar (if (= dim 1)
		       (make-char charset (+ offset chars -1))
		     (make-char charset
				(+ (first offset) (first chars) -1)
				(+ (second offset) (second chars) -1)))))
    (unless (and lowchar highchar)
      (signal-error 'invalid-argument
		    `("Charset not encodable in a buffer" ,charset)))
    (format "%c-%c" lowchar highchar)))

;; From GNU.
(defun map-charset-chars (func charset)
  "Use FUNC to map over all characters in CHARSET for side effects.
FUNC is a function of two args, the start and end (inclusive) of a
character code range.  Thus FUNC should iterate over [START, END]."
  (check-argument-type #'functionp func)
  (setq charset (get-charset charset))
  (let ((dim (charset-dimension charset))
	(chars (charset-chars charset))
	(offset (charset-offset charset)))
    (if (= dim 1)
	(funcall func
		 (make-char charset offset)
		 (make-char charset (+ offset chars -1)))
      (loop with (off1 off2) = offset
	with (chars1 chars2) = chars
	for i from off1 to (+ off1 chars1 -1)
	do
	(funcall func
		 (make-char charset i off2)
		 (make-char charset i (+ off2 chars2 -1)))))))

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
(defsetf charset-ccl-program set-charset-ccl-program)
(defsetf charset-ccl-program set-charset-ccl-program)
(defsetf charset-registries  set-charset-registries)

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
;	(rev-table (make-char-table 'translation-table))
;	(i 0)
;	ch)
;     (while (< i 256)
;       (setq ch (aref vec i))
;       (aset table i ch)
;       (if (>= ch 256)
;	  (aset rev-table ch i))
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
;	      "ASCII (left half of ISO 8859-1) with right-to-left direction"
;	      '(dimension
;		1
;		registries ["ISO8859-1"]
;		chars 94
;		columns 1
;		direction r2l
;		final ?B
;		graphic 0
;		short-name "rev ASCII"
;		long-name "ASCII with right-to-left direction"
;		))

; ;; ISO-2022 allows a use of character sets not registered in ISO with
; ;; final characters `0' (0x30) through `?' (0x3F).  Among them, Emacs
; ;; reserves `0' through `9' to support several private character sets.
; ;; The remaining final characters `:' through `?' are for users.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; DEFINITION OF INTERNAL CHARSETS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE NOTE NOTE NOTE NOTE:
;;
;; We must define these charsets FIRST if we want them to end up with
;; "official" rather than "private" ID's under old-Mule.  This matters in
;; that charsets with official ID's are stored more efficiently (with one
;; less byte) than charsets with private ID's.

;; Here is one option for a more sensible keyword-based interface onto
;; charset creation:

;(defun* make-charset* (name short-name &key long-name doc-string
;			    dimension offset chars direction registries
;			    columns graphic final ccl-program unicode-map)
;  "Make a charset.  This is an alternative interface to `make-charset'.
;This interface uses keyword properties instead of a list of properties,
;and takes a mandatory short-name parameter rather than a doc string.
;NAME is a symbol, the charset's name.  SHORT-NAME is a string describing
; the charset briefly, and will be used as the `short-name' property.
;The keys :long-name, :doc-string, :dimension, :registries, :columns,
; :graphic, :final, :ccl-program, and :unicode-map will be used to
; set the associated charset properties.  If unspecified, :long-name defaults
; to `short-name', and :doc-string defaults to :long-name."
;  (setq long-name (or long-name short-name))
;  (setq doc-string (or doc-string long-name))
;  (make-charset name doc-string
;		`(short-name ,short-name
;		  long-name ,long-name
;		  ,@(and dimension `(dimension ,dimension))
;		  ,@(and offset `(offset ,offset))
;		  ,@(and chars `(chars ,chars))
;		  ,@(and direction `(direction ,direction))
;		  ,@(and registries `(registries ,registries))
;		  ,@(and columns `(columns ,columns))
;		  ,@(and graphic `(graphic ,graphic))
;		  ,@(and final `(final ,final))
;		  ,@(and ccl-program `(ccl-program ,ccl-program))
;		  ,@(and unicode-map `(unicode-map ,unicode-map))
;		  )))

(defun make-internal-charset (name doc-string props)
  "Make an internal charset.
This is the same as `make-charset' except that if a `unicode-map'
specifies a file name, the name is assumed relative to `data-directory', and
will be made so."
  (let ((unicode-map (plist-get props 'unicode-map)))
    (when (and unicode-map (stringp (car unicode-map)))
      ;; During loadup, data-directory is nil, but source-directory is
      ;; defined
      (let ((data-dir
	     (or data-directory (expand-file-name "etc" source-directory))))
	(setq props
	      (plist-put props 'unicode-map
			 (cons (expand-file-name (car unicode-map) data-dir)
			       (cdr unicode-map)))))))
  (make-charset name doc-string props))

(defun* make-internal-128-byte-charset (name short-name &key long-name doc-string unicode-map)
  "Make an internal one-dimension size-128 charset.
NAME is a symbol, the charset's name.
SHORT-NAME is a string describing the charset briefly, and will be used as
the `short-name' property.
The keys :long-name, :doc-string and :unicode-map will be used to set the
associated charset properties.  If unspecified, :long-name defaults to
`short-name', and :doc-string defaults to :long-name.  If :unicode-map
specifies a file name, the name is assumed relative to `data-directory',
and will be made so."
  (setq long-name (or long-name short-name))
  (setq doc-string (or doc-string long-name))
  (make-internal-charset name doc-string
			 `(dimension 1
			   offset 128
			   chars 128
			   ,@(and unicode-map `(unicode-map ,unicode-map))
			   short-name ,short-name
			   long-name ,long-name
			   )))

(defun* make-iso8859-charset (symbol str8859 short-name alphabet-name
			      iso-ir-name final &key doc-string direction)
  (let ((doc-string
	 (or doc-string
	     (format "Right-Hand Part of %s (ISO/IEC %s): %s"
		     alphabet-name str8859 iso-ir-name))))
    (make-internal-charset
     symbol doc-string
     `(dimension 1
       offset 160
       chars 96
       short-name ,short-name
       long-name ,(format "RHP of %s (ISO %s): %s"
			  short-name str8859 iso-ir-name)
       unicode-map
       (,(format "unicode/unicode-consortium/ISO8859/%s.TXT" str8859) #xA0)
       registries ,(vector (format "ISO%s" str8859))
       final ,final
       direction ,(or direction 'l2r)))))

;;;;;;;;;;;;;;;;;;;;; ISO 8859 ;;;;;;;;;;;;;;;;;;;;

;; This is defined internally because it's (probably) needed early on,
;; and because it needs to have a very specific charset ID.
;(make-iso8859-charset 'latin-iso8859-1 "8859-1" "Latin-1" "Latin Alphabet 1"
;		      "ISO-IR-100" ?A)
(make-iso8859-charset 'latin-iso8859-2 "8859-2" "Latin-2" "Latin Alphabet 2"
		      "ISO-IR-101" ?B)
(make-iso8859-charset 'latin-iso8859-3 "8859-3" "Latin-3" "Latin Alphabet 3"
		      "ISO-IR-109" ?C)
(make-iso8859-charset 'latin-iso8859-4 "8859-4" "Latin-4" "Latin Alphabet 4"
		      "ISO-IR-110" ?D)
(make-iso8859-charset 'latin-iso8859-9 "8859-9" "Latin-5" "Latin Alphabet 5"
		      "ISO-IR-148" ?M)
(make-iso8859-charset 'latin-iso8859-15 "8859-15" "Latin-9 (Euro Sign)"
		      "Latin Alphabet 9" "ISO-IR-203" ?b
		      :doc-string
		      "European Supplementary Latin Set (\"Latin 9\") (Euro Sign) (ISO/IEC 8859-15): ISO-IR-203
FIELD OF UTILIZATION: \"Communication and processing of text in European
languages. The set provides for the languages enumerated in ISO/IEC
8859-1. In addition, it contains the EURO SIGN and provides support for the
French, and Finnish languages in addition.\"")

(make-iso8859-charset 'greek-iso8859-7 "8859-7" "Greek" "Latin/Greek Alphabet"
		      "ISO-IR-126" ?F)
(make-iso8859-charset 'cyrillic-iso8859-5 "8859-5" "Cyrillic"
		      "Latin/Cyrillic Alphabet" "ISO-IR-144" ?L)
(make-iso8859-charset 'hebrew-iso8859-8 "8859-8" "Hebrew"
		      "Latin/Hebrew Alphabet" "ISO-IR-138" ?H
		      :direction 'r2l)
(make-iso8859-charset 'arabic-iso8859-6 "8859-6" "Arabic"
		      "Latin/Arabic Alphabet" "ISO-IR-127" ?G
		      :direction 'r2l)

;;;;;;;;;;;;;;;;;;;;; Japanese ;;;;;;;;;;;;;;;;;;;;

(make-internal-charset
 'katakana-jisx0201
 "Katakana Part of JISX0201.1976"
 '(dimension 1
   chars 94
   final ?I
   graphic 1
   short-name "Japanese (JISX0201 Kana)"
   long-name "Japanese Katakana (JISX0201.1976)"
   registries ["jisx0201.1976-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0201.TXT"
		#xA0)
   ))

(make-internal-charset
 'latin-jisx0201
 "Roman Part of JISX0201.1976"
 '(dimension 1
   chars 94
   final ?J
   graphic 0
   short-name "Japanese (JISX0201 Roman)"
   long-name "Japanese Roman (JISX0201.1976)"
   registries ["jisx0201.1976-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0201.TXT"
		#x21 #x7F)
   ))


(make-internal-charset
 'japanese-jisx0208-1978
 "JISX0208.1978 Japanese Kanji (so called \"old JIS\"): ISO-IR-42"
 '(dimension 2
   chars 94
   final ?@
   graphic 0
   short-name "Japanese (JISX0208.1978)"
   long-name "Japanese (JISX0208.1978): ISO-IR-42"
   registries ["jisx0208.1978-0" "jisc6226.1978-0"]
   ;; @@#### FIXME This is not correct!!!!!!!!!!!!
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0208.TXT" nil nil nil ignore-first-column)
   ))

(make-internal-charset
 'japanese-jisx0208
 "JISX0208.1983/1990 Japanese Kanji: ISO-IR-87"
 '(dimension 2
   chars 94
   final ?B
   graphic 0
   short-name "Japanese (JISX0208)"
   long-name "JISX0208.1983/1990 (Japanese): ISO-IR-87"
   registries ["jisx0208.1983-0" "jisx0208.1990-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0208.TXT" nil nil nil ignore-first-column)
   ))

(make-internal-charset
 'japanese-jisx0212
 "JISX0212 Japanese supplement: ISO-IR-159"
 '(dimension 2
   chars 94
   final ?D
   graphic 0
   short-name "Japanese (JISX0212)"
   long-name "JISX0212 (Japanese): ISO-IR-159"
   registries ["jisx0212.1990-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/JIS0212.TXT")
   ))

(when (featurep 'unicode-internal)
  ;; We can support Shift-JIS directly.
  (make-internal-charset
   'japanese-shift-jis
   ;; You could imagine trying to declare this to be an "algorithmic" charset
   ;; with indices shifted in a programmatic way from JIS X 0208:1997.
   ;; However, besides being a lot of hassle for little gain (how many other
   ;; such algorithmic charsets would there be?), I'm pretty sure that
   ;; Shift-JIS adds at least a couple of characters not present in
   ;; JIS X 0208:1997.
   "Shift-JIS Japanese encoding of JIS X 0208:1997"
   '(dimension 2
     ;; See comments in mule-coding.c.
     ;; First byte is in the range [80-9F], [E0-EF]; second byte is in the
     ;; range [40-7E], [80-FC]
     chars (112 189)
     offset (128 64)
     short-name "Japanese (Shift-JIS)"
     long-name "Japanese (Shift-JIS)"
     ;; @@#### FIXME This is the X registry; is it right?
     registries ["sjis"]
     unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/SHIFTJIS.TXT"
		  #x8000)
     ))
  )

;;;;;;;;;;;;;;;;;;;;; Chinese ;;;;;;;;;;;;;;;;;;;;

(make-internal-charset
 'chinese-gb2312
 "GB2312 Chinese simplified: ISO-IR-58"
 '(dimension 2
   chars 94
   final ?A
   graphic 0
   short-name "Chinese simplified (GB2312)"
   long-name "Chinese simplified (GB2312): ISO-IR-58"
   registries ["gb2312.1980-0" "gb2312.80&gb8565.88-0"]
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/GB2312.TXT")
   ))

(make-internal-charset
 'chinese-cns11643-1
 "CNS11643 Plane 1 Chinese traditional: ISO-IR-171"
 '(dimension 2
   chars 94
   final ?G
   graphic 0
   short-name "Chinese traditional (CNS11643-1)"
   long-name "Chinese traditional (CNS11643-1): ISO-IR-171"
   registries ["CNS11643.1992-1"]
   ;; Currently, CNS11643.TXT is based on CNS 11643-1986 (with planes 1, 2,
   ;; and 14, and no Unicode codepoint assignments outside of the BMP),
   ;; where the files in mule-ucs/chinese-cns11643-*.txt are based on
   ;; CNS 11643-1992 (with planes 1-7 and codepoint assignments outside of
   ;; the BMP).
   ;;unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/CNS11643.TXT"
   ;;             #x10000 #x1FFFF #x-10000)
   unicode-map ("unicode/mule-ucs/chinese-cns11643-1.txt")
   ))

(make-internal-charset
 'chinese-cns11643-2
 "CNS11643 Plane 2 Chinese traditional: ISO-IR-172"
 '(dimension 2
   chars 94
   final ?H
   graphic 0
   short-name "Chinese traditional (CNS11643-2)"
   long-name "Chinese traditional (CNS11643-2): ISO-IR-172"
   registries ["CNS11643.1992-2"]
   ;; See above.
   ;;unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/CNS11643.TXT"
   ;;             #x20000 #x2FFFF #x-20000)
   unicode-map ("unicode/mule-ucs/chinese-cns11643-2.txt")
   ))

(if (featurep 'unicode-internal)
    ;; We can support Big5 directly.
    (make-internal-charset
     'chinese-big5
     "Big5 (Chinese traditional)"
     '(dimension 2
       ;; Big5 claims to be a 94x157 charset, but with gaps in the middle.
       ;; In particular, the rows are (theoretically) indexed from A1 - FE
       ;; and the columns from 40 - 7E and A1 - FE.  In fact, there are gaps
       ;; in the rows as well (rows C7 and C8 are missing, as well as rows
       ;; FA - FE), but that appears to be due to accident -- i.e. they just
       ;; ran out of chars and/or wanted to make room for expansion.  Note
       ;; also that the gap at C7 and C8 is due to the Level-1/Level-2
       ;; division of Big5 (see below).  The 94 rows are those between
       ;; A1 and FE, inclusive.  The 157 columns count the sum of the columns
       ;; in each disjoint set.  For us, we need to use the size of the range
       ;; [40, FE], which is 191.
       chars (94 191)
       offset (161 64)
       short-name "Chinese traditional (Big5)"
       long-name "Chinese traditional (Big5)"
       registries ["big5.eten-0"]
       unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/BIG5.TXT")
       ))
  ;; Old Mule situation; we can only handle up to 96x96 charsets.
  ;; So we split it into two charsets.  According to Ken Lunde's CJKV
  ;; book, Big5 itself is split into "Big Five Level 1" (rows A1-C6)
  ;; and "Big Five Level 2" (rows C9-F9), with the latter containing
  ;; less used characters.  We split the same way then coerce the
  ;; result into a 94x94 block.
  (make-internal-charset
   'chinese-big5-1
   "Frequently used part (A141-C67F) of Big5 (Chinese traditional)"
   '(dimension 2
     chars 94
     final ?0
     graphic 0
     short-name "Chinese traditional (Big5), L1"
     long-name "Chinese traditional (Big5) (Level-1) A141-C67F"
     registries ["big5.eten-0"]
     ;; no unicode map, see chinese-big5-2
     ))
  (make-internal-charset
   'chinese-big5-2
   "Less frequently used part (C940-FEFE) of Big5 (Chinese traditional)"
   '(dimension 2
     chars 94
     final ?1
     graphic 0
     short-name "Chinese traditional (Big5), L2"
     long-name "Chinese traditional (Big5) (Level-2) C940-FEFE"
     registries ["big5.eten-0"]
     ;; HACK HACK HACK!  The `big5' special flag tells the internal code
     ;; in Fload_unicode_mapping_table() to take codepoints out of the
     ;; Big5 table, convert them to a codepoint in a "fake" chinese-big5-1
     ;; or chinese-big5-2, and store appropriately.  Hence, it actually
     ;; ignores the name of the charset on which the property is set and
     ;; always stores in the "right" place.  Correspondingly, we must set
     ;; the property on big5-2, not 1, so that both charsets will be
     ;; created by the time we initialize the map.
     unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/BIG5.TXT"
		  nil nil nil big5)
     ))
  )

 ;; PinYin-ZhuYin
(make-internal-charset
 'chinese-sisheng
 "SiSheng characters for PinYin/ZhuYin"
 '(dimension 1
   chars 94
   final ?0
   graphic 0
   short-name "SiSheng"
   long-name "SiSheng (PinYin/ZhuYin)"
   ;; XEmacs addition: first of the two registries
   registries ["omron_udc_zh-0" "sisheng_cwnn-0"]
   unicode-map ("unicode/mule-ucs/chinese-sisheng.txt")
   ))

;;;;;;;;;;;;;;;;;;;;; Korean ;;;;;;;;;;;;;;;;;;;;

(make-internal-charset
 'korean-ksc5601
 "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
 '(dimension 2
   chars 94
   final ?C
   graphic 0
   short-name "Korean (KSC5601)"
   long-name "Korean (KSC5601): ISO-IR-149"
					;registries ["KSC5601.1989"]
   registries ["ksc5601.1987-0"]
   ;; Note that KSC5601.TXT as currently distributed is NOT what
   ;; it claims to be!  See comments in KSX1001.TXT.
   unicode-map ("unicode/unicode-consortium/EASTASIA/OBSOLETE/KSX1001.TXT" )
   ))

;;;;;;;;;;;;;;;;;;;;; Thai ;;;;;;;;;;;;;;;;;;;;

(make-internal-charset
 'thai-tis620
 "Right-Hand Part of TIS620.2533 (Thai): ISO-IR-166"
 '(dimension 1
   chars 96
   final ?T
   graphic 1
   short-name "Thai (TIS620)"
   long-name "RHP of Thai (TIS620): ISO-IR-166"
   registries ["tis620.2529-1"]
   unicode-map ("unicode/mule-ucs/thai-tis620.txt" nil nil #x80)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; DEFINITION OF OTHER CHARSETS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; APPROPRIATE FILE: vietnamese.el
;; CAN'T BE DEFINED THERE BECAUSE: The sample text in the file has
;; Vietnamese chars.  In old-Mule, they will end up in a JIT charset unless
;; the charset is already defined.

;; Vietnamese VISCII.  VISCII is 1-byte character set which contains
;; more than 96 characters.  Since Emacs can't handle it as one
;; character set, it is divided into two: lower case letters and upper
;; case letters.
(make-internal-charset
 'vietnamese-viscii-lower "VISCII1.1 lower-case"
 '(dimension 1
   registries ["VISCII1.1"]
   chars 96
   final ?1
   graphic 1
   unicode-map ("unicode/mule-ucs/vietnamese-viscii-lower.txt"
		nil nil #x80)
   short-name "VISCII lower"
   long-name "VISCII lower-case"
   ))

(make-internal-charset
 'vietnamese-viscii-upper "VISCII1.1 upper-case"
 '(dimension 1
   registries ["VISCII1.1"]
   chars 96
   final ?2
   graphic 1
   unicode-map ("unicode/mule-ucs/vietnamese-viscii-upper.txt"
		nil nil #x80)
   short-name "VISCII upper"
   long-name "VISCII upper-case"
   ))

; ;; Indian scripts.  Symbolic charset for data exchange.  Glyphs are
; ;; not assigned.  They are automatically converted to each Indian
; ;; script which IS-13194 supports.

(make-internal-charset
 'indian-is13194
 "Generic Indian charset for data exchange with IS 13194"
 '(dimension 1
   registries ["IS13194-Devanagari"]
   chars 94
   columns 2
   final ?5
   graphic 1
   unicode-map ("unicode/mule-ucs/indian-is13194.txt"
		nil nil #x80)
   short-name "IS 13194"
   long-name "Indian IS 13194"
   ))

;; Actual Glyph for 1-column width.
(make-internal-charset
 'indian-1-column
 "Indian charset for 2-column width glyphs"
 '(dimension 2
   registries ["MuleIndian-1"]
   chars 94
   columns 1
   final ?6
   graphic 0
   short-name "Indian 1-col"
   long-name "Indian 1 Column"
   ))

;; Actual Glyph for 2-column width.
(make-internal-charset
 'indian-2-column
 "Indian charset for 2-column width glyphs"
 '(dimension    2
   registries ["MuleIndian-2"]
   chars 94
   columns 2
   final ?5
   graphic 0
   short-name "Indian 2-col"
   long-name "Indian 2 Column"
   ))

;; Lao script.
;; ISO10646's 0x0E80..0x0EDF are mapped to 0x20..0x7F.
(make-internal-charset
 'lao "Lao characters (ISO10646 0E80..0EDF)"
 '(dimension 1
   registries ["MuleLao-1"]
   chars 94
   final ?1
   graphic 0
   unicode-map ("unicode/other/lao.txt")
   short-name "Lao"
   long-name "Lao"
   ))

;; APPROPRIATE FILE: ethiopic.el
;; CAN'T BE DEFINED THERE BECAUSE: The charset is used inside of that file.

;; Ethiopic characters (Amharic and Tigrinya).
(make-internal-charset
 'ethiopic "Ethiopic characters"
 '(dimension 2
   registries ["Ethiopic-Unicode"]
   chars 94
   final ?3
   graphic 0
   unicode-map ("unicode/mule-ucs/ethiopic.txt")
   short-name "Ethiopic"
   long-name "Ethiopic characters"
   ))

(make-internal-charset
 'tibetan-1-column "Tibetan 1 column glyph"
 '(dimension 2
   registries ["MuleTibetan-1"]
   chars 94
   columns 1
   final ?8
   graphic 0
   short-name "Tibetan 1-col"
   long-name "Tibetan 1 column"
   ))

;; Tibetan script.
(make-internal-charset
 'tibetan "Tibetan characters"
 '(dimension 2
   registries ["MuleTibetan-2"]
   chars 94
   columns 2
   final ?7
   graphic 0
   unicode-map ("unicode/mule-ucs/tibetan.txt")
   short-name "Tibetan 2-col"
   long-name "Tibetan 2 column"
   ))

;; GNU Emacs has the charsets:

;;     mule-unicode-2500-33ff
;;     mule-unicode-e000-ffff
;;     mule-unicode-0100-24ff

;; built-in.  This is hack--and an incomplete hack at that--against the
;; spirit and the letter of standard ISO 2022 character sets.  Instead of
;; this, we have the jit-ucs-charset-N Mule character sets, created in
;; unicode.c on encountering a Unicode code point that we don't recognise,
;; and saved in ISO 2022 coding systems using the UTF-8 escape described in
;; ISO-IR 196.

;;; mule-charset.el ends here
