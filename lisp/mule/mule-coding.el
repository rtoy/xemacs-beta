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

;; Needed for make-8-bit-coding-system. 
(eval-when-compile (require 'ccl))

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
   mnemonic "CText"))

(make-coding-system
 'iso-8859-1 'no-conversion
 "ISO-8859-1 (Latin-1)"
 '(eol-type nil mnemonic "Noconv"))

(make-coding-system
 'iso-2022-8bit-ss2 'iso2022
 "ISO-2022 8-bit w/SS2"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-1
   charset-g2 t ;; unspecified but can be used later.
   short t
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
   mnemonic "ISO7/SS"
   eol-type nil))

(make-coding-system
 'iso-2022-7bit 'iso2022
 "ISO 2022 7-bit"
 '(charset-g0 ascii
   seven t
   short t
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

(defvar make-8-bit-private-use-start (decode-char 'ucs #xE000)
  "Start of a 256 code private use area for make-8-bit-coding-system.

This is used to ensure that distinct octets on disk for a given coding
system map to distinct XEmacs characters, preventing a spurious changes when
a file is read, not changed, and then written.  ")

(defun make-8-bit-generate-helper (decode-table encode-table
				   encode-failure-octet)
  "Helper function for `make-8-bit-generate-encode-program', which see.

Deals with the case where ASCII and another character set can both be
encoded unambiguously and completely into the coding-system; if this is so,
returns a list corresponding to such a ccl-program.  If not, it returns nil.  "
  (let ((tentative-encode-program-parts
	 (eval-when-compile 
	   (let* ((compiled 
		   (append
                    (ccl-compile
                     `(1
                       (loop
                         (read-multibyte-character r0 r1)
                         (if (r0 == ,(charset-id 'ascii))
                             (write r1)
                           ((if (r0 == #xABAB)
                                ;; #xBFFE is a sentinel in the compiled
                                ;; program.
                                (write r1 ,(make-vector 256 #xBFFE))
                              ((mule-to-unicode r0 r1)
                               (if (r0 == #xFFFD)
                                   (write #xBEEF)
                                 ((lookup-integer encode-table-sym r0 r3)
                                  (if r7
                                      (write-multibyte-character r0 r3)
                                    (write #xBEEF))))))))
                         (repeat)))) nil))
		  (first-part compiled)
		  (last-part
		   (member-if-not (lambda (entr) (eq #xBFFE entr))
				  (member-if
                                   (lambda (entr) (eq #xBFFE entr))
                                   first-part))))
	     (while compiled
	       (if (eq #xBFFE (cadr compiled))
		   (setcdr compiled nil))
	       (setq compiled (cdr compiled)))
             ;; Is the generated code as we expect it to be?
	     (assert (and (memq #xABAB first-part)
			  (memq #xBEEF14 last-part))
	    nil
	    "This code assumes that the constant #xBEEF is #xBEEF14 in \
compiled CCL code,\nand that the constant #xABAB is #xABAB. If that is
not the case, and it appears not to be--that's why you're getting this
message--it will not work.  ")
	     (list first-part last-part))))
	(charset-lower -1)
	(charset-upper -1)
	worth-trying known-charsets encode-program
	other-charset-vector ucs)

    (loop for char across decode-table
      do (pushnew (char-charset char) known-charsets))
    (setq known-charsets (delq 'ascii known-charsets))

    (loop for known-charset in known-charsets 
      do
      ;; This is not possible for two dimensional charsets. 
      (when (eq 1 (charset-dimension known-charset))
	(setq args-out-of-range t)
        (if (eq 'control-1 known-charset)
            (setq charset-lower 0
                  charset-upper 31)
	  ;; There should be a nicer way to get the limits here.
          (condition-case args-out-of-range
              (make-char known-charset #x100)
            (args-out-of-range 
             (setq charset-lower (third args-out-of-range)
                   charset-upper (fourth args-out-of-range)))))
	(loop
	  for i from charset-lower to charset-upper
	  always (and (setq ucs 
			    (encode-char (make-char known-charset i) 'ucs))
		      (gethash ucs encode-table))
	  finally (setq worth-trying known-charset))

	;; Only trying this for one charset at a time, the first find.
	(when worth-trying (return))

	;; Okay, this charset is not worth trying, Try the next.
	(setq charset-lower -1
	      charset-upper -1
	      worth-trying nil)))

    (when worth-trying
      (setq other-charset-vector (make-vector 128 encode-failure-octet))
      (loop for i from charset-lower to charset-upper
        do (aset other-charset-vector i
		 (gethash (encode-char (make-char worth-trying i)
				       'ucs) encode-table)))
      (setq encode-program
            (nsublis
             (list (cons #xABAB (charset-id worth-trying)))
             (nconc
              (copy-list (first 
                          tentative-encode-program-parts))
              (append other-charset-vector nil)
              (copy-tree (second 
                          tentative-encode-program-parts))))))
    encode-program))

(defun make-8-bit-generate-encode-program (decode-table encode-table
					   encode-failure-octet)
  "Generate a CCL program to decode a 8-bit fixed-width charset.

DECODE-TABLE must have 256 non-cons entries, and will be regarded as
describing a map from the octet corresponding to an offset in the
table to the that entry in the table.  ENCODE-TABLE is a hash table
map from unicode values to characters in the range [0,255].
ENCODE-FAILURE-OCTET describes an integer between 0 and 255
\(inclusive) to write in the event that a character cannot be encoded.  "
  (check-argument-type #'vectorp decode-table)
  (check-argument-range (length decode-table) #x100 #x100)
  (check-argument-type #'hash-table-p encode-table)
  (check-argument-type #'integerp encode-failure-octet)
  (check-argument-range encode-failure-octet #x00 #xFF)
  (let ((encode-program nil)
	(general-encode-program
	 (eval-when-compile
	   (let ((prog (append 
			(ccl-compile
			 `(1
			   (loop
			     (read-multibyte-character r0 r1)
			     (mule-to-unicode r0 r1)
			     (if (r0 == #xFFFD)
				 (write #xBEEF)
			       ((lookup-integer encode-table-sym r0 r3)
				(if r7
				    (write-multibyte-character r0 r3)
				  (write #xBEEF))))
			     (repeat)))) nil)))
	     (assert (memq #xBEEF14 prog)
		     nil
		     "This code assumes that the constant #xBEEF is #xBEEF14 \
in compiled CCL code.\nIf that is not the case, and it appears not to
be--that's why you're getting this message--it will not work.  ")
	     prog)))
	(encode-program-with-ascii-optimisation
	 (eval-when-compile 
	   (let ((prog (append
			(ccl-compile
			 `(1
			   (loop
			     (read-multibyte-character r0 r1)
			     (if (r0 == ,(charset-id 'ascii))
				 (write r1)
			       ((mule-to-unicode r0 r1)
				(if (r0 == #xFFFD)
				    (write #xBEEF)
				  ((lookup-integer encode-table-sym r0 r3)
				   (if r7
				       (write-multibyte-character r0 r3)
				     (write #xBEEF))))))
			     (repeat)))) nil)))
	     (assert (memq #xBEEF14 prog)
		     nil
		     "This code assumes that the constant #xBEEF is #xBEEF14 \
in compiled CCL code.\nIf that is not the case, and it appears not to
be--that's why you're getting this message--it will not work.  ")
	     prog)))
         (ascii-encodes-as-itself nil))

    ;; Is this coding system ASCII-compatible? If so, we can avoid the hash
    ;; table lookup for those characters. 
    (loop
      for i from #x00 to #x7f
      always (eq (int-to-char i) (gethash i encode-table))
      finally (setq ascii-encodes-as-itself t))

    ;; Note that this logic handles EBCDIC badly. For example, CP037,
    ;; MIME name ebcdic-na, has the entire repertoire of ASCII and
    ;; Latin 1, and thus a more optimal ccl encode program would check
    ;; for those character sets and use tables. But for now, we do a
    ;; hash table lookup for every character.
    (if (null ascii-encodes-as-itself)
	;; General encode program. Pros; general and correct. Cons;
	;; slow, a hash table lookup + mule-unicode conversion is done
	;; for every character encoding. 
	(setq encode-program general-encode-program)
      (setq encode-program
	    ;; Encode program with ascii-ascii mapping (based on a
	    ;; character's mule character set), and one other mule
	    ;; character set using table-based encoding, other
	    ;; character sets using hash table lookups.
	    ;; make-8-bit-non-ascii-completely-coveredp only returns
	    ;; such a mapping if some non-ASCII charset with
	    ;; characters in decode-table is entirely covered by
	    ;; encode-table.
	    (make-8-bit-generate-helper decode-table encode-table
					encode-failure-octet))
      (unless encode-program
	;; If make-8-bit-non-ascii-completely-coveredp returned nil,
	;; but ASCII still encodes as itself, do one-to-one mapping
	;; for ASCII, and a hash table lookup for everything else.
	(setq encode-program encode-program-with-ascii-optimisation)))

    (setq encode-program
          (nsublis
           (list (cons #xBEEF14 
                       (logior (lsh encode-failure-octet 8)
                               #x14)))
           (copy-tree encode-program)))
    encode-program))

(defun make-8-bit-create-decode-encode-tables (unicode-map)
  "Return a list \(DECODE-TABLE ENCODE-TABLE) given UNICODE-MAP. 
UNICODE-MAP should be an alist mapping from integer octet values to
characters with UCS code points; DECODE-TABLE will be a 256-element
vector, and ENCODE-TABLE will be a hash table mapping from 256 numbers
to 256 distinct characters.  "
  (check-argument-type #'listp unicode-map)
  (let ((decode-table (make-vector 256 nil))
        (encode-table (make-hash-table :size 256))
	(private-use-start (encode-char make-8-bit-private-use-start 'ucs))
	desired-ucs)

    (loop for (external internal)
      in unicode-map
      do
      (aset decode-table external internal)
      (assert (not (eq (encode-char internal 'ucs) -1))
	      nil
	      "Looks like you're calling make-8-bit-coding-system in a \
dumped file, \nand you're either not providing a literal UNICODE-MAP
or PROPS. Don't do that; make-8-bit-coding-system relies on sensible
Unicode mappings being available, which they are at compile time for
dumped files (but this requires the mentioned literals), but not, for
most of them, at run time.  ")

      (puthash (encode-char internal 'ucs)
	       ;; This is semantically an integer, but Dave Love's design
	       ;; for lookup-integer in CCL means we need to store it as a
	       ;; character.
	       (int-to-char external)
	       encode-table))

    ;; Now, go through the decode table looking at the characters that
    ;; remain nil. If the XEmacs character with that integer is already in
    ;; the encode table, map the on-disk octet to a Unicode private use
    ;; character. Otherwise map the on-disk octet to the XEmacs character
    ;; with that numeric value, to make it clearer what it is.
    (dotimes (i 256)
      (when (null (aref decode-table i))
	;; Find a free code point. 
	(setq desired-ucs i)
	(while (gethash desired-ucs encode-table)
	  ;; In the normal case, the code point chosen will be U+E0XY, where
	  ;; XY is the hexadecimal octet on disk. In pathological cases
	  ;; it'll be something else.
	  (setq desired-ucs (+ private-use-start desired-ucs)
		private-use-start (+ private-use-start 1)))
	(puthash desired-ucs (int-to-char i) encode-table)
        (setq desired-ucs (if (> desired-ucs #xFF)
                              (decode-char 'ucs desired-ucs)
                            ;; So we get Latin-1 when run at dump time,
                            ;; instead of JIT-allocated characters.
                            (int-to-char desired-ucs)))
        (aset decode-table i desired-ucs)))
    (values decode-table encode-table)))

(defun make-8-bit-generate-decode-program (decode-table)
  "Given DECODE-TABLE, generate a CCL program to decode an 8-bit charset.
DECODE-TABLE must have 256 non-cons entries, and will be regarded as
describing a map from the octet corresponding to an offset in the
table to the that entry in the table.  "
  (check-argument-type #'vectorp decode-table)
  (check-argument-range (length decode-table) #x100 #x100)
  (let ((decode-program-parts
	 (eval-when-compile
	   (let* ((compiled
		   (append 
		    (ccl-compile 
		     `(3
		       ((read r0)
                        (loop
			 (write-read-repeat r0 ,(make-vector
						 256 'sentinel)))))) nil))
		  (first-part compiled)
		  (last-part
		   (member-if-not #'symbolp
				  (member-if-not #'integerp first-part))))
	     ;; Chop off the sentinel sentinel sentinel [..] part. 
	     (while compiled
	       (if (symbolp (cadr compiled))
		   (setcdr compiled nil))
	       (setq compiled (cdr compiled)))
	     (list first-part last-part)))))
   (nconc
    ;; copy-list needed, because the structure of the literal provided
    ;; by our eval-when-compile hangs around.
    (copy-list (first decode-program-parts))
    (append decode-table nil)
    (second decode-program-parts))))

(defun make-8-bit-choose-category (decode-table)
  "Given DECODE-TABLE, return an appropriate coding category.
DECODE-TABLE is a 256-entry vector describing the mapping from octets on
disk to XEmacs characters for some fixed-width 8-bit coding system.  "
  (check-argument-type #'vectorp decode-table)
  (check-argument-range (length decode-table) #x100 #x100)
  (block category
    (loop
      for i from #x80 to #xBF
      do (unless (= i (aref decode-table i))
           (return-from category 'no-conversion)))
    'iso-8-1))

;;;###autoload
(defun make-8-bit-coding-system (name unicode-map &optional description props)
  "Make and return a fixed-width 8-bit CCL coding system named NAME.
NAME must be a symbol, and UNICODE-MAP a list. 

UNICODE-MAP is a plist describing a map from octets in the coding
system NAME (as integers) to XEmacs characters.  Those XEmacs
characters will be used explicitly on decoding, but for encoding (most
relevantly, on writing to disk) XEmacs characters that map to the same
Unicode code point will be unified.  This means that the ISO-8859-? 
characters that map to the same Unicode code point will not be
distinct when written to disk, which is normally what is intended; it
also means that East Asian Han characters from different XEmacs
character sets will not be distinct when written to disk, which is
less often what is intended.

Any octets not mapped will be decoded into the ISO 8859-1 characters with
the corresponding numeric value; unless another octet maps to that
character, in which case the Unicode private use area will be used.  This
avoids spurious changes to files on disk when they contain octets that would
be otherwise remapped to the canonical values for the corresponding
characters in the coding system.

DESCRIPTION and PROPS are as in `make-coding-system', which see.  This
function also accepts two additional (optional) properties in PROPS;
`aliases', giving a list of aliases to be initialized for this
coding-system, and `encode-failure-octet', an integer between 0 and 256 to
write in place of XEmacs characters that cannot be encoded, defaulting to
the code for tilde `~'.  "
  (check-argument-type #'symbolp name)
  (check-argument-type #'listp unicode-map)
  (check-argument-type #'stringp
		       (or description 
			   (setq description
				 (format "Coding system used for %s." name))))
  (check-valid-plist props)
  (let  ((encode-failure-octet (or (plist-get props 'encode-failure-octet)
				   (char-to-int ?~)))
	 (aliases (plist-get props 'aliases))
	 (hash-table-sym (gentemp (format "%s-encode-table" name)))
	 encode-program decode-program result decode-table encode-table)

    ;; Some more sanity checking. 
    (check-argument-range encode-failure-octet 0 #xFF)
    (check-argument-type #'listp aliases)

    ;; Don't pass on our extra data to make-coding-system.
    (setq props (plist-remprop props 'encode-failure-octet)
	  props (plist-remprop props 'aliases))

    (multiple-value-setq
	(decode-table encode-table)
      (make-8-bit-create-decode-encode-tables unicode-map))

    ;; Register the decode-table. 
    (define-translation-hash-table hash-table-sym encode-table)

    ;; Generate the programs. 
    (setq decode-program (make-8-bit-generate-decode-program decode-table)
          encode-program (make-8-bit-generate-encode-program
                          decode-table encode-table encode-failure-octet))
    (unless (vectorp encode-program) 
      (setq encode-program 
	    (apply #'vector
		   (nsublis (list (cons 'encode-table-sym hash-table-sym))
			    (copy-tree encode-program)))))
    (unless (vectorp decode-program)
      (setq decode-program
	    (apply #'vector decode-program)))

    ;; And now generate the actual coding system.
    (setq result 
	  (make-coding-system 
           name  'ccl
           description 
           (plist-put (plist-put props 'decode decode-program)
                      'encode encode-program)))
    (coding-system-put name 'category 
                       (make-8-bit-choose-category decode-table))
    (loop for alias in aliases
      do (define-coding-system-alias alias name))
    result))

(define-compiler-macro make-8-bit-coding-system (&whole form name unicode-map
						 &optional description props)

  ;; We provide the compiler macro (= macro that is expanded only on
  ;; compilation, and that can punt to a runtime version of the
  ;; associate function if necessary) not for reasons of speed, though
  ;; it does speed up things at runtime a little, but because the
  ;; Unicode mappings are available at compile time in the dumped
  ;; files, but they are not available at run time for the vast
  ;; majority of them.

  (if (not (and (and (consp name) (eq (car name) 'quote))
		(and (consp unicode-map) (eq (car unicode-map) 'quote))
		(and (or (and (consp props) (eq (car props) 'quote))
			 (null props)))))
      ;; The call does not use literals; do it at runtime.
      form
    (setq name (cadr name)
	  unicode-map (cadr unicode-map)
	  props (if props (cadr props)))
    (let  ((encode-failure-octet
	    (or (plist-get props 'encode-failure-octet) (char-to-int ?~)))
	   (aliases (plist-get props 'aliases))
	   encode-program decode-program
	   decode-table encode-table)

      ;; Some sanity checking. 
      (check-argument-range encode-failure-octet 0 #xFF)
      (check-argument-type #'listp aliases)

      ;; Don't pass on our extra data to make-coding-system.
      (setq props (plist-remprop props 'encode-failure-octet)
	    props (plist-remprop props 'aliases))

      ;; Work out encode-table and decode-table. 
      (multiple-value-setq
	  (decode-table encode-table)
	(make-8-bit-create-decode-encode-tables unicode-map))

      ;; Generate the decode and encode programs. 
      (setq decode-program (make-8-bit-generate-decode-program decode-table)
	    encode-program (make-8-bit-generate-encode-program
			    decode-table encode-table encode-failure-octet))

      ;; And return the generated code. 
      `(let ((encode-table-sym (gentemp (format "%s-encode-table" ',name)))
             ;; The case-fold-search bind shouldn't be necessary. If I take
             ;; it, out, though, I get:
             ;; 
             ;; (invalid-read-syntax "Multiply defined symbol label" 1)
             ;;
             ;; when the file is byte compiled.
             (case-fold-search t))
        (define-translation-hash-table encode-table-sym ,encode-table)
        (make-coding-system 
         ',name 'ccl ,description
         (plist-put (plist-put ',props 'decode 
                               ,(apply #'vector decode-program))
                    'encode
                    (apply #'vector
                           (nsublis
                            (list (cons
                                   'encode-table-sym
                                   (symbol-value 'encode-table-sym)))
                            ',encode-program))))
        (coding-system-put ',name 'category ',
                           (make-8-bit-choose-category decode-table))
        ,(macroexpand `(loop for alias in ',aliases
                        do (define-coding-system-alias alias
                             ',name)))
        (find-coding-system ',name)))))
