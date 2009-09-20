;;; make-coding-system.el; Provides the #'make-coding-system function and
;;; much of the implementation of the fixed-width coding system type.

;; Copyright (C) 2009 Free Software Foundation

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

(defvar fixed-width-private-use-start (decode-char 'ucs #xE000)
  "Start of a 256 code private use area for fixed-width coding systems.

This is used to ensure that distinct octets on disk for a given coding
system map to distinct XEmacs characters, preventing spurious changes when
a file is read, not changed, and then written.  ")

(defun fixed-width-generate-helper (decode-table encode-table
				   encode-failure-octet)
  "Helper func, `fixed-width-generate-encode-program-and-skip-chars-strings',
which see.

Deals with the case where ASCII and another character set can both be
encoded unambiguously and completely into the coding-system; if this is so,
returns multiple values comprisig of such a ccl-program and the character
set in question.  If not, it returns nil."
  (let ((tentative-encode-program-parts
	 (eval-when-compile 
	   (let* ((vec-len 128)
		  (compiled 
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
				((r0 = r1 & #x7F)
				 (write r0 ,(make-vector vec-len #xBFFE)))
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
	       (when (eq #xBFFE (cadr compiled))
		 (assert (= vec-len (search '(#xBFFE) (cdr compiled)
					    :test #'/=)) nil
					    "Strange ccl vector length")
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
	     (list first-part last-part vec-len))))
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
      (setq other-charset-vector
	    (make-vector (third tentative-encode-program-parts)
			 encode-failure-octet))
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
    (and encode-program (values encode-program worth-trying))))

(defun fixed-width-generate-encode-program-and-skip-chars-strings
  (decode-table encode-table encode-failure-octet)
  "Generate a CCL program to encode a 8-bit fixed-width charset.

DECODE-TABLE must have 256 non-cons entries, and will be regarded as
describing a map from the octet corresponding to an offset in the
table to the that entry in the table.  ENCODE-TABLE is a hash table
map from unicode values to characters in the range [0,255].
ENCODE-FAILURE-OCTET describes an integer between 0 and 255
\(inclusive) to write in the event that a character cannot be encoded."
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
         (ascii-encodes-as-itself nil)
         (control-1-encodes-as-itself t)
         (invalid-sequence-code-point-start
          (eval-when-compile
            (char-to-unicode
             (aref (decode-coding-string "\xd8\x00\x00\x00" 'utf-16-be) 3))))
         further-char-set skip-chars invalid-sequences-skip-chars)

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
      (multiple-value-setq
          (encode-program further-char-set)
        ;; Encode program with ascii-ascii mapping (based on a
        ;; character's mule character set), and one other mule
        ;; character set using table-based encoding, other
        ;; character sets using hash table lookups.
        ;; fixed-width-non-ascii-completely-coveredp only returns
        ;; such a mapping if some non-ASCII charset with
        ;; characters in decode-table is entirely covered by
        ;; encode-table.
        (fixed-width-generate-helper decode-table encode-table
                                    encode-failure-octet))
      (unless encode-program
	;; If fixed-width-non-ascii-completely-coveredp returned nil,
	;; but ASCII still encodes as itself, do one-to-one mapping
	;; for ASCII, and a hash table lookup for everything else.
	(setq encode-program encode-program-with-ascii-optimisation)))

    (setq encode-program
          (nsublis
           (list (cons #xBEEF14 
                       (logior (lsh encode-failure-octet 8)
                               #x14)))
           (copy-tree encode-program)))
    (loop
      for i from #x80 to #x9f
      do (unless (= i (aref decode-table i))
           (setq control-1-encodes-as-itself nil)
           (return)))
    (loop
      for i from #x00 to #xFF
      initially (setq skip-chars
                      (cond
                       ((and ascii-encodes-as-itself
                             control-1-encodes-as-itself further-char-set)
                        (concat "\x00-\x9f" (charset-skip-chars-string
                                             further-char-set)))
                       ((and ascii-encodes-as-itself
                             control-1-encodes-as-itself)
                        "\x00-\x9f")
                       ((null ascii-encodes-as-itself)
                        (skip-chars-quote (apply #'string
                                                 (append decode-table nil))))
                       (further-char-set
                        (concat (charset-skip-chars-string 'ascii)
                                (charset-skip-chars-string further-char-set)))
                       (t 
                        (charset-skip-chars-string 'ascii)))
                      invalid-sequences-skip-chars "")
      with decoded-ucs = nil
      with decoded = nil
      with no-ascii-transparency-skip-chars-list = 
           (unless ascii-encodes-as-itself (append decode-table nil))
      ;; Can't use #'match-string here, see:
      ;; http://mid.gmane.org/18829.34118.709782.704574@parhasard.net
      with skip-chars-test = 
           #'(lambda (skip-chars-string testing)
               (with-temp-buffer
                 (insert testing)
                 (goto-char (point-min))
                 (skip-chars-forward skip-chars-string)
                 (= (point) (point-max))))
      do
      (setq decoded (aref decode-table i)
            decoded-ucs (char-to-unicode decoded))
      (cond
       ((<= invalid-sequence-code-point-start decoded-ucs
            (+ invalid-sequence-code-point-start #xFF))
        (setq invalid-sequences-skip-chars
              (concat (string decoded)
                      invalid-sequences-skip-chars))
        (assert (not (funcall skip-chars-test skip-chars decoded))
                "This char should only be skipped with \
`invalid-sequences-skip-chars', not by `skip-chars'"))
       ((not (funcall skip-chars-test skip-chars decoded))
        (if ascii-encodes-as-itself
            (setq skip-chars (concat skip-chars (string decoded)))
          (push decoded no-ascii-transparency-skip-chars-list))))
      finally (unless ascii-encodes-as-itself
                (setq skip-chars
                      (skip-chars-quote
                       (apply #'string
                              no-ascii-transparency-skip-chars-list)))))
    (values encode-program skip-chars invalid-sequences-skip-chars)))

(defun fixed-width-create-decode-encode-tables (unicode-map)
  "Return multiple values \(DECODE-TABLE ENCODE-TABLE) given UNICODE-MAP. 
UNICODE-MAP should be an alist mapping from integer octet values to
characters with UCS code points; DECODE-TABLE will be a 256-element
vector, and ENCODE-TABLE will be a hash table mapping from 256 numbers
to 256 distinct characters."
  (check-argument-type #'listp unicode-map)
  (let ((decode-table (make-vector 256 nil))
        (encode-table (make-hash-table :size 256))
	(private-use-start (encode-char fixed-width-private-use-start 'ucs))
        (invalid-sequence-code-point-start
         (eval-when-compile
           (char-to-unicode
            (aref (decode-coding-string "\xd8\x00\x00\x00" 'utf-16-be) 3))))
	desired-ucs decode-table-entry)

    (loop for (external internal)
      in unicode-map
      do
      (aset decode-table external internal)
      (assert (not (eq (encode-char internal 'ucs) -1))
	      nil
	      "Looks like you're creating a fixed-width coding system \
in a dumped file, \nand you're either not providing a literal unicode map
or PROPS. Don't do that; fixed-width coding systems rely on sensible
Unicode mappings being available, which they are at compile time for
dumped files (but this requires the mentioned literals), but not, for
most of them, at run time.  ")

      (puthash (encode-char internal 'ucs)
	       ;; This is semantically an integer, but Dave Love's design
	       ;; for lookup-integer in CCL means we need to store it as a
	       ;; character.
	       (int-to-char external)
	       encode-table))

    ;; Now, go through the decode table. For octet values above #x7f, if the
    ;; decode table entry is nil, this means that they have an undefined
    ;; mapping (= they map to XEmacs characters with keys in
    ;; unicode-error-default-translation-table); for octet values below or
    ;; equal to #x7f, it means that they map to ASCII.

    ;; If any entry (whether below or above #x7f) in the decode-table
    ;; already maps to some character with a key in
    ;; unicode-error-default-translation-table, it is treated as an
    ;; undefined octet by `query-coding-region'. That is, it is not
    ;; necessary for an octet value to be above #x7f for this to happen.

    (dotimes (i 256)
      (setq decode-table-entry (aref decode-table i))
      (if decode-table-entry
          (when (get-char-table
                 decode-table-entry
                 unicode-error-default-translation-table)
            ;; The caller is explicitly specifying that this octet
            ;; corresponds to an invalid sequence on disk:
            (assert (= (get-char-table
                        decode-table-entry
                        unicode-error-default-translation-table) i)
                    "Bad argument for a fixed-width coding system.
If you're going to designate an octet with value below #x80 as invalid
for this coding system, make sure to map it to the invalid sequence
character corresponding to its octet value on disk. "))

        ;; decode-table-entry is nil; either the octet is to be treated as
        ;; contributing to an error sequence (when (> #x7f i)), or it should
        ;; be attempted to treat it as ASCII-equivalent. 
        (setq desired-ucs (or (and (< i #x80) i)
                              (+ invalid-sequence-code-point-start i)))
        (while (gethash desired-ucs encode-table)
          (assert (not (< i #x80))
                  "UCS code point should not already be in encode-table!"
                  ;; There is one invalid sequence char per octet value;
                  ;; with fixed-width coding systems, it makes no sense
                  ;; for us to be multiply allocating them.
                  (gethash desired-ucs encode-table))
          (setq desired-ucs (+ private-use-start desired-ucs)
                private-use-start (+ private-use-start 1)))
        (puthash desired-ucs (int-to-char i) encode-table)
        (setq desired-ucs (if (> desired-ucs #xFF)
                              (unicode-to-char desired-ucs)
                            ;; So we get Latin-1 when run at dump time,
                            ;; instead of JIT-allocated characters.
                            (int-to-char desired-ucs)))
        (aset decode-table i desired-ucs)))
    (values decode-table encode-table)))

(defun fixed-width-generate-decode-program (decode-table)
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

(defun fixed-width-choose-category (decode-table)
  "Given DECODE-TABLE, return an appropriate coding category.
DECODE-TABLE is a 256-entry vector describing the mapping from octets on
disk to XEmacs characters for some fixed-width 8-bit coding system."
  (check-argument-type #'vectorp decode-table)
  (check-argument-range (length decode-table) #x100 #x100)
  (loop
    named category
    for i from #x80 to #x9F
    do (unless (= i (aref decode-table i))
	 (return-from category 'no-conversion))
    finally return 'iso-8-1))

(defun fixed-width-rework-props-runtime (name props)
  "Rework PROPS to a form understood by `make-coding-system-internal'. 

NAME must be a symbol, describing a fixed-width coding system that is
about to be created.  Much of the implementation of the fixed-width
coding system is in Lisp, and this function allows us to rework the
arguments that `make-coding-system-internal' sees accordingly.

If you are calling this function from anywhere but
`make-coding-system', you're probably doing something wrong."
  (check-argument-type #'symbolp name)
  (check-valid-plist props)
  (let  ((encode-failure-octet (or (plist-get props 'encode-failure-octet)
				   (char-to-int ?~)))
         (unicode-map (plist-get props 'unicode-map))
	 (hash-table-sym (gensym (format "%s-encode-table" name)))
	 encode-program decode-program decode-table encode-table skip-chars
         invalid-sequences-skip-chars category)

    (check-argument-range encode-failure-octet 0 #xFF)
    ;; unicode-map must be a true list, and must be non-nil.
    (check-argument-type #'true-list-p unicode-map)
    (check-argument-type #'consp unicode-map)

    ;; Don't pass on our extra data to make-coding-system-internal.
    (setq props (plist-remprop props 'encode-failure-octet)
	  props (plist-remprop props 'unicode-map))

    (multiple-value-setq
	(decode-table encode-table)
      (fixed-width-create-decode-encode-tables unicode-map))

    ;; Register the decode-table. 
    (define-translation-hash-table hash-table-sym encode-table)

    ;; Generate the programs and skip-chars strings. 
    (setq decode-program (fixed-width-generate-decode-program decode-table))
    (multiple-value-setq
        (encode-program skip-chars invalid-sequences-skip-chars)
      (fixed-width-generate-encode-program-and-skip-chars-strings
       decode-table encode-table encode-failure-octet))

    (setq category (fixed-width-choose-category decode-table))

    (unless (vectorp encode-program) 
      (setq encode-program 
	    (apply #'vector
		   (nsublis (list (cons 'encode-table-sym hash-table-sym))
			    (copy-tree encode-program)))))
    (unless (vectorp decode-program)
      (setq decode-program
	    (apply #'vector decode-program)))

    (loop for (symbol . value)
      in `((decode . ,decode-program)
           (encode . ,encode-program)
           (from-unicode . ,encode-table)
           (query-skip-chars . ,skip-chars)
           (invalid-sequences-skip-chars . ,invalid-sequences-skip-chars)
           (category . ,category))
      with default = (gensym)
      do
      (unless (eq default (plist-get props symbol default))
        (error
         'invalid-argument
         "Explicit property not allowed for fixed-width coding systems"
         symbol))
      (setq props (nconc (list symbol value) props)))
    props))

;;;###autoload
(defun make-coding-system (name type &optional description props)
  "Register symbol NAME as a coding system.

TYPE describes the conversion method used and should be one of

nil or `undecided'
     Automatic conversion.  XEmacs attempts to detect the coding system
     used in the file.
`chain'
     Chain two or more coding systems together to make a combination coding
     system.
`no-conversion'
     No conversion.  Use this for binary files and such.  On output,
     graphic characters that are not in ASCII or Latin-1 will be
     replaced by a ?. (For a no-conversion-encoded buffer, these
     characters will only be present if you explicitly insert them.)
`convert-eol'
     Convert CRLF sequences or CR to LF.
`shift-jis'
     Shift-JIS (a Japanese encoding commonly used in PC operating systems).
`unicode'
     Any Unicode encoding (UCS-4, UTF-8, UTF-16, etc.).
`mswindows-unicode-to-multibyte'
     (MS Windows only) Converts from Windows Unicode to Windows Multibyte
     (any code page encoding) upon encoding, and the other way upon decoding.
`mswindows-multibyte'
     Converts to or from Windows Multibyte (any code page encoding).
     This is resolved into a chain of `mswindows-unicode' and
     `mswindows-unicode-to-multibyte'.
`iso2022'
     Any ISO2022-compliant encoding.  Among other things, this includes
     JIS (the Japanese encoding commonly used for e-mail), EUC (the
     standard Unix encoding for Japanese and other languages), and
     Compound Text (the encoding used in X11).  You can specify more
     specific information about the conversion with the PROPS argument.
`fixed-width'
     A fixed-width eight bit encoding that is not necessarily compliant with
     ISO 2022.  This coding system assumes Unicode equivalency, that is, if
     two given XEmacs characters have the same Unicode mapping, they will
     always map to the same octet on disk.
`big5'
     Big5 (the encoding commonly used for Mandarin Chinese in Taiwan).
`ccl'
     The conversion is performed using a user-written pseudo-code
     program.  CCL (Code Conversion Language) is the name of this
     pseudo-code.
`gzip'
     GZIP compression format.
`internal'
     Write out or read in the raw contents of the memory representing
     the buffer's text.  This is primarily useful for debugging
     purposes, and is only enabled when XEmacs has been compiled with
     DEBUG_XEMACS defined (via the --debug configure option).
     WARNING: Reading in a file using `internal' conversion can result
     in an internal inconsistency in the memory representing a
     buffer's text, which will produce unpredictable results and may
     cause XEmacs to crash.  Under normal circumstances you should
     never use `internal' conversion.

DESCRIPTION is a short English phrase describing the coding system,
suitable for use as a menu item. (See also the `documentation' property
below.)

PROPS is a property list, describing the specific nature of the
character set.  Recognized properties are:

`mnemonic'
     String to be displayed in the modeline when this coding system is
     active.

`documentation'
     Detailed documentation on the coding system.

`aliases'
     A list of aliases for the coding system.  See
     `define-coding-system-alias'.

`eol-type'
     End-of-line conversion to be used.  It should be one of

	nil
		Automatically detect the end-of-line type (LF, CRLF,
		or CR).  Also generate subsidiary coding systems named
		`NAME-unix', `NAME-dos', and `NAME-mac', that are
		identical to this coding system but have an EOL-TYPE
		value of `lf', `crlf', and `cr', respectively.
	`lf'
		The end of a line is marked externally using ASCII LF.
		Since this is also the way that XEmacs represents an
		end-of-line internally, specifying this option results
		in no end-of-line conversion.  This is the standard
		format for Unix text files.
	`crlf'
		The end of a line is marked externally using ASCII
		CRLF.  This is the standard format for MS-DOS text
		files.
	`cr'
		The end of a line is marked externally using ASCII CR.
		This is the standard format for Macintosh text files.
	t
		Automatically detect the end-of-line type but do not
		generate subsidiary coding systems.  (This value is
		converted to nil when stored internally, and
		`coding-system-property' will return nil.)

`post-read-conversion'
     The value is a function to call after some text is inserted and
     decoded by the coding system itself and before any functions in
     `after-change-functions' are called. (#### Not actually true in
     XEmacs. `after-change-functions' will be called twice if
     `post-read-conversion' changes something.) The argument of this
     function is the same as for a function in
     `after-insert-file-functions', i.e. LENGTH of the text inserted,
     with point at the head of the text to be decoded.

`pre-write-conversion'
     The value is a function to call after all functions in
     `write-region-annotate-functions' and `buffer-file-format' are
     called, and before the text is encoded by the coding system itself.
     The arguments to this function are the same as those of a function
     in `write-region-annotate-functions', i.e. FROM and TO, specifying
     a region of text.

The following properties are used by `default-query-coding-region',
the default implementation of `query-coding-region'. This
implementation and these properties are not used by the Unicode coding
systems, nor by fixed-width coding systems. 

`safe-chars'
     The value is a char table.  If a character has non-nil value in it,
     the character is safely supported by the coding system.  
     This overrides the `safe-charsets' property.
   
`safe-charsets'
     The value is a list of charsets safely supported by the coding
     system.  For coding systems based on ISO 2022, XEmacs may try to
     encode characters outside these character sets, but outside of
     East Asia and East Asian coding systems, it is unlikely that
     consumers of the data will understand XEmacs' encoding.
     The value t means that all XEmacs character sets handles are supported.  

The following properties are allowed for FSF compatibility but currently
ignored:

`translation-table-for-decode'
     The value is a translation table to be applied on decoding.  See
     the function `make-translation-table' for the format of translation
     table.  This is not applicable to CCL-based coding systems.
    
`translation-table-for-encode'
     The value is a translation table to be applied on encoding.  This is
     not applicable to CCL-based coding systems.
     
`mime-charset'
     The value is a symbol of which name is `MIME-charset' parameter of
     the coding system.
    
`valid-codes' (meaningful only for a coding system based on CCL)
     The value is a list to indicate valid byte ranges of the encoded
     file.  Each element of the list is an integer or a cons of integer.
     In the former case, the integer value is a valid byte code.  In the
     latter case, the integers specifies the range of valid byte codes.

The following additional property is recognized if TYPE is `convert-eol':

`subtype'
     One of `lf', `crlf', `cr' or nil (for autodetection).  When decoding,
     the corresponding sequence will be converted to LF.  When encoding,
     the opposite happens.  This coding system converts characters to
     characters.



The following additional properties are recognized if TYPE is `iso2022':

`charset-g0'
`charset-g1'
`charset-g2'
`charset-g3'
     The character set initially designated to the G0 - G3 registers.
     The value should be one of

          -- A charset object (designate that character set)
	  -- nil (do not ever use this register)
	  -- t (no character set is initially designated to
		the register, but may be later on; this automatically
		sets the corresponding `force-g*-on-output' property)

`force-g0-on-output'
`force-g1-on-output'
`force-g2-on-output'
`force-g2-on-output'
     If non-nil, send an explicit designation sequence on output before
     using the specified register.

`short'
     If non-nil, use the short forms \"ESC $ @\", \"ESC $ A\", and
     \"ESC $ B\" on output in place of the full designation sequences
     \"ESC $ ( @\", \"ESC $ ( A\", and \"ESC $ ( B\".

`no-ascii-eol'
     If non-nil, don't designate ASCII to G0 at each end of line on output.
     Setting this to non-nil also suppresses other state-resetting that
     normally happens at the end of a line.

`no-ascii-cntl'
     If non-nil, don't designate ASCII to G0 before control chars on output.

`seven'
     If non-nil, use 7-bit environment on output.  Otherwise, use 8-bit
     environment.

`lock-shift'
     If non-nil, use locking-shift (SO/SI) instead of single-shift
     or designation by escape sequence.

`no-iso6429'
     If non-nil, don't use ISO6429's direction specification.

`escape-quoted'
     If non-nil, literal control characters that are the same as
     the beginning of a recognized ISO2022 or ISO6429 escape sequence
     (in particular, ESC (0x1B), SO (0x0E), SI (0x0F), SS2 (0x8E),
     SS3 (0x8F), and CSI (0x9B)) are \"quoted\" with an escape character
     so that they can be properly distinguished from an escape sequence.
     (Note that doing this results in a non-portable encoding.) This
     encoding flag is used for byte-compiled files.  Note that ESC
     is a good choice for a quoting character because there are no
     escape sequences whose second byte is a character from the Control-0
     or Control-1 character sets; this is explicitly disallowed by the
     ISO2022 standard.

`input-charset-conversion'
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when decoding is performed.
     Each specification is a list of two elements: the source charset,
     and the destination charset.

`output-charset-conversion'
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when encoding is performed.
     The form of each specification is the same as for
     `input-charset-conversion'.

The following additional properties are recognized if TYPE is
`fixed-width':

`unicode-map'
     Required.  A plist describing a map from octets in the coding system
     NAME (as integers) to XEmacs characters.  Those XEmacs characters will
     be used explicitly on decoding, but for encoding (most relevantly, on
     writing to disk) XEmacs characters that map to the same Unicode code
     point will be unified.  This means that the ISO-8859-? characters that
     map to the same Unicode code point will not be distinct when written to
     disk, which is normally what is intended; it also means that East Asian
     Han characters from different XEmacs character sets will not be
     distinct when written to disk, which is less often what is intended.

     Any octets not mapped, and with values above #x7f, will be decoded into
     XEmacs characters that reflect that their values are undefined.  These
     characters will be displayed in a language-environment-specific
     way. See `unicode-error-default-translation-table' and the
     `invalid-sequence-coding-system' argument to `set-language-info'.

     These characters will normally be treated as invalid when checking
     whether text can be encoded with `query-coding-region'--see the
     IGNORE-INVALID-SEQUENCESP argument to that function to avoid this.  It
     is possible to specify that octets with values less than #x80 (or
     indeed greater than it) be treated in this way, by specifying
     explicitly that they correspond to the character mapping to that octet
     in `unicode-error-default-translation-table'.  Far fewer coding systems
     override the ASCII mapping, though, so this is not the default.

`encode-failure-octet'
     An integer between 0 and 255 to write in place of XEmacs characters
     that cannot be encoded, defaulting to the code for tilde `~'.

The following additional properties are recognized (and required)
if TYPE is `ccl':

`decode'
     CCL program used for decoding (converting to internal format).

`encode'
     CCL program used for encoding (converting to external format).


The following additional properties are recognized if TYPE is `chain':

`chain'
     List of coding systems to be chained together, in decoding order.

`canonicalize-after-coding'
     Coding system to be returned by the detector routines in place of
     this coding system.



The following additional properties are recognized if TYPE is `unicode':

`unicode-type'
     One of `utf-16', `utf-8', `ucs-4', or `utf-7' (the latter is not
     yet implemented).  `utf-16' is the basic two-byte encoding;
     `ucs-4' is the four-byte encoding; `utf-8' is an ASCII-compatible
     variable-width 8-bit encoding; `utf-7' is a 7-bit encoding using
     only characters that will safely pass through all mail gateways.
     [[ This should be \"transformation format\".  There should also be
     `ucs-2' (or `bmp' -- no surrogates) and `utf-32' (range checked). ]]

`little-endian'
     If non-nil, `utf-16' and `ucs-4' will write out the groups of two
     or four bytes little-endian instead of big-endian.  This is required,
     for example, under Windows.

`need-bom'
     If non-nil, a byte order mark (BOM, or Unicode FFFE) should be
     written out at the beginning of the data.  This serves both to
     identify the endianness of the following data and to mark the
     data as Unicode (at least, this is how Windows uses it).
     [[ The correct term is \"signature\", since this technique may also
     be used with UTF-8.  That is the term used in the standard. ]]


The following additional properties are recognized if TYPE is
`mswindows-multibyte':

`code-page'
     Either a number (specifying a particular code page) or one of the
     symbols `ansi', `oem', `mac', or `ebcdic', specifying the ANSI,
     OEM, Macintosh, or EBCDIC code page associated with a particular
     locale (given by the `locale' property).  NOTE: EBCDIC code pages
     only exist in Windows 2000 and later.

`locale'
     If `code-page' is a symbol, this specifies the locale whose code
     page of the corresponding type should be used.  This should be
     one of the following: A cons of two strings, (LANGUAGE
     . SUBLANGUAGE) (see `mswindows-set-current-locale'); a string (a
     language; SUBLANG_DEFAULT, i.e. the default sublanguage, is
     used); or one of the symbols `current', `user-default', or
     `system-default', corresponding to the values of
     `mswindows-current-locale', `mswindows-user-default-locale', or
     `mswindows-system-default-locale', respectively.


The following additional properties are recognized if TYPE is `undecided':
\[[ Doesn't GNU use \"detect-*\" for the following two? ]]

`do-eol'
     Do EOL detection.

`do-coding'
     Do encoding detection.

`coding-system'
     If encoding detection is not done, use the specified coding system
     to do decoding.  This is used internally when implementing coding
     systems with an EOL type that specifies autodetection (the default),
     so that the detector routines return the proper subsidiary.



The following additional property is recognized if TYPE is `gzip':

`level'
     Compression level: 0 through 9, or `default' (currently 6)."
  (when (eq 'fixed-width type)
    (setq props (fixed-width-rework-props-runtime name props)))
  (make-coding-system-internal name type description props))

(define-compiler-macro make-coding-system (&whole form name type
                                           &optional description props)
  (if (equal '(quote fixed-width) type)
      (if (memq (car-safe props) '(quote eval-when-compile))
          (let* ((props (if (eq 'eval-when-compile (car props))
                            (eval (cadr props))
                          (cadr props)))
                 (encode-failure-octet
                  (or (plist-get props 'encode-failure-octet)
                      (char-to-int ?~)))
                 (unicode-map (plist-get props 'unicode-map))
                 (default-plist-entry (gensym))
                 (encode-table-sym (gensym 
                                    (if (eq 'quote (car name))
                                        (format "%s-enc-" (second name)))))
                 encode-program decode-program
                 decode-table encode-table
                 skip-chars invalid-sequences-skip-chars category)

            (check-argument-range encode-failure-octet 0 #xFF)
            ;; unicode-map must be a true list, and must be non-nil.
            (check-argument-type #'true-list-p unicode-map)
            (check-argument-type #'consp unicode-map)

            ;; Don't pass on our extra data to make-coding-system-internal.
            (setq props (plist-remprop props 'encode-failure-octet)
                  props (plist-remprop props 'unicode-map))

            (multiple-value-setq
                (decode-table encode-table)
              (fixed-width-create-decode-encode-tables unicode-map))
    
            ;; Generate the decode and encode programs, and the skip-chars
            ;; arguments.
            (setq decode-program
                  (fixed-width-generate-decode-program decode-table)
                  category (fixed-width-choose-category decode-table))

            (multiple-value-setq
                (encode-program skip-chars invalid-sequences-skip-chars)
              (fixed-width-generate-encode-program-and-skip-chars-strings
               decode-table encode-table encode-failure-octet))

            (unless (vectorp decode-program)
              (setq decode-program
                    (apply #'vector decode-program)))

            (unless (eq default-plist-entry (plist-get props 'encode
                                                       default-plist-entry))
              (error
               'invalid-argument
               "Explicit property not allowed for fixed-width coding system"
               'encode))
            (loop for (symbol . value)
              in `((decode . ,decode-program)
                   (from-unicode . ,encode-table)
                   (query-skip-chars . ,skip-chars)
                   (invalid-sequences-skip-chars .
                                                 ,invalid-sequences-skip-chars)
                   (category . ,category))
              do
              (unless (eq default-plist-entry (plist-get props symbol
                                                         default-plist-entry))
                (error
                 'invalid-argument
                 "Explicit property not allowed for \
fixed-width coding systems"
                 symbol))
              (setq props (nconc (list symbol value) props)))
            `(progn
              (define-translation-hash-table ',encode-table-sym ,encode-table)
              (make-coding-system-internal
               ,name ,type ,description
               ',(nconc (list 'encode
                              (apply #'vector
                                     (nsublis
                                      (list (cons 'encode-table-sym
                                                  encode-table-sym))
                                      encode-program)))
                        props))))
        ;; The form does not use literals; call make-coding-system at
        ;; run time.
        form)
    (if (byte-compile-constp type)
        ;; This is not a fixed-width call; compile it to a form that 21.4
        ;; can also understand.
        `(funcall (or (and (fboundp 'make-coding-system-internal)
                           'make-coding-system-internal)
                      'make-coding-system)
          ,@(cdr form))
      ;; TYPE is not literal; work things out at runtime.
      form)))

