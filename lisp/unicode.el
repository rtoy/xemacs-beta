;;; unicode.el --- Unicode support

;; Copyright (C) 2001, 2002, 2005, 2010 Ben Wing.

;; Keywords: multilingual, Unicode

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Lisp support for Unicode, e.g. initialize the translation tables.

;; Needs to load before mule-charset.el in old-Mule because the code below
;; that deals with Windows Glyph List 4 characters needs Asian charsets not
;; yet created (or more properly, their Unicode maps not yet initialized).

;;; Code:

;; Initialize the Unicode translation tables for some built-in charsets.
;; Currently this only does ISO 8859-1 (Latin-1).  Other charsets defined
;; in C either have no translation map or are special-cased in the code,
;; and internal charsets defined in Lisp specify their own maps.
(when (featurep 'mule)
  (let ((parse-args
	 '(;; Due to the braindamaged way Mule treats the ASCII and Control-1
	   ;; charsets' types, trying to load them results in out-of-range
	   ;; warnings at unicode.c:1439.  They're no-ops anyway, they're
	   ;; hardwired in unicode.c (unicode_to_ichar, ichar_to_unicode).
	   ;; (ascii "unicode/unicode-consortium/ISO8859/8859-1.TXT"
	   ;;        #x00 #x7F #x0)
	   ;; (control-1 "unicode/unicode-consortium/ISO8859/8859-1.TXT"
	   ;;   	 #x80 #x9F #x-80)
	   ;; The 8859-1.TXT G1 assignments are half no-ops, hardwired in
	   ;; unicode.c ichar_to_unicode, but not in unicode_to_ichar.
	   (latin-iso8859-1 "unicode/unicode-consortium/ISO8859/8859-1.TXT"
			    #xA0)
	   )))
    (mapc #'(lambda (args)
	      (apply 'load-unicode-mapping-table
		     (expand-file-name (cadr args)
				       (expand-file-name "etc"
							 source-directory))
		     (car args) (cddr args)))
	  parse-args)
    ))

(defun initialize-default-unicode-precedence-list
  ()
  ;; The default-unicode-precedence-list. We set this here to default to
  ;; *not* mapping various European characters to East Asian characters;
  ;; [[ otherwise the default-unicode-precedence-list is numerically ordered
  ;; by charset ID. ]] -- not true any more, order will be random since
  ;; we get it by mapping over a hash table.
  ;;
  ;; This must be run late, when all charsets have already been created.
  (declare-fboundp
   (set-default-unicode-precedence-list
    `(ascii control-1 latin-iso8859-1
      latin/list iso8859
      ,@(when (not (featurep 'unicode-internal)) '(jit-ucs-charset-0))
      windows-glyph-list-4
      japanese-kanji/list
      chinese/list
      ))))

(defun decode-char (quote-ucs code &optional restriction) 
  "FSF compatibility--return Mule character with Unicode codepoint CODE.
The second argument must be 'ucs, the third argument is ignored.  "
  ;; We're prepared to accept invalid Unicode in unicode-to-char, but not in
  ;; this function, which is the API that should actually be used, since
  ;; it's available in GNU and in Mule-UCS.
  (check-argument-range code #x0 #x10FFFF)
  (assert (eq quote-ucs 'ucs) t
	  "Sorry, decode-char doesn't yet support anything but the UCS.  ")
  (unicode-to-char code))

(defun encode-char (char quote-ucs &optional restriction)
  "FSF compatibility--return the Unicode code point of CHAR.
The second argument must be 'ucs, the third argument is ignored.  "
  (assert (eq quote-ucs 'ucs) t
	  "Sorry, encode-char doesn't yet support anything but the UCS.  ")
  (char-to-unicode char))

(make-coding-system
 'utf-16 'unicode
 "UTF-16"
 '(mnemonic "UTF-16"
   documentation 
   "UTF-16 Unicode encoding -- the standard (almost-) fixed-width
two-byte encoding, with surrogates.  It will be fixed-width if all
characters are in the BMP (Basic Multilingual Plane -- first 65536
codepoints).  Cannot represent characters with codepoints above
0x10FFFF (a little more than 1,000,000).  Unicode and ISO guarantee
never to encode any characters outside this range -- all the rest are
for private, corporate or internal use."
   unicode-type utf-16))

(define-coding-system-alias 'utf-16-be 'utf-16) 

(make-coding-system
 'utf-16-bom 'unicode
 "UTF-16 w/BOM"
 '(mnemonic "UTF16-BOM"
   documentation 
   "UTF-16 Unicode encoding with byte order mark (BOM) at the beginning.
The BOM is Unicode character U+FEFF -- i.e. the first two bytes are
0xFE and 0xFF, respectively, or reversed in a little-endian
representation.  It has been sanctioned by the Unicode Consortium for
use at the beginning of a Unicode stream as a marker of the byte order
of the stream, and commonly appears in Unicode files under Microsoft
Windows, where it also functions as a magic cookie identifying a
Unicode file.  The character is called \"ZERO WIDTH NO-BREAK SPACE\"
and is suitable as a byte-order marker because:

  -- it has no displayable representation
  -- due to its semantics it never normally appears at the beginning
     of a stream
  -- its reverse U+FFFE is not a legal Unicode character
  -- neither byte sequence is at all likely in any other standard
     encoding, particularly at the beginning of a stream

This coding system will insert a BOM at the beginning of a stream when
writing and strip it off when reading."
   unicode-type utf-16
   need-bom t))

(make-coding-system
 'utf-16-little-endian 'unicode
 "UTF-16 Little Endian"
 '(mnemonic "UTF16-LE"
   documentation
   "Little-endian version of UTF-16 Unicode encoding.
See `utf-16' coding system."
   unicode-type utf-16
   little-endian t))

(define-coding-system-alias 'utf-16-le 'utf-16-little-endian) 

(make-coding-system
 'utf-16-little-endian-bom 'unicode
 "UTF-16 Little Endian w/BOM"
 '(mnemonic "MSW-Unicode"
   documentation
   "Little-endian version of UTF-16 Unicode encoding, with byte order mark.
Standard encoding for representing Unicode under MS Windows.  See
`utf-16-bom' coding system."
   unicode-type utf-16
   little-endian t
   need-bom t))

(make-coding-system
 'ucs-4 'unicode
 "UCS-4"
 '(mnemonic "UCS4"
   documentation
   "UCS-4 Unicode encoding -- fully fixed-width four-byte encoding."
   unicode-type ucs-4))

(make-coding-system
 'ucs-4-little-endian 'unicode
 "UCS-4 Little Endian"
 '(mnemonic "UCS4-LE"
   documentation
   ;; #### I don't think this is permitted by ISO 10646, only Unicode.
   ;; Call it UTF-32 instead?
   "Little-endian version of UCS-4 Unicode encoding.  See `ucs-4' coding system."
   unicode-type ucs-4
   little-endian t))

(make-coding-system
 'utf-32 'unicode
 "UTF-32"
 '(mnemonic "UTF32"
   documentation
   "UTF-32 Unicode encoding -- fixed-width four-byte encoding,
characters less than #x10FFFF are not supported.  "
   unicode-type utf-32))

(make-coding-system
 'utf-32-little-endian 'unicode
 "UTF-32 Little Endian"
 '(mnemonic "UTF32-LE"
   documentation
   "Little-endian version of UTF-32 Unicode encoding.

A fixed-width four-byte encoding, characters less than #x10FFFF are not
supported.  "
   unicode-type ucs-4 little-endian t))

;; Now defined in unicode.c.

;;(make-coding-system
;; 'utf-8 'unicode
;; "UTF-8"
;; '(mnemonic "UTF8"
;;   documentation "..."
;;   unicode-type utf-8))

(make-coding-system
 'utf-8-bom 'unicode
 "UTF-8 w/BOM"
 '(mnemonic "MSW-UTF8"
   documentation
   "UTF-8 Unicode encoding, with byte order mark.
Standard encoding for representing UTF-8 under MS Windows."
   unicode-type utf-8
   little-endian t
   need-bom t))

(when (featurep 'mule)
  (make-charset 'windows-glyph-list-4
		"Windows Glyph List 4 etc."
		'(dimension 1
		  offset 128
		  chars 128))

  ;; We don't want the characters in Windows Glyph List 4 (and a few
  ;; others) to map to East Asian character sets when they are displayed.
  ;; WGL4 is a character repertoire from Microsoft that gives a guideline
  ;; for font implementors as to what characters are sufficient for
  ;; pan-European support.  East Asian fonts generally clash strongly with
  ;; European characters both in font choice and character width, and since
  ;; the WGL 4 characters are basically European characters, we want them
  ;; displayed with European fonts.  So create a special charset
  ;; `windows-glyph-list-4' that we put higher in the default Unicode
  ;; precedence list than any Asian charsets. (Under old-Mule, these
  ;; characters will also end up in jit-ucs-charset-0, which likewise we
  ;; put higher than the Asian charsets.)
  ;; 
  (loop for (ucs ascii-or-latin-1)
    in '((#x2013 ?-) ;; U+2013 EN DASH
	 (#x2014 ?-) ;; U+2014 EM DASH
	 (#x2105 ?%) ;; U+2105 CARE OF
	 (#x203e ?-) ;; U+203E OVERLINE
	 (#x221f ?|) ;; U+221F RIGHT ANGLE
	 (#x2584 ?|) ;; U+2584 LOWER HALF BLOCK
	 (#x2588 ?|) ;; U+2588 FULL BLOCK
	 (#x258c ?|) ;; U+258C LEFT HALF BLOCK
	 (#x2550 ?|) ;; U+2550 BOX DRAWINGS DOUBLE HORIZONTAL
	 (#x255e ?|) ;; U+255E BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
	 (#x256a ?|) ;; U+256A BOX DRAWINGS VERTICAL SINGLE & HORIZONTAL DOUBLE
	 (#x2561 ?|) ;; U+2561 BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
	 (#x2215 ?/) ;; U+2215 DIVISION SLASH
	 (#x02c9 ?`) ;; U+02C9 MODIFIER LETTER MACRON
	 (#x2211 ?s) ;; U+2211 N-ARY SUMMATION
	 (#x220f ?s) ;; U+220F N-ARY PRODUCT
	 (#x2248 ?=) ;; U+2248 ALMOST EQUAL TO
	 (#x2264 ?=) ;; U+2264 LESS-THAN OR EQUAL TO
	 (#x2265 ?=) ;; U+2265 GREATER-THAN OR EQUAL TO
	 (#x201c ?') ;; U+201C LEFT DOUBLE QUOTATION MARK
	 (#x2026 ?.) ;; U+2026 HORIZONTAL ELLIPSIS
	 (#x2212 ?-) ;; U+2212 MINUS SIGN
	 (#x2260 ?=) ;; U+2260 NOT EQUAL TO
	 (#x221e ?=) ;; U+221E INFINITY
	 (#x2642 ?=) ;; U+2642 MALE SIGN
	 (#x2640 ?=) ;; U+2640 FEMALE SIGN
	 (#x2032 ?=) ;; U+2032 PRIME
	 (#x2033 ?=) ;; U+2033 DOUBLE PRIME
	 (#x25cb ?=) ;; U+25CB WHITE CIRCLE
	 (#x25cf ?=) ;; U+25CF BLACK CIRCLE
	 (#x25a1 ?=) ;; U+25A1 WHITE SQUARE
	 (#x25a0 ?=) ;; U+25A0 BLACK SQUARE
	 (#x25b2 ?=) ;; U+25B2 BLACK UP-POINTING TRIANGLE
	 (#x25bc ?=) ;; U+25BC BLACK DOWN-POINTING TRIANGLE
	 (#x2192 ?=) ;; U+2192 RIGHTWARDS ARROW
	 (#x2190 ?=) ;; U+2190 LEFTWARDS ARROW
	 (#x2191 ?=) ;; U+2191 UPWARDS ARROW
	 (#x2193 ?=) ;; U+2193 DOWNWARDS ARROW
	 (#x2229 ?=) ;; U+2229 INTERSECTION
	 (#x2202 ?=) ;; U+2202 PARTIAL DIFFERENTIAL
	 (#x2261 ?=) ;; U+2261 IDENTICAL TO
	 (#x221a ?=) ;; U+221A SQUARE ROOT
	 (#x222b ?=) ;; U+222B INTEGRAL
	 (#x2030 ?=) ;; U+2030 PER MILLE SIGN
	 (#x266a ?=) ;; U+266A EIGHTH NOTE
	 (#x2020 ?*) ;; U+2020 DAGGER
	 (#x2021 ?*) ;; U+2021 DOUBLE DAGGER
	 (#x2500 ?|) ;; U+2500 BOX DRAWINGS LIGHT HORIZONTAL
	 (#x2502 ?|) ;; U+2502 BOX DRAWINGS LIGHT VERTICAL
	 (#x250c ?|) ;; U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
	 (#x2510 ?|) ;; U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
	 (#x2518 ?|) ;; U+2518 BOX DRAWINGS LIGHT UP AND LEFT
	 (#x2514 ?|) ;; U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
	 (#x251c ?|) ;; U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
	 (#x252c ?|) ;; U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
	 (#x2524 ?|) ;; U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
	 (#x2534 ?|) ;; U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
	 (#x253c ?|) ;; U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
	 (#x02da ?^) ;; U+02DA RING ABOVE
	 (#x2122 ?\xa9)	;; U+2122 TRADE MARK SIGN, ?©

	 (#x0132 ?\xe6)	;; U+0132 LATIN CAPITAL LIGATURE IJ, ?æ
	 (#x013f ?\xe6)	;; U+013F LATIN CAPITAL LETTER L WITH MIDDLE DOT, ?æ

	 (#x0133 ?\xe6)	;; U+0133 LATIN SMALL LIGATURE IJ, ?æ
	 (#x0140 ?\xe6)	;; U+0140 LATIN SMALL LETTER L WITH MIDDLE DOT, ?æ
	 (#x0149 ?\xe6)	;; U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPH,?æ

	 (#x2194 ?|) ;; U+2194 LEFT RIGHT ARROW
	 (#x2660 ?*) ;; U+2660 BLACK SPADE SUIT
	 (#x2665 ?*) ;; U+2665 BLACK HEART SUIT
	 (#x2663 ?*) ;; U+2663 BLACK CLUB SUIT
	 (#x2592 ?|) ;; U+2592 MEDIUM SHADE
	 (#x2195 ?|) ;; U+2195 UP DOWN ARROW

	 (#x2113 ?\xb9)	;; U+2113 SCRIPT SMALL L, ?¹
	 (#x215b ?\xbe)	;; U+215B VULGAR FRACTION ONE EIGHTH, ?¾
	 (#x215c ?\xbe)	;; U+215C VULGAR FRACTION THREE EIGHTHS, ?¾
	 (#x215d ?\xbe)	;; U+215D VULGAR FRACTION FIVE EIGHTHS, ?¾
	 (#x215e ?\xbe)	;; U+215E VULGAR FRACTION SEVEN EIGHTHS, ?¾
	 (#x207f ?\xbe)	;; U+207F SUPERSCRIPT LATIN SMALL LETTER N, ?¾
  
	 ;; These are not in WGL 4, but are IPA characters that should not
	 ;; be double width. They are the only IPA characters that both
	 ;; occur in packages/mule-packages/leim/ipa.el and end up in East
	 ;; Asian character sets when that file is loaded in an XEmacs
	 ;; without packages.
	 (#x2197 ?|) ;; U+2197 NORTH EAST ARROW
	 (#x2199 ?|) ;; U+2199 SOUTH WEST ARROW
	 (#x2191 ?|) ;; U+2191 UPWARDS ARROW
	 (#x207f ?\xb9)) ;; U+207F SUPERSCRIPT LATIN SMALL LETTER N, ?¹
    with decoded = nil
    with codepoint = #x80
    with syntax-table = (standard-syntax-table)
    initially (unless (featurep 'mule) (return))
    do
    ;; Under old-Mule, the call to `decode-char' creates jit-ucs-charset-0
    ;; entries because it is run at dump time, before we create any
    ;; charsets containing Asian characters. (That happens in
    ;; mule-charset.el.) It doesn't matter whether we move the call to
    ;; (set-unicode-conversion code 'windows-glyph-list-4 ...) before the
    ;; call to `decode-char', because windows-glyph-list-4, being size 128,
    ;; is a non-encodable charset.  By putting jit-ucs-charset-0 above the
    ;; Asian charsets, we ensure that the properties of jit-ucs-charset-0
    ;; determine how the characters are represented.
    ;;
    ;; Under Unicode-internal, we get the same behavior by putting
    ;; windows-glyph-list-4 before the Asian charsets.
    ;;
    (set-unicode-conversion ucs 'windows-glyph-list-4 codepoint)
    (setq decoded (decode-char 'ucs ucs))
    (assert (memq (declare-fboundp (char-charset decoded))
		  '(jit-ucs-charset-0 windows-glyph-list-4))
	    nil "Unexpected Unicode decoding behavior, actual charset=%s.  "
	    (char-charset decoded))
    (incf codepoint)
    (modify-syntax-entry decoded
			 (string 
			  (char-syntax ascii-or-latin-1))
			 syntax-table))
  )

;; *Sigh*, declarations needs to be at the start of the line to be picked up
;; by make-docfile.

;; Create all the Unicode error sequences, normally as jit-ucs-charset-0
;; characters starting at U+200000 (which isn't a valid Unicode code
;; point). Make them available to user code. 
(defvar unicode-error-default-translation-table
  (loop 
    with char-table = (make-char-table 'generic)
    for i from ?\x00 to ?\xFF
    initially (unless (featurep 'mule) (return))
    do
    (put-char-table (aref
                     ;; #xd800 is the first leading surrogate;
                     ;; trailing surrogates must be in the range
                     ;; #xdc00-#xdfff. These examples are not, so we
                     ;; intentionally provoke an error sequence.
                     (decode-coding-string (format "\xd8\x00\x00%c" i)
                                           'utf-16-be)
                     3)
                    i
                    char-table)
    finally return char-table)
  "Translation table mapping Unicode error sequences to Latin-1 chars.

To transform XEmacs Unicode error sequences to the Latin-1 characters that
correspond to the octets on disk, you can use this variable.  ")

(defvar unicode-invalid-sequence-regexp-range
  (and (featurep 'mule)
       (format "%c%c-%c"
               (aref (decode-coding-string "\xd8\x00\x00\x00" 'utf-16-be) 0)
               (aref (decode-coding-string "\xd8\x00\x00\x00" 'utf-16-be) 3)
               (aref (decode-coding-string "\xd8\x00\x00\xFF" 'utf-16-be) 3)))
  "Regular expression range to match Unicode error sequences in XEmacs.

Invalid Unicode sequences on input are represented as XEmacs
characters with values stored as the keys in
`unicode-error-default-translation-table', one character for each
invalid octet.  You can use this variable (with `re-search-forward' or
`skip-chars-forward') to search for such characters; see also
`unicode-error-translate-region'.  ")

;; Check that the lookup table is correct, and that all the actual error
;; sequences are caught by the regexp.
(with-temp-buffer
  (loop
    for i from ?\x00 to ?\xFF
    with to-check = (make-string 20 ?\x20) 
    initially (unless (featurep 'mule) (return))
    do 
    (delete-region (point-min) (point-max))
    (insert to-check)
    (goto-char 10)
    (insert (decode-coding-string (format "\xd8\x00\x00%c" i)
                                  'utf-16-be))
    (backward-char)
    (assert (= i (get-char-table (char-after (point)) 
                                 unicode-error-default-translation-table))
            (format "Char ?\\x%x not the expected error sequence!"
                    i))

    (goto-char (point-min))
    ;; Comment out until the issue in
    ;; 18179.49815.622843.336527@parhasard.net is fixed.
    (assert t ; (re-search-forward (concat "[" 
              ;                        unicode-invalid-sequence-regexp-range
              ;                        "]"))
            nil
            (format "Could not find char ?\\x%x in buffer" i))))

(defun frob-unicode-errors-region (frob-function begin end &optional buffer)
  "Call FROB-FUNCTION on the Unicode error sequences between BEGIN and END.

Optional argument BUFFER specifies the buffer that should be examined for
such sequences.  "
  (check-argument-type #'functionp frob-function)
  (check-argument-range begin (point-min buffer) (point-max buffer))
  (check-argument-range end (point-min buffer) (point-max buffer))
    (save-excursion
      (save-restriction
	(if buffer (set-buffer buffer))
	(narrow-to-region begin end)
	(goto-char (point-min))
	(while end
	  (setq begin
		(progn
		  (skip-chars-forward
		   (concat "^" unicode-invalid-sequence-regexp-range))
		  (point))
		end (and (not (= (point) (point-max)))
			 (progn
			   (skip-chars-forward
			    unicode-invalid-sequence-regexp-range)
			   (point))))
	  (if end
	      (funcall frob-function begin end))))))

(defun unicode-error-translate-region (begin end &optional buffer table)
  "Translate the Unicode error sequences in BUFFER between BEGIN and END.

The error sequences are transformed, by default, into the ASCII,
control-1 and latin-iso8859-1 characters with the numeric values
corresponding to the incorrect octets encountered.  This is achieved
by using `unicode-error-default-translation-table' (which see) for
TABLE; you can change this by supplying another character table,
mapping from the error sequences to the desired characters.  "
    (unless table (setq table unicode-error-default-translation-table))
    (frob-unicode-errors-region
     (lambda (start finish)
       (translate-region start finish table))
     begin end buffer))

;; Sure would be nice to be able to use defface here. 
(copy-face 'highlight 'unicode-invalid-sequence-warning-face)

(unless (featurep 'mule)
  ;; We do this in such a roundabout way--instead of having the above defun
  ;; and defvar calls inside a (when (featurep 'mule) ...) form--to have
  ;; make-docfile.c pick up symbol and function documentation correctly. An
  ;; alternative approach would be to fix make-docfile.c to be able to read
  ;; Lisp.
  (mapc #'unintern
        '(unicode-error-default-translation-table
          unicode-invalid-regexp-range frob-unicode-errors-region
          unicode-error-translate-region unicode-query-coding-region
          unicode-query-coding-skip-chars-arg)))

(make-obsolete 'char-octet 'char-to-charset-codepoint)
(make-obsolete 'split-char 'char-to-charset-codepoint)

;; #### UTF-7 is not yet implemented, and it's tricky to do.  There's
;; an implementation in appendix A.1 of the Unicode Standard, Version
;; 2.0, but I don't know its licensing characteristics.

; (make-coding-system
;  'utf-7 'unicode
;  "UTF-7"
;  '(mnemonic "UTF7"
;    documentation;    "UTF-7 Unicode encoding -- 7-bit-ASCII modal Internet-mail-compatible
; encoding especially designed for headers, with the following
; properties:

;   -- Only characters that are considered safe for passing through any mail
;      gateway without damage are used.

;   -- This is a modal encoding, with two states.  The first, default
;      state encodes the most common Unicode characters (upper and
;      lowercase letters, digits, and 9 common punctuation marks) as
;      themselves, and the second state, entered using '+' and
;      terminated with '-' or any character disallowed in state 2,
;      encodes any Unicode characters by first converting to UTF-16,
;      most significant byte first, and then to a slightly modified
;      Base64 encoding. (Thus, UTF-7 has the same limitations on the
;      characters it can encode as UTF-16.)

;   -- The modified Base64 encoding deviates from standard Base64 in
;      that it omits the `=' pad character.  This is eliminated so as to
;      avoid conflicts with the use of `=' as an escape in the
;      Quoted-Printable encoding and the related Q encoding for headers:
;      With this modification, non-whitespace chars in UTF-7 will be
;      represented in Quoted-Printable and in Q as-is, with no further
;      encoding.

; For more information, see Appendix A.1 of The Unicode Standard 2.0, or
; wherever it is in v3.0."
;    unicode-type utf-7))
