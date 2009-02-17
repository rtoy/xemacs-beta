;;; unicode.el --- Unicode support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002 Ben Wing.

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

;;; Code:

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

(eval-when-compile (when (featurep 'mule) (require 'ccl)))

;; accessed in loadup.el, mule-cmds.el; see discussion in unicode.c
(defvar load-unicode-tables-at-dump-time (eq system-type 'windows-nt)
  "[INTERNAL] Whether to load the Unicode tables at dump time.
Setting this at run-time does nothing.")

;; NOTE: This takes only a fraction of a second on my Pentium III
;; 700Mhz even with a totally optimization-disabled XEmacs.
(defun load-unicode-tables ()
  "Initialize the Unicode translation tables for all standard charsets."
  (let ((parse-args
	 '(("unicode/unicode-consortium"
	    ;; Due to the braindamaged way Mule treats the ASCII and Control-1
	    ;; charsets' types, trying to load them results in out-of-range
	    ;; warnings at unicode.c:1439.  They're no-ops anyway, they're
	    ;; hardwired in unicode.c (unicode_to_ichar, ichar_to_unicode).
	    ;; ("8859-1.TXT" ascii #x00 #x7F #x0)
	    ;; ("8859-1.TXT" control-1 #x80 #x9F #x-80)
            ;; The 8859-1.TXT G1 assignments are half no-ops, hardwired in
	    ;; unicode.c ichar_to_unicode, but not in unicode_to_ichar.
	    ("8859-1.TXT" latin-iso8859-1 #xA0 #xFF #x-80)
	    ;; "8859-10.TXT"
	    ;; "8859-13.TXT"
	    ("8859-14.TXT" latin-iso8859-14 #xA0 #xFF #x-80)
	    ("8859-15.TXT" latin-iso8859-15 #xA0 #xFF #x-80)
	    ("8859-16.TXT" latin-iso8859-16 #xA0 #xFF #x-80)
	    ("8859-2.TXT" latin-iso8859-2 #xA0 #xFF #x-80)
	    ("8859-3.TXT" latin-iso8859-3 #xA0 #xFF #x-80)
	    ("8859-4.TXT" latin-iso8859-4 #xA0 #xFF #x-80)
	    ("8859-5.TXT" cyrillic-iso8859-5 #xA0 #xFF #x-80)
	    ("8859-7.TXT" greek-iso8859-7 #xA0 #xFF #x-80)
	    ("8859-8.TXT" hebrew-iso8859-8 #xA0 #xFF #x-80)
	    ("8859-9.TXT" latin-iso8859-9 #xA0 #xFF #x-80)
	    ;; charset for Big5 does not matter; specifying `big5' will
	    ;; automatically make the right thing happen
	    ("BIG5.TXT" chinese-big5-1 nil nil nil big5)
	    ("CNS11643.TXT" chinese-cns11643-1 #x10000 #x1FFFF #x-10000)
	    ("CNS11643.TXT" chinese-cns11643-2 #x20000 #x2FFFF #x-20000)
	    ;; "CP1250.TXT" 
	    ;; "CP1251.TXT" 
	    ;; "CP1252.TXT" 
	    ;; "CP1253.TXT" 
	    ;; "CP1254.TXT" 
	    ;; "CP1255.TXT" 
	    ;; "CP1256.TXT" 
	    ;; "CP1257.TXT" 
	    ;; "CP1258.TXT" 
	    ;; "CP874.TXT" 
	    ;; "CP932.TXT" 
	    ;; "CP936.TXT" 
	    ;; "CP949.TXT" 
	    ;; "CP950.TXT" 
	    ;; "GB12345.TXT" 
	    ("GB2312.TXT" chinese-gb2312)
	    ;; "HANGUL.TXT"
	    ;; #### shouldn't JIS X 0201's upper limit be 7f?
	    ("JIS0201.TXT" latin-jisx0201 #x21 #x80)
	    ("JIS0201.TXT" katakana-jisx0201 #xA0 #xFF #x-80)
	    ("JIS0208.TXT" japanese-jisx0208 nil nil nil ignore-first-column)
	    ("JIS0212.TXT" japanese-jisx0212)
	    ;; "JOHAB.TXT" 
	    ;; "KOI8-R.TXT" 
	    ;; "KSC5601.TXT" 
	    ;; note that KSC5601.TXT as currently distributed is NOT what
	    ;; it claims to be!  see comments in KSX1001.TXT.
	    ("KSX1001.TXT" korean-ksc5601)
	    ;; "OLD5601.TXT" 
	    ;; "SHIFTJIS.TXT"
	    )
	   ("unicode/mule-ucs"
	    ;; #### we don't support surrogates?!??
	    ;; use these instead of the above ones once we support surrogates
	    ;;("chinese-cns11643-1.txt" chinese-cns11643-1)
	    ;;("chinese-cns11643-2.txt" chinese-cns11643-2)
	    ;;("chinese-cns11643-3.txt" chinese-cns11643-3)
	    ;;("chinese-cns11643-4.txt" chinese-cns11643-4)
	    ;;("chinese-cns11643-5.txt" chinese-cns11643-5)
	    ;;("chinese-cns11643-6.txt" chinese-cns11643-6)
	    ;;("chinese-cns11643-7.txt" chinese-cns11643-7)
	    ("chinese-sisheng.txt" chinese-sisheng)
	    ("ethiopic.txt" ethiopic)
	    ("indian-is13194.txt" indian-is13194)
	    ("ipa.txt" ipa)
	    ("thai-tis620.txt" thai-tis620)
	    ("tibetan.txt" tibetan)
	    ("vietnamese-viscii-lower.txt" vietnamese-viscii-lower)
	    ("vietnamese-viscii-upper.txt" vietnamese-viscii-upper)
	    )
	   ("unicode/other"
	    ("lao.txt" lao)
	    )
	   )))
    (mapcar #'(lambda (tables)
		(let ((undir
		       (expand-file-name (car tables) data-directory)))
		  (mapcar #'(lambda (args)
			      (apply 'load-unicode-mapping-table
				     (expand-file-name (car args) undir)
				     (cdr args)))
			  (cdr tables))))
	    parse-args)
    ;; The default-unicode-precedence-list. We set this here to default to
    ;; *not* mapping various European characters to East Asian characters;
    ;; otherwise the default-unicode-precedence-list is numerically ordered
    ;; by charset ID.
    (declare-fboundp
     (set-default-unicode-precedence-list
      '(ascii control-1 latin-iso8859-1 latin-iso8859-2 latin-iso8859-15
	greek-iso8859-7 hebrew-iso8859-8 ipa cyrillic-iso8859-5
	latin-iso8859-16 latin-iso8859-3 latin-iso8859-4 latin-iso8859-9
	vietnamese-viscii-lower vietnamese-viscii-upper 
	jit-ucs-charset-0 japanese-jisx0208 japanese-jisx0208-1978
	japanese-jisx0212 japanese-jisx0213-1 japanese-jisx0213-2
	chinese-gb2312 chinese-sisheng chinese-big5-1 chinese-big5-2
	indian-is13194 korean-ksc5601 chinese-cns11643-1 chinese-cns11643-2
	chinese-isoir165 
	composite ethiopic indian-1-column indian-2-column jit-ucs-charset-0
	katakana-jisx0201 lao thai-tis620 thai-xtis tibetan tibetan-1-column
	latin-jisx0201 chinese-cns11643-3 chinese-cns11643-4
	chinese-cns11643-5 chinese-cns11643-6 chinese-cns11643-7)))))

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

(make-coding-system
 'utf-8 'unicode
 "UTF-8"
 '(mnemonic "UTF8"
   documentation "
UTF-8 Unicode encoding -- ASCII-compatible 8-bit variable-width encoding
sharing the following principles with the Mule-internal encoding:

  -- All ASCII characters (codepoints 0 through 127) are represented
     by themselves (i.e. using one byte, with the same value as the
     ASCII codepoint), and these bytes are disjoint from bytes
     representing non-ASCII characters.

     This means that any 8-bit clean application can safely process
     UTF-8-encoded text as it were ASCII, with no corruption (e.g. a
     '/' byte is always a slash character, never the second byte of
     some other character, as with Big5, so a pathname encoded in
     UTF-8 can safely be split up into components and reassembled
     again using standard ASCII processes).

  -- Leading bytes and non-leading bytes in the encoding of a
     character are disjoint, so moving backwards is easy.

  -- Given only the leading byte, you know how many following bytes
     are present.
"
   unicode-type utf-8))

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

(defconst ccl-encode-to-ucs-2
  (eval-when-compile
    (let ((pre-existing 
           ;; This is the compiled CCL program from the assert
           ;; below. Since this file is dumped and ccl.el isn't (and
           ;; even when it was, it was dumped much later than this
           ;; one), we can't compile the program at dump time. We can
           ;; check at byte compile time that the program is as
           ;; expected, though.
           [1 16 131127 7 98872 65823 1307 5 -65536 65313 64833 1028
              147513 8 82009 255 22]))
      (when (featurep 'mule)
        ;; Check that the pre-existing constant reflects the intended
        ;; CCL program.
        (assert
         (equal pre-existing
                (ccl-compile
                 `(1 
                   ( ;; mule-to-unicode's first argument is the
                    ;; charset ID, the second its first byte
                    ;; left shifted by 7 bits masked with its
                    ;; second byte.
                    (r1 = (r1 << 7)) 
                    (r1 = (r1 | r2)) 
                    (mule-to-unicode r0 r1) 
                    (if (r0 & ,(lognot #xFFFF))
                        ;; Redisplay looks in r1 and r2 for the first
                        ;; and second bytes of the X11 font,
                        ;; respectively. For non-BMP characters we
                        ;; display U+FFFD.
                        ((r1 = #xFF)
                         (r2 = #xFD))
                      ((r1 = (r0 >> 8)) 
                       (r2 = (r0 & #xFF))))))))
         nil 
         "The pre-compiled CCL program appears broken. "))
      pre-existing))
  "CCL program to transform Mule characters to UCS-2.")

(when (featurep 'mule)
  (put 'ccl-encode-to-ucs-2 'ccl-program-idx
       (declare-fboundp
	(register-ccl-program 'ccl-encode-to-ucs-2 ccl-encode-to-ucs-2))))

;; Now, create jit-ucs-charset-0 entries for those characters in Windows
;; Glyph List 4 that would otherwise end up in East Asian character sets.
;; 
;; WGL4 is a character repertoire from Microsoft that gives a guideline
;; for font implementors as to what characters are sufficient for
;; pan-European support.  The intention of this code is to avoid the
;; situation where these characters end up mapping to East Asian XEmacs
;; characters, which generally clash strongly with European characters
;; both in font choice and character width; jit-ucs-charset-0 is a
;; single-width character set which comes before the East Asian character
;; sets in the default-unicode-precedence-list above.
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
       (#x2122 ?\xa9) ;; U+2122 TRADE MARK SIGN, ?,A)(B

       (#x0132 ?\xe6) ;; U+0132 LATIN CAPITAL LIGATURE IJ, ?,Af(B
       (#x013f ?\xe6) ;; U+013F LATIN CAPITAL LETTER L WITH MIDDLE DOT, ?,Af(B

       (#x0133 ?\xe6) ;; U+0133 LATIN SMALL LIGATURE IJ, ?,Af(B
       (#x0140 ?\xe6) ;; U+0140 LATIN SMALL LETTER L WITH MIDDLE DOT, ?,Af(B
       (#x0149 ?\xe6) ;; U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPH,?,Af(B

       (#x2194 ?|) ;; U+2194 LEFT RIGHT ARROW
       (#x2660 ?*) ;; U+2660 BLACK SPADE SUIT
       (#x2665 ?*) ;; U+2665 BLACK HEART SUIT
       (#x2663 ?*) ;; U+2663 BLACK CLUB SUIT
       (#x2592 ?|) ;; U+2592 MEDIUM SHADE
       (#x2195 ?|) ;; U+2195 UP DOWN ARROW

       (#x2113 ?\xb9) ;; U+2113 SCRIPT SMALL L, ?,A9(B
       (#x215b ?\xbe) ;; U+215B VULGAR FRACTION ONE EIGHTH, ?,A>(B
       (#x215c ?\xbe) ;; U+215C VULGAR FRACTION THREE EIGHTHS, ?,A>(B
       (#x215d ?\xbe) ;; U+215D VULGAR FRACTION FIVE EIGHTHS, ?,A>(B
       (#x215e ?\xbe) ;; U+215E VULGAR FRACTION SEVEN EIGHTHS, ?,A>(B
       (#x207f ?\xbe) ;; U+207F SUPERSCRIPT LATIN SMALL LETTER N, ?,A>(B
  
       ;; These are not in WGL 4, but are IPA characters that should not
       ;; be double width. They are the only IPA characters that both
       ;; occur in packages/mule-packages/leim/ipa.el and end up in East
       ;; Asian character sets when that file is loaded in an XEmacs
       ;; without packages.
       (#x2197 ?|) ;; U+2197 NORTH EAST ARROW
       (#x2199 ?|) ;; U+2199 SOUTH WEST ARROW
       (#x2191 ?|) ;; U+2191 UPWARDS ARROW
       (#x207f ?\xb9)) ;; U+207F SUPERSCRIPT LATIN SMALL LETTER N, ?,A9(B
  with decoded = nil
  with syntax-table = (standard-syntax-table)
  initially (unless (featurep 'mule) (return))
  ;; This creates jit-ucs-charset-0 entries because:
  ;;
  ;;    1. If the tables are dumped, it is run at dump time before they are
  ;;    dumped, and as such before the relevant conversions are available
  ;;    (they are made available in mule/general-late.el). 
  ;;
  ;;    2. If the tables are not dumped, it is run at dump time, long before
  ;;    any of the other mappings are available.
  ;;
  do
  (setq decoded (decode-char 'ucs ucs))
  (assert (eq (declare-fboundp (char-charset decoded))
              'jit-ucs-charset-0) nil 
              "Unexpected Unicode decoding behavior.  ")
  (modify-syntax-entry decoded
                       (string 
                        (char-syntax ascii-or-latin-1))
                       syntax-table))

;; *Sigh*, declarations needs to be at the start of the line to be picked up
;; by make-docfile. Not so much an issue with ccl-encode-to-ucs-2, which we
;; don't necessarily want to advertise, but the following are important.

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

(defvar unicode-query-coding-skip-chars-arg nil ;; Set in general-late.el
  "Used by `unicode-query-coding-region' to skip chars with known mappings.")

(defun unicode-query-coding-region (begin end coding-system
				    &optional buffer ignore-invalid-sequencesp
                                    errorp highlightp)
  "The `query-coding-region' implementation for Unicode coding systems.

Supports IGNORE-INVALID-SEQUENCESP, that is, XEmacs characters that reflect
invalid octets on disk will be treated as encodable if this argument is
specified, and as not encodable if it is not specified."

  ;; Potential problem here; the octets that correspond to octets from #x00
  ;; to #x7f on disk will be treated by utf-8 and utf-7 as invalid
  ;; sequences, and thus, in theory, encodable.

  (check-argument-type #'coding-system-p
                       (setq coding-system (find-coding-system coding-system)))
  (check-argument-type #'integer-or-marker-p begin)
  (check-argument-type #'integer-or-marker-p end)
  (let* ((skip-chars-arg (concat unicode-query-coding-skip-chars-arg
				 (if ignore-invalid-sequencesp
				     unicode-invalid-sequence-regexp-range
				   "")))
         (ranges (make-range-table))
         (looking-at-arg (concat "[" skip-chars-arg "]"))
         (case-fold-search nil)
	 (invalid-sequence-lower-unicode-bound
	  (char-to-unicode
	   (aref (decode-coding-string "\xd8\x00\x00\x00"
				       'utf-16-be) 3)))
	  (invalid-sequence-upper-unicode-bound
	   (char-to-unicode
	    (aref (decode-coding-string "\xd8\x00\x00\xFF"
					'utf-16-be) 3)))
         fail-range-start fail-range-end char-after failed
	 extent char-unicode failed-reason previous-failed-reason)
    (save-excursion
      (when highlightp
        (query-coding-clear-highlights begin end buffer))
      (goto-char begin buffer)
      (skip-chars-forward skip-chars-arg end buffer)
      (while (< (point buffer) end)
        (setq char-after (char-after (point buffer) buffer)
              fail-range-start (point buffer))
        (while (and
                (< (point buffer) end)
                (not (looking-at looking-at-arg))
                (or (and
                     (= -1 (setq char-unicode (char-to-unicode char-after)))
                     (setq failed-reason 'unencodable))
                    (and (not ignore-invalid-sequencesp)
                         ;; The default case, with ignore-invalid-sequencesp
                         ;; not specified:
                         ;; If the character is in the Unicode range that
                         ;; corresponds to an invalid octet, we want to
                         ;; treat it as unencodable.
                         (<= invalid-sequence-lower-unicode-bound
                             char-unicode)
                         (<= char-unicode
			     invalid-sequence-upper-unicode-bound)
                         (setq failed-reason 'invalid-sequence)))
                (or (null previous-failed-reason)
                    (eq previous-failed-reason failed-reason)))
          (forward-char 1 buffer)
          (setq char-after (char-after (point buffer) buffer)
                failed t
                previous-failed-reason failed-reason))
        (if (= fail-range-start (point buffer))
            ;; The character can actually be encoded by the coding
            ;; system; check the characters past it.
	    (forward-char 1 buffer)
          ;; Can't be encoded; note this.
          (when errorp 
            (error 'text-conversion-error
                   (format "Cannot encode %s using coding system"
                           (buffer-substring fail-range-start (point buffer)
                                             buffer))
                   (coding-system-name coding-system)))
          (assert
           (not (null previous-failed-reason)) t
           "If we've got here, previous-failed-reason should be non-nil.")
          (put-range-table fail-range-start
                           ;; If char-after is non-nil, we're not at
                           ;; the end of the buffer.
                           (setq fail-range-end (if char-after
                                                    (point buffer)
                                                  (point-max buffer)))
                           previous-failed-reason ranges)
          (setq previous-failed-reason nil)
          (when highlightp
            (setq extent (make-extent fail-range-start fail-range-end buffer))
            (set-extent-priority extent (+ mouse-highlight-priority 2))
            (set-extent-face extent 'query-coding-warning-face)))
        (skip-chars-forward skip-chars-arg end buffer))
      (if failed
          (values nil ranges)
        (values t nil)))))

(loop
  for coding-system in (coding-system-list)
  initially (unless (featurep 'mule) (return))
  do (when (eq 'unicode (coding-system-type coding-system))
       (coding-system-put coding-system 'query-coding-function
			  #'unicode-query-coding-region)))

(unless (featurep 'mule)
  ;; We do this in such a roundabout way--instead of having the above defun
  ;; and defvar calls inside a (when (featurep 'mule) ...) form--to have
  ;; make-docfile.c pick up symbol and function documentation correctly. An
  ;; alternative approach would be to fix make-docfile.c to be able to read
  ;; Lisp.
  (mapcar #'unintern
          '(ccl-encode-to-ucs-2 unicode-error-default-translation-table
            unicode-invalid-regexp-range frob-unicode-errors-region
            unicode-error-translate-region unicode-query-coding-region
            unicode-query-coding-skip-chars-arg)))

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
