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
	    ("8859-6.TXT" arabic-iso8859-6 #xA0 #xFF #x-80)
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
	    parse-args)))

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
   type utf-16))

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
   type utf-16
   need-bom t))

(make-coding-system
 'utf-16-little-endian 'unicode
 "UTF-16 Little Endian"
 '(mnemonic "UTF16-LE"
   documentation
   "Little-endian version of UTF-16 Unicode encoding.
See `utf-16' coding system."
   type utf-16
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
   type utf-16
   little-endian t
   need-bom t))

(make-coding-system
 'ucs-4 'unicode
 "UCS-4"
 '(mnemonic "UCS4"
   documentation
   "UCS-4 Unicode encoding -- fully fixed-width four-byte encoding."
   type ucs-4))

(make-coding-system
 'ucs-4-little-endian 'unicode
 "UCS-4 Little Endian"
 '(mnemonic "UCS4-LE"
   documentation
   ;; #### I don't think this is permitted by ISO 10646, only Unicode.
   ;; Call it UTF-32 instead?
   "Little-endian version of UCS-4 Unicode encoding.  See `ucs-4' coding system."
   type ucs-4
   little-endian t))

(make-coding-system
 'utf-8 'unicode
 "UTF-8"
 '(mnemonic "UTF8"
   documentation
   "UTF-8 Unicode encoding -- ASCII-compatible 8-bit variable-width encoding
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
   type utf-8))

(make-coding-system
 'utf-8-bom 'unicode
 "UTF-8 w/BOM"
 '(mnemonic "MSW-UTF8"
   documentation
   "UTF-8 Unicode encoding, with byte order mark.
Standard encoding for representing UTF-8 under MS Windows."
   type utf-8
   little-endian t
   need-bom t))

(defun decode-char (quote-ucs code &optional restriction) 
  "FSF compatibility--return Mule character with Unicode codepoint CODE.
The second argument must be 'ucs, the third argument is ignored.  "
  (assert (eq quote-ucs 'ucs) t
	  "Sorry, decode-char doesn't yet support anything but the UCS.  ")
  (unicode-to-char code))

(defun encode-char (char quote-ucs &optional restriction)
  "FSF compatibility--return the Unicode code point of CHAR.
The second argument must be 'ucs, the third argument is ignored.  "
  (assert (eq quote-ucs 'ucs) t
	  "Sorry, encode-char doesn't yet support anything but the UCS.  ")
  (char-to-unicode char))

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
;    type utf-7))
