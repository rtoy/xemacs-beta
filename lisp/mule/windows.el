;;; windows.el --- Support Windows code pages

;; Copyright (C) 2005, 2010 Ben Wing.

;; Keywords: multilingual, Windows

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

;; There's a file in GNU Emacs called international/codepage.el, but it
;; has nothing to do with this file.  It uses CCL for conversion, which
;; we don't need.  It's too annoying to put all the charsets for the
;; various code pages in individual language-specific files, and at some
;; point soon most of the info in those language files won't be necessary
;; because it will be derived from Unicode tables.

(defun windows-ansioem-to-family (ansioem)
  ;; Given a string specifying a pretty version of the family (Ansi, OEM,
  ;; etc.), return the corresponding charset tag(s) as a list.
  (cond ((equal ansioem "ANSI") '(windows-ansi))
	((equal ansioem "Ansi/OEM") '(windows-ansi windows-oem))
	((equal ansioem "OEM") '(windows-oem))
	((equal ansioem "EBCDIC") '(ebcdic))
	((equal ansioem "Mac") '(macintosh))
	(t (signal-error 'invalid-argument
			 `("Bad `ansioem' value" ,ansioem)))))

;; Make a Windows charset corresponding to a specified code page.  CODEPAGE
;; is the number of the code page.  SCRIPT is a symbol indicating the
;; writing system of the code page, e.g. `latin' or `cyrillic'.  NAME is a
;; string describing the code page, e.g. \"Eastern Europe\" (code page
;; 1250).  DIMENSION, CHARS, OFFSET, and UNICODE-MAP go directly to the
;; `make-charset' properties of the same names.  TAGS has a family tag
;; derived from ANSIOEM added to it as well as the SCRIPT tag added to it,
;; and then goes to the `tags' property of `make-charset'.
(defun* make-windows-charset (codepage script name
			      dimension chars offset
			      &key tags ansioem unicode-map)
  (or (listp tags) (setq tags (list tags)))
  (make-charset
   (intern (format "%s-windows-%s" script codepage))
   (format "Windows code page %s (%s)" codepage name)
   `(dimension ,dimension
     chars ,chars
     offset ,offset
     ,@(and unicode-map `(unicode-map ,unicode-map))
     tags ,(append (and ansioem (windows-ansioem-to-family ansioem))
		   (list script) tags)
     short-name ,(format "Windows %s (%s)" codepage name)
     long-name ,(format "Windows code page %s (%s)" codepage name)
     )))

;; Make a one-dimension Windows charset corresponding to a specified code
;; page.  CODEPAGE is the number of the code page.  SCRIPT is a symbol
;; indicating the writing system of the code page, e.g. `latin' or
;; `cyrillic'.  NAME is a string describing the code page, e.g. \"Eastern
;; Europe\" (code page 1250).
(defun* make-one-dimension-windows-charset (codepage script name l1 h1
					    &key tags ansioem unicode-map)
  (make-windows-charset codepage script name 1 (1+ (- h1 l1)) l1
			:tags tags
			:ansioem ansioem
			:unicode-map unicode-map))

;; Make a two-dimension Windows charset corresponding to a specified code
;; page.  CODEPAGE is the number of the code page.  SCRIPT is a symbol
;; indicating the writing system of the code page, e.g. `chinese'.  NAME is
;; a string describing the code page, e.g. \"Simplified Chinese\" (code
;; page 936).  The charset has characters in the range (L1, L2) - (H1, H2),
;; inclusive.
(defun* make-two-dimension-windows-charset (codepage script name l1 l2 h1 h2
					    &key tags ansioem unicode-map)
  (make-windows-charset codepage script name
			2 `(,(1+ (- h1 l1)) ,(1+ (- h2 l2))) `(,l1 ,l2)
			:tags tags
			:ansioem ansioem
			:unicode-map unicode-map))

(defun* make-windows-coding-system (codepage script name
				    &key category ansioem mnemonic extra-doc)
  (make-coding-system
   (intern (format "windows-%s" codepage))
   'multibyte
   (format "Microsoft's CP%s (%s, %s)" codepage ansioem name)
   `(charsets (ascii ,(intern (format "%s-windows-%s" script codepage)))
     documentation
     ,(format "CP %s, Microsoft's %s encoding (%s).
This implements the encoding specified by code page %s.
%s" codepage name (or ansioem "OEM") codepage (or extra-doc ""))
     mnemonic ,(or mnemonic (format "CP%s" codepage))
     aliases (,(intern (format "cp%s" codepage)))))
  (coding-system-put (intern (format "windows-%s" codepage))
		     'category (or category 'no-conversion)))

;; If `unicode-dir' is given, use CP###.TXT (### is the codepage) in
;; directory `unicode-dir' under unicode/unicode-consortium/VENDORS/MICSFT/.
;; If `unicode-file' is given, just use it directly.

(defun windows-generate-unicode-map (codepage unicode-file unicode-dir
				     unicode-offset)
  (let ((unicode-file
	 (or unicode-file (and unicode-dir (format
		    "unicode/unicode-consortium/VENDORS/MICSFT/%s/CP%d.TXT"
					    unicode-dir codepage)))))
    (and unicode-file
	 `(,unicode-file ,unicode-offset))))
  
(defun* make-1d-windows-charset-and-coding-system
  (codepage script name &key tags unicode-file unicode-dir
	    low high category ansioem mnemonic extra-doc)
  (or low (setq low 128))
  (or high (setq high 255))
  (let ((unicode-map
	 (windows-generate-unicode-map codepage unicode-file unicode-dir
				       low)))
    (make-one-dimension-windows-charset codepage script name low high
					:ansioem ansioem
					:tags tags
					:unicode-map unicode-map))
  (make-windows-coding-system codepage script name :category category
			      :ansioem ansioem :mnemonic mnemonic
			      :extra-doc extra-doc))

(defun* make-2d-windows-charset-and-coding-system
  (codepage script name &key tags unicode-file unicode-dir unicode-offset
	    low high category ansioem mnemonic extra-doc)
  (or low (setq low '(0 0)))
  (or high (setq high '(255 255)))
  (let ((unicode-map
	 (windows-generate-unicode-map codepage unicode-file unicode-dir
				       (or unicode-offset #x8000))))
    (make-two-dimension-windows-charset codepage script name
					(first low) (second low)
					(first high) (second high)
					:ansioem ansioem
					:tags tags
					:unicode-map unicode-map))
  (make-windows-coding-system codepage script name :category category
			      :ansioem ansioem :mnemonic mnemonic
			      :extra-doc extra-doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Now make all the Windows code page charsets/coding systems        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The ANSI and OEM code pages

(make-1d-windows-charset-and-coding-system
 437 'latin "MS-DOS United States" :unicode-dir "PC"
 :ansioem "OEM")
;; This is ISO-8859-6. 
;;(make-1d-windows-charset-and-coding-system
;; 708 'arabic "Arabic (ASMO 708)"
;; :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 709 'arabic "Arabic (ASMO 449+, BCON V4)"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 710 'arabic "Arabic (Transparent Arabic)"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 720 'arabic "Arabic (Transparent ASMO)"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 737 'greek "Greek (formerly 437G)" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 775 'latin "Baltic" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 850 'latin "MS-DOS Multilingual (Latin I)" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 852 'latin "MS-DOS Slavic (Latin II)" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 855 'cyrillic "IBM Cyrillic (primarily Russian)" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 857 'latin "IBM Turkish" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 860 'latin "MS-DOS Portuguese" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 861 'latin "MS-DOS Icelandic" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 862 'hebrew "Hebrew" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 863 'latin "MS-DOS Canadian-French" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 864 'arabic "Arabic" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 865 'latin "MS-DOS Nordic" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 866 'cyrillic "MS-DOS Russian" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 869 'greek "IBM Modern Greek" :unicode-dir "PC"
 :ansioem "OEM")
(make-1d-windows-charset-and-coding-system
 874 'thai "Thai" :unicode-dir "WINDOWS"
 :ansioem "Ansi/OEM")
(make-2d-windows-charset-and-coding-system
 932 'japanese "Japanese" :unicode-dir "WINDOWS"
 :low '(#x81 #x40) :high '(#xfe #xfe)
 :category 'shift-jis :ansioem "Ansi/OEM")
(make-2d-windows-charset-and-coding-system
 936 'chinese "Simplified Chinese (PRC, Singapore)" :unicode-dir "WINDOWS"
 :low '(#x81 #x40) :high '(#xfe #xfe)
 :category 'iso-8-2 :ansioem "Ansi/OEM")
(make-2d-windows-charset-and-coding-system
 949 'korean "Korean" :unicode-dir "WINDOWS"
 :low '(#x81 #x41) :high '(#xfe #xfe)
 :category 'iso-8-2 :ansioem "Ansi/OEM")
(make-2d-windows-charset-and-coding-system
 950 'chinese "Traditional Chinese (Taiwan; Hong Kong SAR, PRC)" :unicode-dir "WINDOWS"
 :low '(#xa1 #x40) :high '(#xfe #xfe)
 :category 'big5 :ansioem "Ansi/OEM")
;; This code page doesn't work. See 
;; http://blogs.msdn.com/michkap/archive/2005/08/01/446475.aspx
;;(make-2d-windows-charset-and-coding-system
;; 1200 'unicode "Unicode (BMP of ISO 10646)"
;; :category 'utf-16-little-endian :ansioem "ANSI")
(make-1d-windows-charset-and-coding-system
 1250 'latin "Eastern Europe" :unicode-dir "WINDOWS"
 :ansioem "ANSI"
 :extra-doc 
 "See also `iso-8859-2' and `windows-1252' for Western Europe.")
(make-1d-windows-charset-and-coding-system
 1251 'cyrillic "Cyrillic" :unicode-dir "WINDOWS"
 :ansioem "ANSI"
 :mnemonic "CyrW"
 :extra-doc
   "This ASCII-compatible encoding is unfortunately not compatible at
the code point level with the KOI8 family of encodings, but it
provides several widely-used punctuation and quotation marks that
KOI-8R and its relatives don't, and has become widely used. 

It supports Russian, Bulgarian, Serbian and other languages written using
Cyrillic script.  ")
(make-1d-windows-charset-and-coding-system
 1252 'latin "ANSI" :unicode-dir "WINDOWS"
 :ansioem "ANSI"
 :extra-doc
 "This is Microsoft's extension of iso-8859-1 for Western Europe
and the Americas.")
(make-1d-windows-charset-and-coding-system
 1253 'greek "Greek" :unicode-dir "WINDOWS"
 :ansioem "ANSI"
 :mnemonic "GrkW"
 :extra-doc
"This encoding is used for monotonic Greek.
This ASCII-compatible encoding is slightly incompatibile with
ISO-8859-7; it provides several widely-used punctuation marks in the C1
ISO-2022 area, which makes it incompatible with the latter standard, but
that latter standard is not used in Greece.")
(make-1d-windows-charset-and-coding-system
 1254 'latin "Turkish" :unicode-dir "WINDOWS"
 :ansioem "ANSI")
(make-1d-windows-charset-and-coding-system
 1255 'hebrew "Hebrew" :unicode-dir "WINDOWS"
 :ansioem "ANSI")
(make-1d-windows-charset-and-coding-system
 1256 'arabic "Arabic" :unicode-dir "WINDOWS"
 :ansioem "ANSI"
 :extra-doc
   "This is the much superior to the ISO standard one.")
(make-1d-windows-charset-and-coding-system
 1257 'latin "Baltic Rim" :unicode-dir "WINDOWS"
 :ansioem "ANSI")
(make-1d-windows-charset-and-coding-system
 1258 'latin "VietNam" :unicode-dir "WINDOWS"
 :ansioem "ANSI")
;; #### Is this category right? I don't have Lunde to hand, and the
;; online information on Johab is scant.
(make-2d-windows-charset-and-coding-system
 1361 'korean "Korean (Johab)"
 :low '(#x84 #x31) :high '(#xf9 #xfe)
 :unicode-file "unicode/libiconv/JOHAB.TXT"
 :category 'iso-8-2 :ansioem "Ansi/OEM")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The MAC code pages



;;We provide this in latin.el
;;(make-1d-windows-charset-and-coding-system
;; 10000 'latin "Macintosh Roman"
;; :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/ROMAN.TXT"
;; :ansioem "Mac")
(make-2d-windows-charset-and-coding-system
 10001 'japanese "Macintosh Japanese"
 :low '(#x81 #x40) :high '(#xfe #xfe)
 ;; #### Not clear about this, take a random guess that it's the same as
 ;; Shift-JIS, i.e. code page 932.
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/WINDOWS/CP932.TXT" 
 :category 'shift-jis :ansioem "Mac")
(make-1d-windows-charset-and-coding-system
 10006 'greek "Macintosh Greek I"
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/GREEK.TXT" 
 :ansioem "Mac")
(make-1d-windows-charset-and-coding-system
 10007 'cyrillic "Macintosh Cyrillic"
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/CYRILLIC.TXT"
 :ansioem "Mac")
(make-1d-windows-charset-and-coding-system
 10029 'latin "Macintosh Latin 2"
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/LATIN2.TXT"
 :ansioem "Mac")
(make-1d-windows-charset-and-coding-system
 10079 'latin "Macintosh Icelandic"
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/ICELAND.TXT"
 :ansioem "Mac")
(make-1d-windows-charset-and-coding-system
 10081 'latin "Macintosh Turkish"
 :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/MAC/TURKISH.TXT"
 :ansioem "Mac")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The EBCDIC code pages

;; #### Fuck this shit!  I don't feel like dealing with this now.  If we
;; do want to deal with it, we have to fix `make-windows-coding-system'
;; so it doesn't include `ascii' as one of the charsets.
;; 
;; (make-1d-windows-charset-and-coding-system
;;  ;; need to specify the filename otherwise we try to look for CP37.TXT
;;  037 'latin "EBCDIC"
;;  :unicode-file "unicode/unicode-consortium/VENDORS/MICSFT/EBCDIC/CP037.TXT"
;;  :low 0 :high 255
;;  :ansioem "EBCDIC")
;; (make-1d-windows-charset-and-coding-system
;;  500 'latin "EBCDIC \"500V1\"" :unicode-dir "EBCDIC" :low 0 :high 255
;;  :ansioem "EBCDIC")
;; (make-1d-windows-charset-and-coding-system
;;  875 'greek "EBCDIC" :unicode-dir "EBCDIC" :low 0 :high 255
;;  :ansioem "EBCDIC")
;; (make-1d-windows-charset-and-coding-system
;;  1026 'latin "EBCDIC" :unicode-dir "EBCDIC" :low 0 :high 255
;;  :ansioem "EBCDIC")
