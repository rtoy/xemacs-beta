;;; japanese.el --- Japanese support -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2000, 2002 Ben Wing.

;; Keywords: multilingual, Japanese

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

;;; Synched up with: Emacs 20.6 (international/japanese.el).

;;; Commentary:

;; For Japanese, character sets JISX0201, JISX0208, JISX0212 are
;; supported.

;;; Code:

; (make-charset 'katakana-jisx0201 
; 	      "Katakana Part of JISX0201.1976"
; 	      '(dimension
; 		1
; 		registry "JISX0201"
; 		chars 94
; 		columns 1
; 		direction l2r
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
; 		columns 1
; 		direction l2r
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
; 		columns 2
; 		direction l2r
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
; 		columns 2
; 		direction l2r
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
; 		columns 2
; 		direction l2r
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
		columns 2
		direction l2r
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
		columns 2
		direction l2r
		final ?P
		graphic 0
		short-name "JISX0213-2"
		long-name "JISX0213-2"
		))

;;; Syntax of Japanese characters.
(modify-syntax-entry 'katakana-jisx0201 "w")
(modify-syntax-entry 'japanese-jisx0212 "w")

(modify-syntax-entry 'japanese-jisx0208 "w")
(loop for row in '(33 34 40)
      do (modify-syntax-entry `[japanese-jisx0208 ,row] "_"))
(loop for char in '(?ー ?゛ ?゜ ?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-syntax-entry char "w"))
(modify-syntax-entry ?\（ "(）")
(modify-syntax-entry ?\［ "(］")
(modify-syntax-entry ?\｛ "(｝")
(modify-syntax-entry ?\「 "(」")
(modify-syntax-entry ?\『 "(』")
(modify-syntax-entry ?\） ")（")
(modify-syntax-entry ?\］ ")［")
(modify-syntax-entry ?\｝ ")｛")
(modify-syntax-entry ?\」 ")「")
(modify-syntax-entry ?\』 ")『")

;;; Character categories S, A, H, K, G, Y, and C
(define-category ?S "Japanese 2-byte symbol character.")
(modify-category-entry [japanese-jisx0208 33] ?S)
(modify-category-entry [japanese-jisx0208 34] ?S)
(modify-category-entry [japanese-jisx0208 40] ?S)
(define-category ?A "Japanese 2-byte Alphanumeric character.")
(modify-category-entry [japanese-jisx0208 35] ?A)
(define-category ?H "Japanese 2-byte Hiragana character.")
(modify-category-entry [japanese-jisx0208 36] ?H)
(define-category ?K "Japanese 2-byte Katakana character.")
(modify-category-entry [japanese-jisx0208 37] ?K)
(define-category ?G "Japanese 2-byte Greek character.")
(modify-category-entry [japanese-jisx0208 38] ?G)
(define-category ?Y "Japanese 2-byte Cyrillic character.")
(modify-category-entry [japanese-jisx0208 39] ?Y)
(define-category ?C "Japanese 2-byte Kanji characters.")
(loop for row from 48 to 126
      do (modify-category-entry `[japanese-jisx0208 ,row] ?C))
(loop for char in '(?ー ?゛ ?゜)
      do (modify-category-entry char ?K)
         (modify-category-entry char ?H))
(loop for char in '(?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-category-entry char ?C))
(modify-category-entry 'japanese-jisx0212 ?C)

(defvar japanese-word-regexp
  "\\cA+\\cH*\\|\\cK+\\cH*\\|\\cC+\\cH*\\|\\cH+\\|\\ck+\\|\\sw+"
  "Regular expression used to match a Japanese word.")

(set-word-regexp japanese-word-regexp)
(setq forward-word-regexp  "\\w\\>")
(setq backward-word-regexp "\\<\\w")

;;; Paragraph setting
(setq sentence-end
      (concat
       "\\("
       "\\("
       "[.?!][]\"')}]*"
       "\\|"
       "[．？！][］”’）｝〕〉》」』]*"
       "\\)"
       "\\($\\|\t\\|  \\)"
       "\\|"
       "。"
       "\\)"
       "[ \t\n]*"))
(setq paragraph-start    "^[ 　\t\n\f]")
(setq paragraph-separate "^[ 　\t\f]*$")

;; EGG specific setup
(define-egg-environment 'japanese
  "Japanese settings for egg."
  (lambda ()
    (with-boundp '(its:*standard-modes* its:*current-map* wnn-server-type)
      (with-fboundp 'its:get-mode-map
	(when (not (featurep 'egg-jpn))
	  (load "its-hira")
	  (load "its-kata")
	  (load "its-hankaku")
	  (load "its-zenkaku")
	  (setq its:*standard-modes*
		(append
		 (list (its:get-mode-map "roma-kana")
		       (its:get-mode-map "roma-kata")
		       (its:get-mode-map "downcase")
		       (its:get-mode-map "upcase")
		       (its:get-mode-map "zenkaku-downcase")
		       (its:get-mode-map "zenkaku-upcase"))
		 its:*standard-modes*))
	  (provide 'egg-jpn))
	(setq wnn-server-type 'jserver)
	;; Can't do this here any more.  Must do it when selecting egg-wnn
	;; or egg-sj3
	;; (setq egg-default-startup-file "eggrc-wnn")
	(setq-default its:*current-map* (its:get-mode-map "roma-kana"))))))

;; stuff for providing grammatic processing of Japanese text
;; something like this should probably be created for all environments...
;; #### Arrgh.  This stuff should defvar'd in either fill.el or kinsoku.el.
;; Then the language environment should set these things, probably buffer-
;; locally.

;; #### will be moved to fill.el
(defvar space-insertable
  (let* ((aletter (concat "\\(" ascii-char "\\|" kanji-char "\\)"))
	 (kanji-space-insertable
	  (concat 
	   "、" aletter                   "\\|"
	   "。" aletter                   "\\|"
	   aletter "（"                   "\\|"
	   "）" aletter                   "\\|"
	   ascii-alphanumeric  kanji-kanji-char "\\|"
	   kanji-kanji-char    ascii-alphanumeric)))
    (concat " " aletter "\\|" kanji-space-insertable))
  "Regexp for finding points that can have spaces inserted into them for justification")

;; Beginning of FSF synching with international/japanese.el.

;; (make-coding-system
;;  'iso-2022-jp 2 ?J
;;  "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201) nil nil nil
;;    short ascii-eol ascii-cntl seven)
;;  '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
;;                   latin-jisx0201 japanese-jisx0212 katakana-jisx0201)
;;    (mime-charset . iso-2022-jp)))

(make-coding-system
 'iso-2022-jp 'iso2022
 "ISO-2022-JP (Japanese mail)"
 '(charset-g0 ascii
   short t
   seven t
   input-charset-conversion ((latin-jisx0201 ascii)
			     (japanese-jisx0208-1978 japanese-jisx0208))
   mnemonic "MULE/7bit"
   documentation
   "Coding system used for communication with mail and news in Japan."
   ))

(make-coding-system
 'jis7 'iso2022
 "JIS7 (old Japanese 7-bit encoding)"
 '(charset-g0 ascii
   charset-g1 katakana-jisx0201
   short t
   seven t
   lock-shift t
   input-charset-conversion ((latin-jisx0201 ascii)
			     (japanese-jisx0208-1978 japanese-jisx0208))
   mnemonic "JIS7"
   documentation
   "Old JIS 7-bit encoding; mostly superseded by ISO-2022-JP.
Uses locking-shift (SI/SO) to select half-width katakana."
   ))

(make-coding-system
 'jis8 'iso2022
 "JIS8 (old Japanese 8-bit encoding)"
 '(charset-g0 ascii
   charset-g1 katakana-jisx0201
   short t
   input-charset-conversion ((latin-jisx0201 ascii)
			     (japanese-jisx0208-1978 japanese-jisx0208))
   mnemonic "JIS8"
   documentation
   "Old JIS 8-bit encoding; mostly superseded by ISO-2022-JP.
Uses high bytes for half-width katakana."
   ))

(define-coding-system-alias 'junet 'iso-2022-jp)

;; (make-coding-system
;;  'iso-2022-jp-2 2 ?J
;;  "ISO 2022 based 7bit encoding for CJK, Latin-1, and Greek (MIME:ISO-2022-JP-2)"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201
;;           chinese-gb2312 korean-ksc5601) nil
;;           (nil latin-iso8859-1 greek-iso8859-7) nil
;;  short ascii-eol ascii-cntl seven nil single-shift)
;;  '((safe-charsets ascii japanese-jisx0208-1978 japanese-jisx0208
;;                   latin-jisx0201 japanese-jisx0212 katakana-jisx0201
;;                   chinese-gb2312 korean-ksc5601
;;                   latin-iso8859-1 greek-iso8859-7)
;;    (mime-charset . iso-2022-jp-2)))

;; (make-coding-system
;;  'japanese-shift-jis 1 ?S
;;  "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)"
;;  nil
;;  '((safe-charsets ascii japanese-jisx0208 japanese-jisx0208-1978
;;                   latin-jisx0201 katakana-jisx0201)
;;    (mime-charset . shift-jis)
;;    (charset-origin-alist (japanese-jisx0208 "SJIS" encode-sjis-char)
;;                          (katakana-jisx0201 "SJIS" encode-sjis-char))))

(make-coding-system
 'shift-jis 'shift-jis
 "Shift-JIS"
 '(mnemonic "Ja/SJIS"
   documentation "The standard Japanese encoding in MS Windows."
))

;; A former name?
(define-coding-system-alias 'shift_jis 'shift-jis)

;; FSF:
;; (define-coding-system-alias 'shift-jis 'japanese-shift-jis)
;; (define-coding-system-alias 'sjis 'japanese-shift-jis)

;; (make-coding-system
;;  'japanese-iso-7bit-1978-irv 2 ?j
;;  "ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman"
;;  '((ascii japanese-jisx0208-1978 japanese-jisx0208
;;           latin-jisx0201 japanese-jisx0212 katakana-jisx0201 t) nil nil nil
;;    short ascii-eol ascii-cntl seven nil nil use-roman use-oldjis)
;;  '(ascii japanese-jisx0208-1978 japanese-jisx0208 latin-jisx0201))

(make-coding-system
 'iso-2022-jp-1978-irv 'iso2022
 "ISO-2022-JP-1978-IRV (Old JIS)"
 '(charset-g0 ascii
   short t
   seven t
   output-charset-conversion ((ascii latin-jisx0201)
			      (japanese-jisx0208 japanese-jisx0208-1978))
   documentation
   "This is a coding system used for old JIS terminals.  It's an ISO
2022 based 7-bit encoding for Japanese JISX0208-1978 and JISX0201-Roman."
   mnemonic "Ja-78/7bit"
   ))

;; FSF:
;; (define-coding-system-alias 'iso-2022-jp-1978-irv 'japanese-iso-7bit-1978-irv)
;; (define-coding-system-alias 'old-jis 'japanese-iso-7bit-1978-irv)

(define-coding-system-alias 'old-jis 'iso-2022-jp-1978-irv)

;; (make-coding-system
;;  'japanese-iso-8bit 2 ?E
;;  "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)"
;;  '(ascii japanese-jisx0208 katakana-jisx0201 japanese-jisx0212
;;    short ascii-eol ascii-cntl nil nil single-shift)
;;  '((safe-charsets ascii latin-jisx0201 japanese-jisx0208 japanese-jisx0208-1978
;; 		 katakana-jisx0201 japanese-jisx0212)
;;    (mime-charset . euc-jp)))
;; 
(make-coding-system
 'euc-jp 'iso2022
 "Japanese EUC"
 '(charset-g0 ascii
   charset-g1 japanese-jisx0208
   charset-g2 katakana-jisx0201
   charset-g3 japanese-jisx0212
   short t
   mnemonic "Ja/EUC"
   documentation
   "Japanese EUC (Extended Unix Code), the standard Japanese encoding in Unix.
Equivalent MIME encoding: EUC-JP.

Japanese EUC was the forefather of all the different EUC's, which all follow
a similar structure:

1. Up to four character sets can be encoded.

2. This is a non-modal encoding, i.e. it is impossible to set a global state
   that affects anything more than the directly following character. [Modal
   encodings typically have escape sequences to change global settings, which
   affect all the following characters until the setting is turned off.
   Modal encodings are typically used when it's necessary to support text in
   a wide variety of character sets and still keep basic ASCII compatibility,
   or in cases (e.g. sending email) where the allowed characters that can
   pass the gateway are small and (typically) no high-bit range is available.

3. The first character set is always ASCII or some national variant of it,
   and encoded in the standard ASCII position.  All characters in all other
   character sets are encoded entirely using high-half bytes.  Therefore,
   it is safe to scan for ASCII characters, such as '/' to separate path
   components, in the obvious way.

4. Each of the other three character sets can be of dimension 1, 2, or 3.
   A dimension-1 character set contains 96 bytes; a dimension-2 character
   set contains 96 x 96 bytes; and a dimension-3 character set contains
   96 x 96 x 96 bytes.  94 instead of 96 as the number of characters per
   dimension is also supported.  Character sets of dimensions 1, 2, and 3
   use 1-3 bytes, respectively, to encode a character, and each byte is
   in the range A0-FF (or A1-FE for those with 94 bytes per dimension).

5. The four character sets encoded in EUC are called G0, G1, G2, and G3.
   As mentioned earlier, G0 is ASCII or some variant, and encoded into
   the ASCII positions 00 - 7F.  G1 is encoded directly by laying out
   its bytes.  G2 is encoded using an 8E byte followed by the character's
   bytes.  G3 is encoded using an 8F byte followed by the character's bytes."

   ))

;; FSF:
;; (define-coding-system-alias 'euc-japan-1990 'japanese-iso-8bit)
;; (define-coding-system-alias 'euc-japan 'japanese-iso-8bit)
;; (define-coding-system-alias 'euc-jp 'japanese-iso-8bit)

(define-coding-system-alias 'euc-japan 'euc-jp) ; only for w3
(define-coding-system-alias 'japanese-euc 'euc-jp)

(set-language-info-alist
 "Japanese" '((setup-function . setup-japanese-environment-internal)
	      (exit-function . exit-japanese-environment)
	      (tutorial . "TUTORIAL.ja")
	      (charset japanese-jisx0208 japanese-jisx0208-1978
		       japanese-jisx0212 latin-jisx0201 katakana-jisx0201)
	      (coding-system iso-2022-jp euc-jp
			     shift-jis iso-2022-jp-2)
	      (coding-priority iso-2022-jp euc-jp
			       shift-jis iso-2022-jp-2)
	      ;; These locale names come from the X11R6 locale.alias file.
	      ;; What an incredible fucking mess!!!!!!!!!!!!!!!!!!!!!!!!!!
	      ;; What's worse is that typical Unix implementations of
	      ;; setlocale() return back exactly what you passed them, even
	      ;; though it's perfectly allowed (and in fact done under
	      ;; Windows) to expand the locale to its full form (including
	      ;; encoding), so you have some hint as to the encoding!!!
	      ;;
	      ;; We order them in such a way that we're maximally likely
	      ;; to get an encoding name.
	      ;;
	      (locale
	       ;; SunOS 5.7: ja ja_JP.PCK ja_JP.UTF-8 japanese
	       ;; RedHat Linux 6.2J: ja ja_JP ja_JP.eucJP ja_JP.ujis \
	       ;;   japanese japanese.euc
	       ;; HP-UX 10.20: ja_JP.SJIS ja_JP.eucJPput ja_JP.kana8
	       ;; Cygwin b20.1: ja_JP.EUC
	       ;; FreeBSD 2.2.8: ja_JP.EUC ja_JP.SJIS 

	       ;; EUC locales
	       "ja_JP.EUC"
	       "ja_JP.eucJP"
	       "ja_JP.AJEC"
	       "ja_JP.ujis"
	       "Japanese-EUC"
	       "japanese.euc"

	       ;; Shift-JIS locales
	       "ja_JP.SJIS"
	       "ja_JP.mscode"
	       "ja.SJIS"

	       ;; 7-bit locales
	       "ja_JP.ISO-2022-JP"
	       "ja_JP.jis7"
	       "ja_JP.pjis"
	       "ja_JP.JIS"
	       "ja.JIS"

	       ;; 8-bit locales
	       "ja_JP.jis8"

	       ;; encoding-unspecified locales
	       "ja_JP"
	       "Ja_JP"
	       "Jp_JP"
	       "japanese"
	       "japan"
	       "ja"
	       )

	      (native-coding-system
	       ;; first, see if an explicit encoding was given.
	       #'(lambda (locale)
		   (let ((case-fold-search t))
		     (cond
		      ;; many unix versions
		      ((string-match "\\.euc" locale) 'euc-jp)
		      ((string-match "\\.sjis" locale) 'shift-jis)

		      ;; X11R6 (CJKV p. 471)
		      ((string-match "\\.jis7" locale) 'jis7)
		      ((string-match "\\.jis8" locale) 'jis8)
		      ((string-match "\\.mscode" locale) 'shift-jis)
		      ((string-match "\\.pjis" locale) 'iso-2022-jp)
		      ((string-match "\\.ujis" locale) 'euc-jp)

		      ;; other names in X11R6 locale.alias
		      ((string-match "\\.ajec" locale) 'euc-jp)
		      ((string-match "-euc" locale) 'euc-jp)
		      ((string-match "\\.iso-2022-jp" locale) 'iso-2022-jp)
		      ((string-match "\\.jis" locale) 'jis7) ;; or just jis?
		      )))

	       ;; aix (CJKV p. 465)
	       #'(lambda (locale)
		   (when (eq system-type 'aix)
		     (cond
		      ((string-match "^Ja_JP" locale) 'shift-jis)
		      ((string-match "^ja_JP" locale) 'euc-jp))))

	       ;; other X11R6 locale.alias
	       #'(lambda (locale)
		   (cond
		    ((string-match "^Jp_JP" locale) 'euc-jp)
		    ((and (eq system-type 'hpux) (eq locale "japanese"))
		     'shift-jis)))

	       ;; fallback
	       'euc-jp)

;;	      (input-method . "japanese")
	      (features japan-util)
	      (sample-text . "Japanese (日本語)	こんにちは, ｺﾝﾆﾁﾊ")
	      (documentation . t)))

;;; japanese.el ends here
