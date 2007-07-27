;;; chinese.el --- Support for Chinese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2000, 2001, 2002 Ben Wing.

;; Keywords: multilingual, Chinese

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

;;; Commentary:

;; For Chinese, three character sets GB2312, BIG5, and CNS11643 are
;; supported.

;;; Code:

(eval-when-compile (progn (require 'ccl) (require 'china-util)))

;; Syntax of Chinese characters.
(loop for row in '(33 34 41)
      do (modify-syntax-entry `[chinese-gb2312 ,row] "."))

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
       `(registries 
         ,(vector (concat "cns11643.1992-" plane ))
         dimension 2
         chars 94
         final ,final
         graphic 0
	 short-name ,(concat "CNS11643-" plane)
	 long-name ,(format "CNS11643-%s (Chinese traditional): ISO-IR-183"
			    plane)))
      (modify-syntax-entry   name "w")
      (modify-category-entry name ?t)
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
 `(registries ["isoir165-0"]
   dimension 2
   chars 94
   final ?E
   graphic 0
   short-name "ISO-IR-165"
   long-name "ISO-IR-165 (CCITT Extended GB; Chinese simplified)"))

;; PinYin-ZhuYin
(make-charset 'chinese-sisheng 
	      "SiSheng characters for PinYin/ZhuYin"
	      '(dimension
		1
		;; XEmacs addition: second half of registry spec
		registries ["omron_udc_zh-0" "sisheng_cwnn-0"]
		chars 94
		columns 1
		direction l2r
		final ?0
		graphic 0
		short-name "SiSheng"
		long-name "SiSheng (PinYin/ZhuYin)"
		))

;; If you prefer QUAIL to EGG, please modify below as you wish.
;;(when (and (featurep 'egg) (featurep 'wnn))
;;  (setq wnn-server-type 'cserver)
;;  (load "pinyin")
;;  (setq its:*standard-modes*
;;        (cons (its:get-mode-map "PinYin") its:*standard-modes*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'iso-2022-cn 2 ?C
;;  "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN)"
;;  '(ascii
;;    (nil chinese-gb2312 chinese-cns11643-1)
;;    (nil chinese-cns11643-2)
;;    nil
;;    nil ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
;;    init-bol)
;;  '((safe-charsets ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
;;    (mime-charset . iso-2022-cn)))

;; (define-coding-system-alias 'chinese-iso-7bit 'iso-2022-cn)

;; (make-coding-system
;;  'iso-2022-cn-ext 2 ?C
;;  "ISO 2022 based 7bit encoding for Chinese GB and CNS (MIME:ISO-2022-CN-EXT)"
;;  '(ascii
;;    (nil chinese-gb2312 chinese-cns11643-1)
;;    (nil chinese-cns11643-2)
;;    (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
;;         chinese-cns11643-6 chinese-cns11643-7)
;;    nil ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
;;    init-bol)
;;  '((safe-charsets ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2
;;                   chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
;;                   chinese-cns11643-6 chinese-cns11643-7)
;;    (mime-charset . iso-2022-cn-ext)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'chinese-iso-8bit 2 ?c
;;  "ISO 2022 based EUC encoding for Chinese GB2312 (MIME:CN-GB-2312)"
;;  '(ascii chinese-gb2312 nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil)
;;  '((safe-charsets ascii chinese-gb2312)
;;    (mime-charset . cn-gb-2312)))

(make-coding-system
 'cn-gb-2312 'iso2022
 "Chinese EUC"
 '(charset-g0 ascii
   charset-g1 chinese-gb2312
   charset-g2 chinese-sisheng
   charset-g3 t
   mnemonic "Zh-GB/EUC"
   documentation
   "Chinese EUC (Extended Unix Code), the standard Chinese encoding on Unix.
This follows the same overall EUC principles as Japanese EUC (see the
description under Japanese EUC), but specifies different character sets:

G0: ASCII
G1: Chinese-GB2312
G2: Sisheng (PinYin - ZhuYin)"
   ))

;; For consistency with euc-jp, euc-ko
(define-coding-system-alias 'euc-cn 'cn-gb-2312)

(define-coding-system-alias 'gb2312 'cn-gb-2312)
(define-coding-system-alias 'chinese-euc 'cn-gb-2312)

;; (make-coding-system
;;  'chinese-hz 0 ?z
;;  "Hz/ZW 7-bit encoding for Chinese GB2312 (MIME:HZ-GB-2312)"
;;  nil
;;  '((safe-charsets ascii chinese-gb2312)
;;    (mime-charset . hz-gb-2312)
;;    (post-read-conversion . post-read-decode-hz)
;;    (pre-write-conversion . pre-write-encode-hz)))
;; (put 'chinese-hz 'post-read-conversion 'post-read-decode-hz)
;; (put 'chinese-hz 'pre-write-conversion 'pre-write-encode-hz)

(make-coding-system
 'hz-gb-2312 'no-conversion
 "Hz/ZW (Chinese)"
 '(mnemonic "Zh-GB/Hz"
   eol-type lf
   post-read-conversion post-read-decode-hz
   pre-write-conversion pre-write-encode-hz
   documentation "Hz/ZW 7-bit encoding for Chinese GB2312 (MIME:HZ-GB-2312)"
))

;; (define-coding-system-alias 'hz-gb-2312 'chinese-hz)
;; (define-coding-system-alias 'hz 'chinese-hz)

(define-coding-system-alias 'hz 'hz-gb-2312)

(defun post-read-decode-hz (len)
  (let ((pos (point))
	(buffer-modified-p (buffer-modified-p))
	;last-coding-system-used
	)
    (prog1
	(decode-hz-region pos (+ pos len))
      (set-buffer-modified-p buffer-modified-p))))

(defun pre-write-encode-hz (from to)
  (let ((buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    ;(let (last-coding-system-used)
    (encode-hz-region 1 (point-max))
    nil))

(set-language-info-alist
 "Chinese-GB" '((setup-function . setup-chinese-gb-environment-internal)
		(charset chinese-gb2312 chinese-sisheng)
		(coding-system cn-gb-2312 iso-2022-7bit hz-gb-2312)
		(coding-priority cn-gb-2312 big5 iso-2022-7bit)
		(cygwin-locale "zh")
		(locale "zh_CN.eucCN" "zh_CN.EUC" "zh_CN"
			"chinese-s" "zh"
			 (lambda (arg)
			      (and arg (let ((case-fold-search t))
					 (string-match "^zh_.*.GB.*" arg)))))
		(mswindows-locale ("CHINESE" . "CHINESE_SIMPLIFIED"))
		(native-coding-system cn-gb-2312)
		(input-method . "chinese-py-punct")
		(features china-util)
		(sample-text . "Chinese ($AVPND(B,$AFUM(;0(B,$A::So(B)	$ADc:C(B")
		(documentation .
"Supports Simplified Chinese, used in mainland China.
Uses the GB2312 character set."))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'chinese-big5 3 ?B "BIG5 8-bit encoding for Chinese (MIME:CN-BIG5)"
;;  nil
;;  '((safe-charsets ascii chinese-big5-1 chinese-big5-2)
;;    (mime-charset . cn-big5)
;;    (charset-origin-alist (chinese-big5-1  "BIG5" encode-big5-char)
;;                          (chinese-big5-2  "BIG5" encode-big5-char))))

(make-coding-system
 'big5 'big5
 "Big5"
 '(mnemonic "Zh/Big5"
   documentation
   "A non-modal encoding formed by five large Taiwanese companies
\(hence \"Big5\") to produce a character set and encoding for
traditional Chinese writing.  Big5 encodes some 13,000+ characters.
ASCII is encoded as normal, and Chinese characters as two bytes, but
Chinese characters do not exclusively use the high half.  The first
byte is in the high half standard position A1-FE, but the second byte
is in either low 40-7E or high A1-FE.  Thus Big5 suffers from the
classic \"it might look like a slash, but it's really the second byte
of a Chinese character\"."))

;; (define-coding-system-alias 'big5 'chinese-big5)
;; (define-coding-system-alias 'cn-big5 'chinese-big5)

(define-coding-system-alias 'cn-big5 'big5)

;; Big5 font requires special encoding.
(define-ccl-program ccl-encode-big5-font
  `(0
    ;; In:  R0:chinese-big5-1 or chinese-big5-2
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    ((r2 = ((((r1 - #x21) * 94) + r2) - #x21))
     (if (r0 == ,(charset-id 'chinese-big5-2)) (r2 += 6280))
     (r1 = ((r2 / 157) + #xA1))
     (r2 %= 157)
     (if (r2 < #x3F) (r2 += #x40) (r2 += #x62))))
  "CCL program to encode a Big5 code to code point of Big5 font.")

(set-charset-ccl-program 'chinese-big5-1 'ccl-encode-big5-font)
(set-charset-ccl-program 'chinese-big5-2 'ccl-encode-big5-font)

(set-language-info-alist
 "Chinese-BIG5" '((charset chinese-big5-1 chinese-big5-2)
		  (coding-system big5 iso-2022-7bit)
		  (coding-priority big5 cn-gb-2312 iso-2022-7bit)
		  (cygwin-locale "zh_TW")
		  (locale "zh_TW.Big5" "zh_TW.big5" "zh_CN.big5" "zh_TW"
			  "chinese-t"
			  (lambda (arg)
			      (and arg (let ((case-fold-search t))
					 (string-match "^zh_.*.BIG5.*" arg)))))
		  (mswindows-locale ("CHINESE" . "CHINESE_TRADITIONAL"))
		  (native-coding-system big5)
		  (input-method . "chinese-py-punct-b5")
		  (features china-util)
		  (sample-text . "Cantonese ($(0GnM$(B,$(0N]0*Hd(B)	$(0*/=((B, $(0+$)p(B")
		  (documentation .
"Supports Traditional Chinese, used in Taiwan, Hong Kong, and Singapore.
Uses the Chinese Big5 character set."
))
 '("Chinese"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-language-info-alist
;;  "Chinese-CNS" '((charset chinese-cns11643-1 chinese-cns11643-2
;;                           chinese-cns11643-3 chinese-cns11643-4
;;                           chinese-cns11643-5 chinese-cns11643-6
;;                           chinese-cns11643-7)
;;                  (coding-system iso-2022-cn)
;;                  (coding-priority iso-2022-cn chinese-big5 chinese-iso-8bit)
;;                  (features china-util)
;;                  (input-method . "chinese-cns-quick")
;;                  (documentation . "Support for Chinese CNS character sets."))
;;  '("Chinese"))

;;; chinese.el ends here
