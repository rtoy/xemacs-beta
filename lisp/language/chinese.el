;;; chinese.el --- Support for Chinese

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1997 MORIOKA Tomohiko

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

;; Syntax of Chinese characters.
(modify-syntax-entry 'chinese-gb2312 "w")
(loop for row in '(33 34 41)
      do (modify-syntax-entry `[chinese-gb2312 ,row] "."))
;;(loop for row from 35 to  40
;;      do (modify-syntax-entry `[chinese-gb2312 ,row] "w"))
;;(loop for row from 42 to 126
;;      do (modify-syntax-entry `[chinese-gb2312 ,row] "w"))

(modify-syntax-entry 'chinese-cns11643-1  "w")
(modify-syntax-entry 'chinese-cns11643-2  "w")
(modify-syntax-entry 'chinese-big5-1 "w")
(modify-syntax-entry 'chinese-big5-2 "w")

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
         graphic 0))
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
(make-charset
 'chinese-isoir165
 "ISO-IR-165 (CCITT Extended GB; Chinese simplified)"
 `(registry "isoir165"
   dimension 2
   chars 94
   final ?E
   graphic 0))

;; PinYin-ZhuYin
(make-charset 'sisheng "PinYin-ZhuYin"
	      '(registry "sisheng_cwnn\\|OMRON_UDC_ZH"
		dimension 1
		chars 94
		final ?0
		graphic 0
		))

;; If you prefer QUAIL to EGG, please modify below as you wish.
;;(when (and (featurep 'egg) (featurep 'wnn))
;;  (setq wnn-server-type 'cserver)
;;  (load "its/pinyin")
;;  (setq its:*standard-modes*
;;        (cons (its:get-mode-map "PinYin") its:*standard-modes*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese (general)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'iso-2022-cn 2 ?C
;;  "Coding system ISO-2022-CN for Chinese (GB and CNS character sets)."
;;  '(ascii
;;    (nil chinese-gb2312 chinese-cns11643-1)
;;    (nil chinese-cns11643-2)
;;    (nil chinese-cns11643-3 chinese-cns11643-4 chinese-cns11643-5
;;         chinese-cns11643-6 chinese-cns11643-7)
;;    nil ascii-eol ascii-cntl seven locking-shift single-shift nil nil nil
;;    init-bol))

;; (define-coding-system-alias 'iso-2022-cn 'iso-2022-cn-ext)

(defun describe-chinese-support ()
  "Describe how Emacs supports Chinese."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (get-language-info "Chinese" 'documentation))
    (princ "\n")))
	   
(set-language-info-alist
 "Chinese" '((describe-function . describe-chinese-support)
	     (documentation . "\
Emacs provides the following three kinds of Chinese support:
  Chinese-GB: for users of the charset GB2312
  Chinese-BIG5: for users of the charset Big5
  Chinese-CNS: for users of the charset CNS11643 family
Please specify one of them to get more information.")
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese GB2312 (simplified) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'cn-gb-2312 2 ?C
;;  "Coding-system of Chinese EUC (so called GB Encoding)."
;;  '((ascii t) chinese-gb2312 chinese-sisheng nil
;;    nil ascii-eol ascii-cntl nil nil single-shift nil))

(make-coding-system
 'cn-gb-2312 'iso2022
 "Coding-system of Chinese EUC (Extended Unix Code)."
 '(charset-g0 ascii
   charset-g1 chinese-gb2312
   charset-g2 sisheng
   charset-g3 t
   mnemonic "Zh-GB/EUC"
   ))

;;(define-coding-system-alias 'cn-gb-2312 'euc-china)

(copy-coding-system 'cn-gb-2312 'gb2312)
(copy-coding-system 'cn-gb-2312 'chinese-euc)

;; (make-coding-system
;;  'hz-gb-2312 0 ?z
;;  "Codins-system of Hz/ZW used for Chinese (GB)."
;;  nil)
;; (put 'hz-gb-2312 'post-read-conversion 'post-read-decode-hz)
;; (put 'hz-gb-2312 'pre-write-conversion 'pre-write-encode-hz)

(make-coding-system
 'hz-gb-2312 'no-conversion
 "Coding-system of Hz/ZW used for Chinese."
 '(mnemonic "Zh-GB/Hz"
   eol-type lf
   post-read-conversion post-read-decode-hz
   pre-write-conversion pre-write-encode-hz))

;; (define-coding-system-alias 'hz-gb-2312 'hz)

(copy-coding-system 'hz-gb-2312 'hz)
(copy-coding-system 'hz-gb-2312 'chinese-hz)

(defun post-read-decode-hz (len)
  (let ((pos (point)))
    (decode-hz-region pos (+ pos len))))

(defun pre-write-encode-hz (from to)
  (let ((buf (current-buffer))
	(work (get-buffer-create " *pre-write-encoding-work*")))
    (set-buffer work)
    (erase-buffer)
    (if (stringp from)
	(insert from)
      (insert-buffer-substring buf from to))
    (encode-hz-region 1 (point-max))
    nil))

(register-input-method
 "Chinese-GB" '("quail-ccdospy" quail-use-package "quail/ccdospy"))
(register-input-method
 "Chinese-GB" '("quail-ctlau" quail-use-package "quail/ctlau"))
(register-input-method
 "Chinese-GB" '("quail-punct" quail-use-package "quail/punct"))
(register-input-method
 "Chinese-GB" '("quail-qj" quail-use-package "quail/qj"))
(register-input-method
 "Chinese-GB" '("quail-sw" quail-use-package "quail/sw"))
(register-input-method
 "Chinese-GB" '("quail-ziranma" quail-use-package "quail/ziranma"))
(register-input-method
 "Chinese-GB" '("quail-tonepy" quail-use-package "quail/tonepy"))
(register-input-method
 "Chinese-GB" '("quail-py" quail-use-package "quail/py"))

(defun setup-chinese-gb-environment ()
  "Setup multilingual environment (MULE) for Chinese GB2312 users."
  (interactive)
  (setq primary-language "Chinese-GB")

  (setq coding-category-iso-8-2 'cn-gb-2312)
  (setq coding-category-iso-else 'iso-2022-cn)
  (setq coding-category-big5 'cn-big5)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-iso-8-2
     coding-category-big5
     coding-category-iso-8-1
     coding-category-internal
     ))

  (setq-default buffer-file-coding-system 'cn-gb-2312)
  (set-terminal-coding-system 'cn-gb-2312)
  (set-keyboard-coding-system 'cn-gb-2312)

  (setq default-input-method '("Chinese-GB" . "quail-py"))
  )

(defun describe-chinese-gb-support ()
  "Describe how Emacs supports Chinese for GB2312 users."
  (interactive)
  (describe-language-support-internal "Chinese-GB"))

(set-language-info-alist
 "Chinese-GB" '((setup-function . setup-chinese-gb-environment)
		(describe-function . describe-chinese-gb-support)
		(charset . (chinese-gb2312 chinese-sisheng))
		(coding-system . (cn-gb-2312 hz-gb-2312 iso-2022-cn))
		(sample-text . "Chinese ($AVPND(B,$AFUM(;0(B,$A::So(B)	$ADc:C(B")
		(documentation . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese BIG5 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-coding-system
;;  'big5 3 ?B
;;  "Coding-system of BIG5.")

;;(define-coding-system-alias 'big5 'cn-big5)

(make-coding-system
 'big5 'big5
 "Coding-system of BIG5."
 '(mnemonic "Zh/Big5"))

(copy-coding-system 'big5 'cn-big5)
(copy-coding-system 'big5 'chinese-big5)

;; Big5 font requires special encoding.
;; (define-ccl-program ccl-encode-big5-font
;;   `(0
;;     ;; In:  R0:chinese-big5-1 or chinese-big5-2
;;     ;;      R1:position code 1
;;     ;;      R2:position code 2
;;     ;; Out: R1:font code point 1
;;     ;;      R2:font code point 2
;;     ((r2 = ((((r1 - ?\x21) * 94) + r2) - ?\x21))
;;      (if (r0 == ,(charset-id 'chinese-big5-2)) (r2 += 6280))
;;      (r1 = ((r2 / 157) + ?\xA1))
;;      (r2 %= 157)
;;      (if (r2 < ?\x3F) (r2 += ?\x40) (r2 += ?\x62))))
;;   "CCL program to encode a Big5 code to code point of Big5 font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "big5" ccl-encode-big5-font) font-ccl-encoder-alist))

(define-ccl-program ccl-encode-big5-1-font
  '(((r1 = ((((r0 - #x21) * 94) + r1) - #x21))
     (r0 = ((r1 / 157) + #xA1))
     (r1 %= 157)
     (if (r1 < #x3F) (r1 += #x40) (r1 += #x62))))
  "CCL program to encode a Big5 code (level1) to code point of Big5 font.")

;; 6280 is the number of characters that got shoved into `chinese-big5-1'.
(define-ccl-program ccl-encode-big5-2-font
  '(((r1 = (((((r0 - #x21) * 94) + r1) - #x21) + 6280))
     (r0 = ((r1 / 157) + #xA1))
     (r1 %= 157)
     (if (r1 < #x3F) (r1 += #x40) (r1 += #x62))))
  "CCL program to encode a Big5 code (level2) to code point of Big5 font.")

(set-charset-ccl-program 'chinese-big5-1 ccl-encode-big5-1-font)
(set-charset-ccl-program 'chinese-big5-2 ccl-encode-big5-2-font)

(register-input-method
 "Chinese-BIG5" '("quail-qj-b5" quail-use-package "quail/qj-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-zozy" quail-use-package "quail/zozy"))
(register-input-method
 "Chinese-BIG5" '("quail-tsangchi-b5" quail-use-package "quail/tsangchi-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-py-b5" quail-use-package "quail/py-b5"))
(register-input-method
 "Chinese-BIG5" '("quail-quick-b5" quail-use-package "quail/quick-bt"))
(register-input-method
 "Chinese-BIG5" '("quail-etzy" quail-use-package "quail/etzy"))
(register-input-method
 "Chinese-BIG5" '("quail-ecdict" quail-use-package "quail/ecdict"))
(register-input-method
 "Chinese-BIG5" '("quail-ctlaub" quail-use-package "quail/ctlaub"))
(register-input-method
 "Chinese-BIG5" '("quail-array30" quail-use-package "quail/array30"))
(register-input-method
 "Chinese-BIG5" '("quail-4corner" quail-use-package "quail/4corner"))

(defun setup-chinese-big5-environment ()
  "Setup multilingual environment (MULE) for Chinese Big5 users."
  (interactive)
  (setq primary-language "Chinese-BIG5")

  (setq coding-category-big5 'cn-big5)
  (setq coding-category-iso-else 'iso-2022-cn)
  (setq coding-category-iso-8-2 'cn-gb-2312)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-big5
     coding-category-iso-8-2))

  (setq-default buffer-file-coding-system 'cn-big5)
  (set-terminal-coding-system 'cn-big5)
  (set-keyboard-coding-system 'cn-big5)

  (setq default-input-method '("Chinese-BIG5" . "quail-py-b5"))
  )

(defun describe-chinese-big5-support ()
  "Describe how Emacs supports Chinese for Big5 users."
  (interactive)
  (describe-language-support-internal "Chinese-BIG5"))

(set-language-info-alist
 "Chinese-BIG5" '((setup-function . setup-chinese-big5-environment)
		  (describe-function . describe-chinese-big5-support)
		  (charset . (chinese-big5-1 chinese-big5-2))
		  (coding-system . (cn-big5 iso-2022-cn))
		  (sample-text . "Cantonese ($(0GnM$(B,$(0N]0*Hd(B)	$(0*/=((B, $(0+$)p(B")
		  (documentation . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese CNS11643 (traditional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-input-method
 "Chinese-CNS" '("quail-quick-cns" quail-use-package "quail/quick-cns"))
(register-input-method
 "Chinese-CNS" '("quail-tsangchi-cns" quail-use-package "quail/tsangchi-cns"))

(defun setup-chinese-cns-environment ()
  "Setup multilingual environment (MULE) for Chinese CNS11643 family users."
  (interactive)
  (setq primary-language "Chinese-CNS")

  (setq coding-category-iso-else 'iso-2022-cn)
  (setq coding-category-big5 'cn-big5)
  (setq coding-category-iso-8-2 'cn-gb-2312)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-else
     coding-category-iso-8-2
     coding-category-big5))

  (setq-default buffer-file-coding-system 'iso-2022-cn)
  (set-terminal-coding-system 'iso-2022-cn)
  (set-keyboard-coding-system 'iso-2022-cn)

  (setq default-input-method '("Chinese-CNS" . "quail-py-cns"))
  )

(defun describe-chinese-cns-support ()
  "Describe how Emacs supports Chinese for CNS11643 family users."
  (interactive)
  (describe-language-support-internal "Chinese-CNS"))

(set-language-info-alist
 "Chinese-CNS" '((setup-function . setup-chinese-cns-environment)
		 (describe-function . describe-chinese-cns-support)
		 (charset . (chinese-cns11643-1 chinese-cns11643-2
			     chinese-cns11643-3 chinese-cns11643-4
			     chinese-cns11643-5 chinese-cns11643-6
			     chinese-cns11643-7))
		 (coding-system . (iso-2022-cn))
		 (documentation . nil)))


;;; for XEmacs (will be obsoleted)

(define-language-environment 'chinese
  "Chinese (includes GB, Big5, and CNS)"
  (lambda ()
    (require 'chinese)
    (set-coding-category-system 'iso-8-2 'cn-gb-2312)
    (set-coding-priority-list '(iso-8-2 big5 iso-8-designate))
    (set-pathname-coding-system 'cn-gb-2312)
    (set-default-buffer-file-coding-system 'cn-gb-2312) ; GB encoding
    (setq terminal-coding-system    'cn-gb-2312)
    (setq keyboard-coding-system    'cn-gb-2312)
    (add-hook 'comint-exec-hook
              (lambda ()
                (let ((proc (get-buffer-process (current-buffer))))
                  (set-process-input-coding-system  proc 'cn-gb-2312)
                  (set-process-output-coding-system proc 'cn-gb-2312))))
    (set-buffer-file-coding-system-for-read 'automatic-conversion)
    (set-default-buffer-file-coding-system 'cn-gb-2312)
    (setq keyboard-coding-system           'cn-gb-2312)
    (setq terminal-coding-system           'cn-gb-2312)
    (when (eq 'x (device-type (selected-device)))
      (x-use-halfwidth-roman-font 'chinese-gb2312 "gb1988"))))
;;    (when (featurep 'egg)
;;      (setq-default its:*current-map* (its:get-mode-map "PinYin")))
;;    (setq-default quail-current-package (assoc "py" quail-package-alist))))
;;    ))

(set-coding-category-system 'big5 'big5)

;;; chinese.el ends here
