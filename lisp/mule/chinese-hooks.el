;;; chinese-hooks.el --- pre-loaded support for Chinese.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.

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

;;; Synched up with: Mule 2.3.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHINESE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       name (concat "Chinese CNS Plane " plane)
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

;; PinYin-ZhuYin
(make-charset 'sisheng "PinYin-ZhuYin"
	      '(registry "sisheng_cwnn\\|OMRON_UDC_ZH"
		dimension 1
		chars 94
		final ?0
		graphic 0
		))

(make-coding-system
 'euc-china 'iso2022
 "Coding-system of Chinese EUC (Extended Unix Code)."
 '(charset-g0 ascii
   charset-g1 chinese-gb2312
   charset-g2 sisheng
   charset-g3 t
   mnemonic "EUC/Zh"
   ))

(make-coding-system
 'hz 'no-conversion
 "Coding-system of Hz/ZW used for Chinese."
 '(mnemonic "Hz/ZW"
   eol-type lf
   post-read-conversion hz2gb-region
   pre-write-conversion gb2hz-region))

(make-coding-system
 'big5 'big5
 "Coding-system of BIG5."
 '(mnemonic "Big5"))

(copy-coding-system 'big5 'big5-eten)

;; If you prefer QUAIL to EGG, please modify below as you wish.
;;(when (and (featurep 'egg) (featurep 'wnn))
;;  (setq wnn-server-type 'cserver)
;;  (load "its/pinyin")
;;  (setq its:*standard-modes*
;;        (cons (its:get-mode-map "PinYin") its:*standard-modes*)))

;; For QUAIL
;; Please add your own quail package if any.

;; For GB character input
;;(add-hook 'quail-package-alist '("py"      "quail/py"))
;;(add-hook 'quail-package-alist '("qj"      "quail/qj"))
;;(add-hook 'quail-package-alist '("punct"   "quail/punct"))
;;(add-hook 'quail-package-alist '("sw"      "quail/sw"))
;;(add-hook 'quail-package-alist '("tonepy"  "quail/tonepy"))
;;(add-hook 'quail-package-alist '("ccdospy" "quail/ccdospy"))
;;(add-hook 'quail-package-alist '("ctlau"   "quail/ctlau"))

;; For BIG5 character input
;;(add-hook 'quail-package-alist '("py-b5"    "quail/py-b5"))
;;(add-hook 'quail-package-alist '("qj-b5"    "quail/qj-b5"))
;;(add-hook 'quail-package-alist '("punct-b5" "quail/punct-b5"))
;;(add-hook 'quail-package-alist '("ctlaub"   "quail/ctlaub"))
;;(add-hook 'quail-package-alist '("zozy"     "quail/zozy"))
;;(add-hook 'quail-package-alist '("etzy"     "quail/etzy"))

;; For Big5 handling

(define-ccl-program ccl-internal-to-big5-1
  '(((r1 = ((((r0 - #x21) * 94) + r1) - #x21))
     (r0 = ((r1 / 157) + #xA1))
     (r1 %= 157)
     (if (r1 < #x3F) (r1 += #x40) (r1 += #x62))))
  "CCL program to convert internal Big5 code (level1) to code point of Big5 font.")

;; 6280 is the number of characters that got shoved into `chinese-big5-1'.
(define-ccl-program ccl-internal-to-big5-2
  '(((r1 = (((((r0 - #x21) * 94) + r1) - #x21) + 6280))
     (r0 = ((r1 / 157) + #xA1))
     (r1 %= 157)
     (if (r1 < #x3F) (r1 += #x40) (r1 += #x62))))
  "CCL program to convert internal Big5 code (level2) to code point of Big5 font.")

(set-charset-ccl-program 'chinese-big5-1 ccl-internal-to-big5-1)
(set-charset-ccl-program 'chinese-big5-2 ccl-internal-to-big5-2)

;; This isn't used and doesn't work yet because CCL doesn't know
;; how to handle things like 'chinese-big5-1 currently.  The
;; original of this program had 152 (the leading byte) in place of
;; chinese-big5-1, etc., but that will never do.  No exposing of
;; leading bytes to the Lisp level!
;(define-ccl-program ccl-big5-to-internal
;  '(0
;    ((if (r0 < #xC9)
;	 ((r2 = (((r0 - #xA1) * 157) + r1))
;	  (r0 = chinese-big5-1))
;       ((r2 = (((r0 - #xC9) * 157) + r1))
;	(r0 = chinese-big5-2)))
;     (if (r1 < #x7F) (r2 -= #x40) (r2 -= #x62))
;     (r1 = ((r2 / 94) + #x21))
;     (r2 = ((r2 % 94) + #x21))
;     ))
;  "CCL program to convert Big5 code to internal code.")

(define-language-environment 'chinese
  "Chinese (includes GB, Big5, and CNS)"
  (lambda ()
    (require 'chinese)
    (set-coding-category-system 'iso-8-2 'euc-china)
    (set-coding-priority-list '(iso-8-2 big5 iso-8-designate))
    (set-pathname-coding-system 'euc-china)
    (set-default-buffer-file-coding-system 'euc-china) ; GB encoding
    (setq terminal-coding-system    'euc-china)
    (setq keyboard-coding-system    'euc-china)
    (add-hook 'comint-exec-hook
              (lambda ()
                (let ((proc (get-buffer-process (current-buffer))))
                  (set-process-input-coding-system  proc 'euc-china)
                  (set-process-output-coding-system proc 'euc-china))))
    (set-buffer-file-coding-system-for-read 'autodetect)
    (set-default-buffer-file-coding-system 'euc-china)
    (setq keyboard-coding-system     'euc-china)
    (setq terminal-coding-system     'euc-china)
    (when (eq 'x (device-type (selected-device)))
      (x-use-halfwidth-roman-font 'chinese-gb2312 "gb1988"))))
;;    (when (featurep 'egg)
;;      (setq-default its:*current-map* (its:get-mode-map "PinYin")))
;;    (setq-default quail-current-package (assoc "py" quail-package-alist))))
;;    ))

(set-coding-category-system 'big5 'big5)
