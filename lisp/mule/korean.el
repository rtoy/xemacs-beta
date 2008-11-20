;;; korean.el --- Support for Korean -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Korean

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

;; For Korean, the character set KSC5601 is supported.

;;; Code:

; (make-charset 'korean-ksc5601 
; 	      "KSC5601 Korean Hangul and Hanja: ISO-IR-149"
; 	      '(dimension
; 		2
; 		registry "KSC5601.1989"
; 		chars 94
; 		columns 2
; 		direction l2r
; 		final ?C
; 		graphic 0
; 		short-name "KSC5601"
; 		long-name "KSC5601 (Korean): ISO-IR-149"
; 		))

;; Syntax of Korean characters.
(loop for row from 33 to  34 do
      (modify-syntax-entry `[korean-ksc5601 ,row] "."))
(loop for row from 38 to  41 do
      (modify-syntax-entry `[korean-ksc5601 ,row] "."))

;; Setting for coding-system and quail were moved to
;; language/korean.el.

(make-coding-system
 'iso-2022-int-1 'iso2022
 "ISO-2022-INT-1 (Korean)"
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   safe-charsets (ascii korean-ksc5601)
   short t
   seven t
   lock-shift t
   mnemonic "INT-1"))

;; EGG specific setup
(define-egg-environment 'korean
  "Korean settings for egg"
  (lambda ()
    (with-boundp '(its:*standard-modes* its:*current-map* wnn-server-type
					egg-default-startup-file)
      (with-fboundp 'its:get-mode-map
	(when (not (featurep 'egg-kor))
	  (load "its-hangul")
	  (setq its:*standard-modes*
		(cons (its:get-mode-map "hangul") its:*standard-modes*))
	  (provide 'egg-kor))
	(setq wnn-server-type 'kserver)
	(setq egg-default-startup-file "eggrc-wnn")
	(setq-default its:*current-map* (its:get-mode-map "hangul"))))))

;; (make-coding-system
;;  'korean-iso-8bit 2 ?K
;;  "ISO 2022 based EUC encoding for Korean KSC5601 (MIME:EUC-KR)"
;;  '(ascii korean-ksc5601 nil nil
;;    nil ascii-eol ascii-cntl)
;;  '((safe-charsets ascii korean-ksc5601)
;;    (mime-charset . euc-kr)))

(make-coding-system
 'euc-kr 'iso2022
 "Korean EUC"
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   mnemonic "ko/EUC"
   safe-charsets (ascii korean-ksc5601)
   documentation
   "Korean EUC (Extended Unix Code), the standard Korean encoding on Unix.
This follows the same overall EUC principles (see the description under
Japanese EUC), but specifies different character sets:

G0: ASCII
G1: Korean-KSC5601"
   eol-type nil))

;;(define-coding-system-alias 'euc-kr 'euc-korea)

(define-coding-system-alias 'korean-euc 'euc-kr)

;; (make-coding-system
;;  'iso-2022-kr 2 ?k
;;  "ISO 2022 based 7-bit encoding for Korean KSC5601 (MIME:ISO-2022-KR)."
;;  '(ascii (nil korean-ksc5601) nil nil
;;          nil ascii-eol ascii-cntl seven locking-shift nil nil nil nil nil
;;          designation-bol)
;;  '((safe-charsets ascii korean-ksc5601)
;;    (mime-charset . iso-2022-kr)))

(make-coding-system
 'iso-2022-kr 'iso2022
 "ISO-2022-KR (Korean mail)"
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   force-g1-on-output t
   seven t
   lock-shift t
   safe-charsets (ascii korean-ksc5601)
   mnemonic "Ko/7bit"
   documentation "Coding-System used for communication with mail in Korea."
   eol-type lf))

;; (define-coding-system-alias 'korean-iso-7bit-lock 'iso-2022-kr)

(set-language-info-alist
 "Korean" '((setup-function . setup-korean-environment-internal)
	    (exit-function . exit-korean-environment)
	    (tutorial . "TUTORIAL.ko")
	    (charset korean-ksc5601)
	    (coding-system euc-kr iso-2022-kr)
	    (coding-priority euc-kr iso-2022-kr)
	    (locale "ko_KR.eucKR" "ko_KR.EUC" "ko_KR.euc" "ko_KR" "ko")
	    (native-coding-system euc-kr)
	    (input-method . "korean-hangul")
	    (features korea-util)
	    (sample-text . "Hangul (한글)	안녕하세요, 안녕하십니까")
	    (documentation . "\
The following key bindings are available while using Korean input methods:
  Shift-SPC:	toggle-korean-input-mthod
  Control-F9:	quail-hangul-switch-symbol-ksc
  F9:		quail-hangul-switch-hanja")
	    ))

;;; korean.el ends here
