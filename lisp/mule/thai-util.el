;;; thai-util.el --- utilities for Thai -*- coding: utf-8; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual, thai

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

;;; Synched up with: Emacs 21.1 (language/thai-util.el).

;;; Commentary:

;;; Code:

;; Setting information of Thai characters.

(defconst thai-category-table (make-category-table))
(define-category ?c "Thai consonant" thai-category-table)
(define-category ?v "Thai upper/lower vowel" thai-category-table)
(define-category ?t "Thai tone" thai-category-table)

;; The general composing rules are as follows:
;;
;;                          T
;;       V        T         V                  T
;; CV -> C, CT -> C, CVT -> C, Cv -> C, CvT -> C
;;                                   v         v
;;
;; where C: consonant, V: vowel upper, v: vowel lower, T: tone mark.

(defvar thai-composition-pattern "\\cc\\(\\ct\\|\\cv\\ct?\\)"
  "Regular expression matching a Thai composite sequence.")

(let ((l '((?ก consonant "LETTER KO KAI")				; 0xA1
	   (?ข consonant "LETTER KHO KHAI")				; 0xA2
	   (?ฃ consonant "LETTER KHO KHUAT")				; 0xA3
	   (?ค consonant "LETTER KHO KHWAI")				; 0xA4
	   (?ฅ consonant "LETTER KHO KHON")				; 0xA5
	   (?ฆ consonant "LETTER KHO RAKHANG")				; 0xA6
	   (?ง consonant "LETTER NGO NGU")				; 0xA7
	   (?จ consonant "LETTER CHO CHAN")				; 0xA8
	   (?ฉ consonant "LETTER CHO CHING")				; 0xA9
	   (?ช consonant "LETTER CHO CHANG")				; 0xAA
	   (?ซ consonant "LETTER SO SO")				; 0xAB
	   (?ฌ consonant "LETTER CHO CHOE")				; 0xAC
	   (?ญ consonant "LETTER YO YING")				; 0xAD
	   (?ฎ consonant "LETTER DO CHADA")				; 0xAE
	   (?ฏ consonant "LETTER TO PATAK")				; 0xAF
	   (?ฐ consonant "LETTER THO THAN")				; 0xB0
	   (?ฑ consonant "LETTER THO NANGMONTHO")			; 0xB1
	   (?ฒ consonant "LETTER THO PHUTHAO")				; 0xB2
	   (?ณ consonant "LETTER NO NEN")				; 0xB3
	   (?ด consonant "LETTER DO DEK")				; 0xB4
	   (?ต consonant "LETTER TO TAO")				; 0xB5
	   (?ถ consonant "LETTER THO THUNG")				; 0xB6
	   (?ท consonant "LETTER THO THAHAN")				; 0xB7
	   (?ธ consonant "LETTER THO THONG")				; 0xB8
	   (?น consonant "LETTER NO NU")				; 0xB9
	   (?บ consonant "LETTER BO BAIMAI")				; 0xBA
	   (?ป consonant "LETTER PO PLA")				; 0xBB
	   (?ผ consonant "LETTER PHO PHUNG")				; 0xBC
	   (?ฝ consonant "LETTER FO FA")				; 0xBD
	   (?พ consonant "LETTER PHO PHAN")				; 0xBE
	   (?ฟ consonant "LETTER FO FAN")				; 0xBF
	   (?ภ consonant "LETTER PHO SAMPHAO")				; 0xC0
	   (?ม consonant "LETTER MO MA")				; 0xC1
	   (?ย consonant "LETTER YO YAK")				; 0xC2
	   (?ร consonant "LETTER RO RUA")				; 0xC3
	   (?ฤ vowel-base "LETTER RU (Pali vowel letter)")		; 0xC4
	   (?ล consonant "LETTER LO LING")				; 0xC5
	   (?ฦ vowel-base "LETTER LU (Pali vowel letter)")		; 0xC6
	   (?ว consonant "LETTER WO WAEN")				; 0xC7
	   (?ศ consonant "LETTER SO SALA")				; 0xC8
	   (?ษ consonant "LETTER SO RUSI")				; 0xC9
	   (?ส consonant "LETTER SO SUA")				; 0xCA
	   (?ห consonant "LETTER HO HIP")				; 0xCB
	   (?ฬ consonant "LETTER LO CHULA")				; 0xCC
	   (?อ consonant "LETTER O ANG")				; 0xCD
	   (?ฮ consonant "LETTER HO NOK HUK")				; 0xCE
	   (?ฯ special "PAI YAN NOI (abbreviation)")			; 0xCF
	   (?ะ vowel-base "VOWEL SIGN SARA A")				; 0xD0
	   (?ั vowel-upper "VOWEL SIGN MAI HAN-AKAT N/S-T")		; 0xD1
	   (?า vowel-base "VOWEL SIGN SARA AA")				; 0xD2
	   (?ำ vowel-base "VOWEL SIGN SARA AM")				; 0xD3
	   (?ิ vowel-upper "VOWEL SIGN SARA I N/S-T")			; 0xD4
	   (?ี vowel-upper "VOWEL SIGN SARA II N/S-T")			; 0xD5
	   (?ึ vowel-upper "VOWEL SIGN SARA UE N/S-T")			; 0xD6
	   (?ื vowel-upper "VOWEL SIGN SARA UEE N/S-T")			; 0xD7
	   (?ุ vowel-lower "VOWEL SIGN SARA U N/S-B")			; 0xD8
	   (?ู vowel-lower "VOWEL SIGN SARA UU N/S-B")			; 0xD9
	   (?ฺ vowel-lower "VOWEL SIGN PHINTHU N/S-B (Pali virama)")	; 0xDA
	   (?� invalid nil)						; 0xDA
	   (?� invalid nil)						; 0xDC
	   (?� invalid nil)						; 0xDC
	   (?� invalid nil)						; 0xDC
	   (?฿ special "BAHT SIGN (currency symbol)")			; 0xDF
	   (?เ vowel-base "VOWEL SIGN SARA E")				; 0xE0
	   (?แ vowel-base "VOWEL SIGN SARA AE")				; 0xE1
	   (?โ vowel-base "VOWEL SIGN SARA O")				; 0xE2
	   (?ใ vowel-base "VOWEL SIGN SARA MAI MUAN")			; 0xE3
	   (?ไ vowel-base "VOWEL SIGN SARA MAI MALAI")			; 0xE4
	   (?ๅ vowel-base "LAK KHANG YAO")				; 0xE5
	   (?ๆ special "MAI YAMOK (repetion)")				; 0xE6
	   (?็ vowel-upper "VOWEL SIGN MAI TAI KHU N/S-T")		; 0xE7
	   (?่ tone "TONE MAI EK N/S-T")				; 0xE8
	   (?้ tone "TONE MAI THO N/S-T")				; 0xE9
	   (?๊ tone "TONE MAI TRI N/S-T")				; 0xEA
	   (?๋ tone "TONE MAI CHATTAWA N/S-T")				; 0xEB
	   (?์ tone "THANTHAKHAT N/S-T (cancellation mark)")		; 0xEC
	   (?ํ tone "NIKKHAHIT N/S-T (final nasal)")			; 0xED
	   (?๎ vowel-upper "YAMAKKAN N/S-T")				; 0xEE
	   (?๏ special "FONRMAN")					; 0xEF
	   (?๐ special "DIGIT ZERO")					; 0xF0
	   (?๑ special "DIGIT ONE")					; 0xF1
	   (?๒ special "DIGIT TWO")					; 0xF2
	   (?๓ special "DIGIT THREE")					; 0xF3
	   (?๔ special "DIGIT FOUR")					; 0xF4
	   (?๕ special "DIGIT FIVE")					; 0xF5
	   (?๖ special "DIGIT SIX")					; 0xF6
	   (?๗ special "DIGIT SEVEN")					; 0xF7
	   (?๘ special "DIGIT EIGHT")					; 0xF8
	   (?๙ special "DIGIT NINE")					; 0xF9
	   (?๚ special "ANGKHANKHU (ellipsis)")				; 0xFA
	   (?๛ special "KHOMUT (beginning of religious texts)")		; 0xFB
	   (?� invalid nil)						; 0xFC
	   (?� invalid nil)						; 0xFD
	   (?� invalid nil)						; 0xFE
	   ))
      elm)
  (while l
    (setq elm (car l) l (cdr l))
    (let ((char (car elm))
	  (ptype (nth 1 elm)))
      (put-char-code-property char 'phonetic-type ptype)
      (cond ((eq ptype 'consonant)
	     (modify-category-entry char ?c thai-category-table))
	    ((memq ptype '(vowel-upper vowel-lower))
	     (modify-category-entry char ?v thai-category-table))
	    ((eq ptype 'tone)
	     (modify-category-entry char ?t thai-category-table)))
      (put-char-code-property char 'name (nth 2 elm)))))

;;;###autoload
(defun thai-compose-region (beg end)
  "Compose Thai characters in the region.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (with-category-table thai-category-table
      (while (re-search-forward thai-composition-pattern nil t)
	(compose-region (match-beginning 0) (match-end 0))))))

;;;###autoload
(defun thai-compose-string (string)
  "Compose Thai characters in STRING and return the resulting string."
  (with-category-table thai-category-table
    (let ((idx 0))
      (while (setq idx (string-match thai-composition-pattern string idx))
	(compose-string string idx (match-end 0))
	(setq idx (match-end 0)))))
  string)
      
;;;###autoload
(defun thai-compose-buffer ()
  "Compose Thai characters in the current buffer."
  (interactive)
  (thai-compose-region (point-min) (point-max)))

;;;###autoload
(defun thai-post-read-conversion (len)
  (thai-compose-region (point) (+ (point) len))
  len)

;;;###autoload
(defun thai-composition-function (from to pattern &optional string)
  "Compose Thai text in the region FROM and TO.
The text matches the regular expression PATTERN.
Optional 4th argument STRING, if non-nil, is a string containing text
to compose.

The return value is number of composed characters."
  (if (< (1+ from) to)
      (prog1 (- to from)
	(if string
	    (compose-string string from to)
	  (compose-region from to))
	(- to from))))

;;
(provide 'thai-util)

;;; thai-util.el ends here
