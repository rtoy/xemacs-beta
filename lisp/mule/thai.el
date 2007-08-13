;; Thai language specific setup for Mule
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is part of XEmacs.
;; This file contains European characters

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

;;; 93.1.21  created for Mule Ver.0.9.7.1 by K.Handa <handa@etl.go.jp>

(defconst thai-character-alist
  '((?,T!(B . consonant) ; 0xA1: LETTER KO KAI
    (?,T"(B . consonant) ; 0xA2: LETTER KHO KHAI
    (?,T#(B . consonant) ; 0xA3: LETTER KHO KHUAT (obsolete)
    (?,T$(B . consonant) ; 0xA4: LETTER KHO KHWAI
    (?,T%(B . consonant) ; 0xA5: LETTER KHO KHON (obsolete)
    (?,T&(B . consonant) ; 0xA6: LETTER KHO RAKHANG
    (?,T'(B . consonant) ; 0xA7: LETTER NGO NGU
    (?,T((B . consonant) ; 0xA8: LETTER CHO CHAN
    (?,T)(B . consonant) ; 0xA9: LETTER CHO CHING
    (?,T*(B . consonant) ; 0xAA: LETTER CHO CHANG
    (?,T+(B . consonant) ; 0xAB: LETTER SO SO
    (?,T,(B . consonant) ; 0xAC: LETTER CHO CHOE
    (?,T-(B . consonant) ; 0xAD: LETTER YO YING
    (?,T.(B . consonant) ; 0xAE: LETTER DO CHADA
    (?,T/(B . consonant) ; 0xAF: LETTER TO PATAK
    (?,T0(B . consonant) ; 0xB0: LETTER THO THAN
    (?,T1(B . consonant) ; 0xB1: LETTER THO NANGMONTHO
    (?,T2(B . consonant) ; 0xB2: LETTER THO PHUTHAO
    (?,T3(B . consonant) ; 0xB3: LETTER NO NEN
    (?,T4(B . consonant) ; 0xB4: LETTER DO DEK
    (?,T5(B . consonant) ; 0xB5: LETTER TO TAO
    (?,T6(B . consonant) ; 0xB6: LETTER THO THUNG
    (?,T7(B . consonant) ; 0xB7: LETTER THO THAHAN
    (?,T8(B . consonant) ; 0xB8: LETTER THO THONG
    (?,T9(B . consonant) ; 0xB9: LETTER NO NU
    (?,T:(B . consonant) ; 0xBA: LETTER BO BAIMAI
    (?,T;(B . consonant) ; 0xBB: LETTER PO PLA
    (?,T<(B . consonant) ; 0xBC: LETTER PHO PHUNG
    (?,T=(B . consonant) ; 0xBD: LETTER FO FA
    (?,T>(B . consonant) ; 0xBE: LETTER PHO PHAN
    (?,T?(B . consonant) ; 0xBF: LETTER FO FAN
    (?,T@(B . consonant) ; 0xC0: LETTER PHO SAMPHAO
    (?,TA(B . consonant) ; 0xC1: LETTER MO MA
    (?,TB(B . consonant) ; 0xC2: LETTER YO YAK
    (?,TC(B . consonant) ; 0xC3: LETTER RO RUA
    (?,TD(B . vowel-base) ; 0xC4: LETTER RU (vowel letter used to write Pali)
    (?,TE(B . consonant) ; 0xC5: LETTER LO LING
    (?,TF(B . vowel-base) ; 0xC6: LETTER LU (vowel letter used to write Pali)
    (?,TG(B . consonant) ; 0xC7: LETTER WO WAEN
    (?,TH(B . consonant) ; 0xC8: LETTER SO SALA
    (?,TI(B . consonant) ; 0xC9: LETTER SO RUSI
    (?,TJ(B . consonant) ; 0xCA: LETTER SO SUA
    (?,TK(B . consonant) ; 0xCB: LETTER HO HIP
    (?,TL(B . consonant) ; 0xCC: LETTER LO CHULA
    (?,TM(B . consonant) ; 0xCD: LETTER O ANG
    (?,TN(B . consonant) ; 0xCE: LETTER HO NOK HUK
    (?,TO(B . special) ; 0xCF: PAI YAN NOI (abbreviation)
    (?,TP(B . vowel-base) ; 0xD0: VOWEL SIGN SARA A
    (?,TQ(B . vowel-upper) ; 0xD1: VOWEL SIGN MAI HAN-AKAT N/S-T
    (?,TR(B . vowel-base) ; 0xD2: VOWEL SIGN SARA AA
    (?,TS(B . vowel-base) ; 0xD3: VOWEL SIGN SARA AM
    (?,TT(B . vowel-upper) ; 0xD4: VOWEL SIGN SARA I N/S-T
    (?,TU(B . vowel-upper) ; 0xD5: VOWEL SIGN SARA II N/S-T
    (?,TV(B . vowel-upper) ; 0xD6: VOWEL SIGN SARA UE N/S-T
    (?,TW(B . vowel-upper) ; 0xD7: VOWEL SIGN SARA UEE N/S-T
    (?,TX(B . vowel-lower) ; 0xD8: VOWEL SIGN SARA U N/S-B
    (?,TY(B . vowel-lower) ; 0xD9: VOWEL SIGN SARA UU N/S-B
    (?,TZ(B . vowel-lower) ; 0xDA: VOWEL SIGN PHINTHU N/S-B (Pali virama)
    (?,T[(B . not-used) ; 0xDA:
    (?,T\(B . not-used) ; 0xDC:
    (?,T](B . not-used) ; 0xDC:
    (?,T^(B . not-used) ; 0xDC:
    (?,T_(B . special) ; 0xDF: BAHT SIGN (currency symbol)
    (?,T`(B . vowel-base) ; 0xE0: VOWEL SIGN SARA E
    (?,Ta(B . vowel-base) ; 0xE1: VOWEL SIGN SARA AE
    (?,Tb(B . vowel-base) ; 0xE2: VOWEL SIGN SARA O
    (?,Tc(B . vowel-base) ; 0xE3: VOWEL SIGN SARA MAI MUAN
    (?,Td(B . vowel-base) ; 0xE4: VOWEL SIGN SARA MAI MALAI
    (?,Te(B . vowel-base) ; 0xE5: LAK KHANG YAO
    (?,Tf(B . special) ; 0xE6: MAI YAMOK (repetion)
    (?,Tg(B . vowel-upper) ; 0xE7: VOWEL SIGN MAI TAI KHU N/S-T
    (?,Th(B . tone) ; 0xE8: TONE MAI EK N/S-T
    (?,Ti(B . tone) ; 0xE9: TONE MAI THO N/S-T
    (?,Tj(B . tone) ; 0xEA: TONE MAI TRI N/S-T
    (?,Tk(B . tone) ; 0xEB: TONE MAI CHATTAWA N/S-T
    (?,Tl(B . tone) ; 0xEC: THANTHAKHAT N/S-T (cancellation mark)
    (?,Tm(B . tone) ; 0xED: NIKKHAHIT N/S-T (final nasal)
    (?,Tn(B . vowel-upper) ; 0xEE: YAMAKKAN N/S-T
    (?,To(B . special) ; 0xEF: FONRMAN
    (?,Tp(B . special) ; 0xF0: DIGIT ZERO
    (?,Tq(B . special) ; 0xF1: DIGIT ONE
    (?,Tr(B . special) ; 0xF2: DIGIT TWO
    (?,Ts(B . special) ; 0xF3: DIGIT THREE
    (?,Tt(B . special) ; 0xF4: DIGIT FOUR
    (?,Tu(B . special) ; 0xF5: DIGIT FIVE
    (?,Tv(B . special) ; 0xF6: DIGIT SIX
    (?,Tw(B . special) ; 0xF7: DIGIT SEVEN
    (?,Tx(B . special) ; 0xF8: DIGIT EIGHT
    (?,Ty(B . special) ; 0xF9: DIGIT NINE
    (?,Tz(B . special) ; 0xFA: ANGKHANKHU (ellipsis)
    (?,T{(B . special) ; 0xFB: KHOMUT (beginning of religious texts)
    (?,T|(B . not-used) ; 0xFC:
    (?,T}(B . not-used) ; 0xFD:
    (?,T~(B . not-used) ; 0xFE:
    )
  "Association list of thai-character and property.")
(setq thai-character-alist
      (cons (cons (string-to-char "0,TQi1(B") 'vowel-upper-tone) thai-character-alist))

(defconst thai-category-table
  (copy-category-table (standard-category-table))
  "Category table for Thai.")
(define-category-mnemonic ?0 "Thai consonants"
  thai-category-table)
(define-category-mnemonic ?1 "Thai upper/lower vowel or tone mark."
  thai-category-table)
(define-category-mnemonic ?2 "Thai base vowel or special characters."
  thai-category-table)

(let ((chars thai-character-alist)
      ch prop)
  (while chars
    (setq ch (car (car chars))
	  prop (cdr (car chars)))
    (cond ((eq prop 'consonant)
	   (modify-category-entry ch ?0 thai-category-table))
	  ((or (eq prop 'vowel-upper)
	       (eq prop 'vowel-lower)
	       (eq prop 'tone))
	   (modify-category-entry ch ?1 thai-category-table))
	  ((null (eq prop 'vowel-upper-tone))
	   (modify-category-entry ch ?2 thai-category-table)))
    (setq chars (cdr chars))))

;;;###autoload
(defun thai-compose-buffer ()
  "Compose Thai characters in the current buffer."
  (interactive)
  (thai-compose-region (point-min) (point-max)))

;;;###autoload
(defun thai-compose-region (beg end)
  "Compose Thai characters in the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (decompose-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((ctbl (category-table))
	  str)
      (unwind-protect
	  (progn
	    (set-category-table thai-category-table)
	    (while (re-search-forward "\\c0\\c1+" nil t)
	      (compose-region (match-beginning 0) (match-end 0))))
	(set-category-table ctbl)))))

;;;
(provide 'thai)
