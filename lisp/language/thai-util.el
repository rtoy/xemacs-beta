;;; thai-util.el --- utilities for Thai

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1997 MORIOKA Tomohiko

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

;;; Code:

;; Setting information of Thai characters.

;; (let ((l '((?,T!(B consonant "LETTER KO KAI")                               ; 0xA1
;;            (?,T"(B consonant "LETTER KHO KHAI")                             ; 0xA2
;;            (?,T#(B consonant "LETTER KHO KHUAT")                            ; 0xA3
;;            (?,T$(B consonant "LETTER KHO KHWAI")                            ; 0xA4
;;            (?,T%(B consonant "LETTER KHO KHON")                             ; 0xA5
;;            (?,T&(B consonant "LETTER KHO RAKHANG")                          ; 0xA6
;;            (?,T'(B consonant "LETTER NGO NGU")                              ; 0xA7
;;            (?,T((B consonant "LETTER CHO CHAN")                             ; 0xA8
;;            (?,T)(B consonant "LETTER CHO CHING")                            ; 0xA9
;;            (?,T*(B consonant "LETTER CHO CHANG")                            ; 0xAA
;;            (?,T+(B consonant "LETTER SO SO")                                ; 0xAB
;;            (?,T,(B consonant "LETTER CHO CHOE")                             ; 0xAC
;;            (?,T-(B consonant "LETTER YO YING")                              ; 0xAD
;;            (?,T.(B consonant "LETTER DO CHADA")                             ; 0xAE
;;            (?,T/(B consonant "LETTER TO PATAK")                             ; 0xAF
;;            (?,T0(B consonant "LETTER THO THAN")                             ; 0xB0
;;            (?,T1(B consonant "LETTER THO NANGMONTHO")                       ; 0xB1
;;            (?,T2(B consonant "LETTER THO PHUTHAO")                          ; 0xB2
;;            (?,T3(B consonant "LETTER NO NEN")                               ; 0xB3
;;            (?,T4(B consonant "LETTER DO DEK")                               ; 0xB4
;;            (?,T5(B consonant "LETTER TO TAO")                               ; 0xB5
;;            (?,T6(B consonant "LETTER THO THUNG")                            ; 0xB6
;;            (?,T7(B consonant "LETTER THO THAHAN")                           ; 0xB7
;;            (?,T8(B consonant "LETTER THO THONG")                            ; 0xB8
;;            (?,T9(B consonant "LETTER NO NU")                                ; 0xB9
;;            (?,T:(B consonant "LETTER BO BAIMAI")                            ; 0xBA
;;            (?,T;(B consonant "LETTER PO PLA")                               ; 0xBB
;;            (?,T<(B consonant "LETTER PHO PHUNG")                            ; 0xBC
;;            (?,T=(B consonant "LETTER FO FA")                                ; 0xBD
;;            (?,T>(B consonant "LETTER PHO PHAN")                             ; 0xBE
;;            (?,T?(B consonant "LETTER FO FAN")                               ; 0xBF
;;            (?,T@(B consonant "LETTER PHO SAMPHAO")                          ; 0xC0
;;            (?,TA(B consonant "LETTER MO MA")                                ; 0xC1
;;            (?,TB(B consonant "LETTER YO YAK")                               ; 0xC2
;;            (?,TC(B consonant "LETTER RO RUA")                               ; 0xC3
;;            (?,TD(B vowel-base "LETTER RU (Pali vowel letter)")              ; 0xC4
;;            (?,TE(B consonant "LETTER LO LING")                              ; 0xC5
;;            (?,TF(B vowel-base "LETTER LU (Pali vowel letter)")              ; 0xC6
;;            (?,TG(B consonant "LETTER WO WAEN")                              ; 0xC7
;;            (?,TH(B consonant "LETTER SO SALA")                              ; 0xC8
;;            (?,TI(B consonant "LETTER SO RUSI")                              ; 0xC9
;;            (?,TJ(B consonant "LETTER SO SUA")                               ; 0xCA
;;            (?,TK(B consonant "LETTER HO HIP")                               ; 0xCB
;;            (?,TL(B consonant "LETTER LO CHULA")                             ; 0xCC
;;            (?,TM(B consonant "LETTER O ANG")                                ; 0xCD
;;            (?,TN(B consonant "LETTER HO NOK HUK")                           ; 0xCE
;;            (?,TO(B special "PAI YAN NOI (abbreviation)")                    ; 0xCF
;;            (?,TP(B vowel-base "VOWEL SIGN SARA A")                          ; 0xD0
;;            (?,TQ(B vowel-upper "VOWEL SIGN MAI HAN-AKAT N/S-T")             ; 0xD1
;;            (?,TR(B vowel-base "VOWEL SIGN SARA AA")                         ; 0xD2
;;            (?,TS(B vowel-base "VOWEL SIGN SARA AM")                         ; 0xD3
;;            (?,TT(B vowel-upper "VOWEL SIGN SARA I N/S-T")                   ; 0xD4
;;            (?,TU(B vowel-upper "VOWEL SIGN SARA II N/S-T")                  ; 0xD5
;;            (?,TV(B vowel-upper "VOWEL SIGN SARA UE N/S-T")                  ; 0xD6
;;            (?,TW(B vowel-upper "VOWEL SIGN SARA UEE N/S-T")                 ; 0xD7
;;            (?,TX(B vowel-lower "VOWEL SIGN SARA U N/S-B")                   ; 0xD8
;;            (?,TY(B vowel-lower "VOWEL SIGN SARA UU N/S-B")                  ; 0xD9
;;            (?,TZ(B vowel-lower "VOWEL SIGN PHINTHU N/S-B (Pali virama)")    ; 0xDA
;;            (?,T[(B invalid nil)                                             ; 0xDA
;;            (?,T\(B invalid nil)                                             ; 0xDC
;;            (?,T](B invalid nil)                                             ; 0xDC
;;            (?,T^(B invalid nil)                                             ; 0xDC
;;            (?,T_(B special "BAHT SIGN (currency symbol)")                   ; 0xDF
;;            (?,T`(B vowel-base "VOWEL SIGN SARA E")                          ; 0xE0
;;            (?,Ta(B vowel-base "VOWEL SIGN SARA AE")                         ; 0xE1
;;            (?,Tb(B vowel-base "VOWEL SIGN SARA O")                          ; 0xE2
;;            (?,Tc(B vowel-base "VOWEL SIGN SARA MAI MUAN")                   ; 0xE3
;;            (?,Td(B vowel-base "VOWEL SIGN SARA MAI MALAI")                  ; 0xE4
;;            (?,Te(B vowel-base "LAK KHANG YAO")                              ; 0xE5
;;            (?,Tf(B special "MAI YAMOK (repetion)")                          ; 0xE6
;;            (?,Tg(B vowel-upper "VOWEL SIGN MAI TAI KHU N/S-T")              ; 0xE7
;;            (?,Th(B tone "TONE MAI EK N/S-T")                                ; 0xE8
;;            (?,Ti(B tone "TONE MAI THO N/S-T")                               ; 0xE9
;;            (?,Tj(B tone "TONE MAI TRI N/S-T")                               ; 0xEA
;;            (?,Tk(B tone "TONE MAI CHATTAWA N/S-T")                          ; 0xEB
;;            (?,Tl(B tone "THANTHAKHAT N/S-T (cancellation mark)")            ; 0xEC
;;            (?,Tm(B tone "NIKKHAHIT N/S-T (final nasal)")                    ; 0xED
;;            (?,Tn(B vowel-upper "YAMAKKAN N/S-T")                            ; 0xEE
;;            (?,To(B special "FONRMAN")                                       ; 0xEF
;;            (?,Tp(B special "DIGIT ZERO")                                    ; 0xF0
;;            (?,Tq(B special "DIGIT ONE")                                     ; 0xF1
;;            (?,Tr(B special "DIGIT TWO")                                     ; 0xF2
;;            (?,Ts(B special "DIGIT THREE")                                   ; 0xF3
;;            (?,Tt(B special "DIGIT FOUR")                                    ; 0xF4
;;            (?,Tu(B special "DIGIT FIVE")                                    ; 0xF5
;;            (?,Tv(B special "DIGIT SIX")                                     ; 0xF6
;;            (?,Tw(B special "DIGIT SEVEN")                                   ; 0xF7
;;            (?,Tx(B special "DIGIT EIGHT")                                   ; 0xF8
;;            (?,Ty(B special "DIGIT NINE")                                    ; 0xF9
;;            (?,Tz(B special "ANGKHANKHU (ellipsis)")                         ; 0xFA
;;            (?,T{(B special "KHOMUT (beginning of religious texts)")         ; 0xFB
;;            (?,T|(B invalid nil)                                             ; 0xFC
;;            (?,T}(B invalid nil)                                             ; 0xFD
;;            (?,T~(B invalid nil)                                             ; 0xFE
;;            ))
;;       elm)
;;   (while l
;;     (setq elm (car l))
;;     (put-char-code-property (car elm) 'phonetic-type (car (cdr elm)))
;;     (put-char-code-property (car elm) 'name (nth 2 elm))
;;     (setq l (cdr l))))

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
      (cons (cons (string-to-char "0,TQi1(B") 'vowel-upper-tone)
	    thai-character-alist))

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

;; ;;;###autoload
;; (defun thai-compose-region (beg end)
;;   "Compose Thai characters in the region.
;; When called from a program, expects two arguments,
;; positions (integers or markers) specifying the region."
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region beg end)
;;     (decompose-region (point-min) (point-max))
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\c0\\(\\c2\\|\\c3\\|\\c4\\)+" nil t)
;;       (if (aref (char-category-set (char-after (match-beginning 0))) ?t)
;;           (compose-region (match-beginning 0) (match-end 0))))))

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

;;;###autoload
(defun thai-compose-buffer ()
  "Compose Thai characters in the current buffer."
  (interactive)
  (thai-compose-region (point-min) (point-max)))

;; ;;;###autoload
;; (defun thai-post-read-conversion (len)
;;   (save-excursion
;;     (save-restriction
;;       (let ((buffer-modified-p (buffer-modified-p)))
;;         (narrow-to-region (point) (+ (point) len))
;;         (thai-compose-region (point-min) (point-max))
;;         (set-buffer-modified-p buffer-modified-p)
;;         (- (point-max) (point-min))))))

;; ;;;###autoload
;; (defun thai-pre-write-conversion (from to)
;;   (let ((old-buf (current-buffer))
;;         (work-buf (get-buffer-create " *thai-work*")))
;;     (set-buffer work-buf)
;;     (erase-buffer)
;;     (if (stringp from)
;;         (insert from)
;;       (insert-buffer-substring old-buf from to))
;;     (decompose-region (point-min) (point-max))
;;     ;; Should return nil as annotations.
;;     nil))

;;
(provide 'language/thai-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; thai-util.el ends here
