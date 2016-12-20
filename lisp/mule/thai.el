;;; thai.el --- support for Thai -*- coding: utf-8; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Thai

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Emacs 21.1 (language/thai.el).

;;; Commentary:

(make-coding-system
 'tis-620 'fixed-width
 "TIS620 (Thai)"
 '(mnemonic "TIS620"
   unicode-map
   ((#x80 ?\u0080) ;; <control>
    (#x81 ?\u0081) ;; <control>
    (#x82 ?\u0082) ;; <control>
    (#x83 ?\u0083) ;; <control>
    (#x84 ?\u0084) ;; <control>
    (#x85 ?\u0085) ;; <control>
    (#x86 ?\u0086) ;; <control>
    (#x87 ?\u0087) ;; <control>
    (#x88 ?\u0088) ;; <control>
    (#x89 ?\u0089) ;; <control>
    (#x8A ?\u008A) ;; <control>
    (#x8B ?\u008B) ;; <control>
    (#x8C ?\u008C) ;; <control>
    (#x8D ?\u008D) ;; <control>
    (#x8E ?\u008E) ;; <control>
    (#x8F ?\u008F) ;; <control>
    (#x90 ?\u0090) ;; <control>
    (#x91 ?\u0091) ;; <control>
    (#x92 ?\u0092) ;; <control>
    (#x93 ?\u0093) ;; <control>
    (#x94 ?\u0094) ;; <control>
    (#x95 ?\u0095) ;; <control>
    (#x96 ?\u0096) ;; <control>
    (#x97 ?\u0097) ;; <control>
    (#x98 ?\u0098) ;; <control>
    (#x99 ?\u0099) ;; <control>
    (#x9A ?\u009A) ;; <control>
    (#x9B ?\u009B) ;; <control>
    (#x9C ?\u009C) ;; <control>
    (#x9D ?\u009D) ;; <control>
    (#x9E ?\u009E) ;; <control>
    (#x9F ?\u009F) ;; <control>
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u0E01) ;; THAI CHARACTER KO KAI
    (#xA2 ?\u0E02) ;; THAI CHARACTER KHO KHAI
    (#xA3 ?\u0E03) ;; THAI CHARACTER KHO KHUAT
    (#xA4 ?\u0E04) ;; THAI CHARACTER KHO KHWAI
    (#xA5 ?\u0E05) ;; THAI CHARACTER KHO KHON
    (#xA6 ?\u0E06) ;; THAI CHARACTER KHO RAKHANG
    (#xA7 ?\u0E07) ;; THAI CHARACTER NGO NGU
    (#xA8 ?\u0E08) ;; THAI CHARACTER CHO CHAN
    (#xA9 ?\u0E09) ;; THAI CHARACTER CHO CHING
    (#xAA ?\u0E0A) ;; THAI CHARACTER CHO CHANG
    (#xAB ?\u0E0B) ;; THAI CHARACTER SO SO
    (#xAC ?\u0E0C) ;; THAI CHARACTER CHO CHOE
    (#xAD ?\u0E0D) ;; THAI CHARACTER YO YING
    (#xAE ?\u0E0E) ;; THAI CHARACTER DO CHADA
    (#xAF ?\u0E0F) ;; THAI CHARACTER TO PATAK
    (#xB0 ?\u0E10) ;; THAI CHARACTER THO THAN
    (#xB1 ?\u0E11) ;; THAI CHARACTER THO NANGMONTHO
    (#xB2 ?\u0E12) ;; THAI CHARACTER THO PHUTHAO
    (#xB3 ?\u0E13) ;; THAI CHARACTER NO NEN
    (#xB4 ?\u0E14) ;; THAI CHARACTER DO DEK
    (#xB5 ?\u0E15) ;; THAI CHARACTER TO TAO
    (#xB6 ?\u0E16) ;; THAI CHARACTER THO THUNG
    (#xB7 ?\u0E17) ;; THAI CHARACTER THO THAHAN
    (#xB8 ?\u0E18) ;; THAI CHARACTER THO THONG
    (#xB9 ?\u0E19) ;; THAI CHARACTER NO NU
    (#xBA ?\u0E1A) ;; THAI CHARACTER BO BAIMAI
    (#xBB ?\u0E1B) ;; THAI CHARACTER PO PLA
    (#xBC ?\u0E1C) ;; THAI CHARACTER PHO PHUNG
    (#xBD ?\u0E1D) ;; THAI CHARACTER FO FA
    (#xBE ?\u0E1E) ;; THAI CHARACTER PHO PHAN
    (#xBF ?\u0E1F) ;; THAI CHARACTER FO FAN
    (#xC0 ?\u0E20) ;; THAI CHARACTER PHO SAMPHAO
    (#xC1 ?\u0E21) ;; THAI CHARACTER MO MA
    (#xC2 ?\u0E22) ;; THAI CHARACTER YO YAK
    (#xC3 ?\u0E23) ;; THAI CHARACTER RO RUA
    (#xC4 ?\u0E24) ;; THAI CHARACTER RU
    (#xC5 ?\u0E25) ;; THAI CHARACTER LO LING
    (#xC6 ?\u0E26) ;; THAI CHARACTER LU
    (#xC7 ?\u0E27) ;; THAI CHARACTER WO WAEN
    (#xC8 ?\u0E28) ;; THAI CHARACTER SO SALA
    (#xC9 ?\u0E29) ;; THAI CHARACTER SO RUSI
    (#xCA ?\u0E2A) ;; THAI CHARACTER SO SUA
    (#xCB ?\u0E2B) ;; THAI CHARACTER HO HIP
    (#xCC ?\u0E2C) ;; THAI CHARACTER LO CHULA
    (#xCD ?\u0E2D) ;; THAI CHARACTER O ANG
    (#xCE ?\u0E2E) ;; THAI CHARACTER HO NOKHUK
    (#xCF ?\u0E2F) ;; THAI CHARACTER PAIYANNOI
    (#xD0 ?\u0E30) ;; THAI CHARACTER SARA A
    (#xD1 ?\u0E31) ;; THAI CHARACTER MAI HAN-AKAT
    (#xD2 ?\u0E32) ;; THAI CHARACTER SARA AA
    (#xD3 ?\u0E33) ;; THAI CHARACTER SARA AM
    (#xD4 ?\u0E34) ;; THAI CHARACTER SARA I
    (#xD5 ?\u0E35) ;; THAI CHARACTER SARA II
    (#xD6 ?\u0E36) ;; THAI CHARACTER SARA UE
    (#xD7 ?\u0E37) ;; THAI CHARACTER SARA UEE
    (#xD8 ?\u0E38) ;; THAI CHARACTER SARA U
    (#xD9 ?\u0E39) ;; THAI CHARACTER SARA UU
    (#xDA ?\u0E3A) ;; THAI CHARACTER PHINTHU
    (#xDF ?\u0E3F) ;; THAI CURRENCY SYMBOL BAHT
    (#xE0 ?\u0E40) ;; THAI CHARACTER SARA E
    (#xE1 ?\u0E41) ;; THAI CHARACTER SARA AE
    (#xE2 ?\u0E42) ;; THAI CHARACTER SARA O
    (#xE3 ?\u0E43) ;; THAI CHARACTER SARA AI MAIMUAN
    (#xE4 ?\u0E44) ;; THAI CHARACTER SARA AI MAIMALAI
    (#xE5 ?\u0E45) ;; THAI CHARACTER LAKKHANGYAO
    (#xE6 ?\u0E46) ;; THAI CHARACTER MAIYAMOK
    (#xE7 ?\u0E47) ;; THAI CHARACTER MAITAIKHU
    (#xE8 ?\u0E48) ;; THAI CHARACTER MAI EK
    (#xE9 ?\u0E49) ;; THAI CHARACTER MAI THO
    (#xEA ?\u0E4A) ;; THAI CHARACTER MAI TRI
    (#xEB ?\u0E4B) ;; THAI CHARACTER MAI CHATTAWA
    (#xEC ?\u0E4C) ;; THAI CHARACTER THANTHAKHAT
    (#xED ?\u0E4D) ;; THAI CHARACTER NIKHAHIT
    (#xEE ?\u0E4E) ;; THAI CHARACTER YAMAKKAN
    (#xEF ?\u0E4F) ;; THAI CHARACTER FONGMAN
    (#xF0 ?\u0E50) ;; THAI DIGIT ZERO
    (#xF1 ?\u0E51) ;; THAI DIGIT ONE
    (#xF2 ?\u0E52) ;; THAI DIGIT TWO
    (#xF3 ?\u0E53) ;; THAI DIGIT THREE
    (#xF4 ?\u0E54) ;; THAI DIGIT FOUR
    (#xF5 ?\u0E55) ;; THAI DIGIT FIVE
    (#xF6 ?\u0E56) ;; THAI DIGIT SIX
    (#xF7 ?\u0E57) ;; THAI DIGIT SEVEN
    (#xF8 ?\u0E58) ;; THAI DIGIT EIGHT
    (#xF9 ?\u0E59) ;; THAI DIGIT NINE
    (#xFA ?\u0E5A) ;; THAI CHARACTER ANGKHANKHU
    (#xFB ?\u0E5B));; THAI CHARACTER KHOMUT
   documentation "Non-composed Thai"
   aliases (iso-8859-11)))

(make-coding-system
 'windows-874 'fixed-width "Microsoft's CP874"
 '(mnemonic "CP874"
   unicode-map
   ((#x80 ?\u20AC) ;; EURO SIGN
    (#x85 ?\u2026) ;; HORIZONTAL ELLIPSIS
    (#x91 ?\u2018) ;; LEFT SINGLE QUOTATION MARK
    (#x92 ?\u2019) ;; RIGHT SINGLE QUOTATION MARK
    (#x93 ?\u201C) ;; LEFT DOUBLE QUOTATION MARK
    (#x94 ?\u201D) ;; RIGHT DOUBLE QUOTATION MARK
    (#x95 ?\u2022) ;; BULLET
    (#x96 ?\u2013) ;; EN DASH
    (#x97 ?\u2014) ;; EM DASH
    (#xA0 ?\u00A0) ;; NO-BREAK SPACE
    (#xA1 ?\u0E01) ;; THAI CHARACTER KO KAI
    (#xA2 ?\u0E02) ;; THAI CHARACTER KHO KHAI
    (#xA3 ?\u0E03) ;; THAI CHARACTER KHO KHUAT
    (#xA4 ?\u0E04) ;; THAI CHARACTER KHO KHWAI
    (#xA5 ?\u0E05) ;; THAI CHARACTER KHO KHON
    (#xA6 ?\u0E06) ;; THAI CHARACTER KHO RAKHANG
    (#xA7 ?\u0E07) ;; THAI CHARACTER NGO NGU
    (#xA8 ?\u0E08) ;; THAI CHARACTER CHO CHAN
    (#xA9 ?\u0E09) ;; THAI CHARACTER CHO CHING
    (#xAA ?\u0E0A) ;; THAI CHARACTER CHO CHANG
    (#xAB ?\u0E0B) ;; THAI CHARACTER SO SO
    (#xAC ?\u0E0C) ;; THAI CHARACTER CHO CHOE
    (#xAD ?\u0E0D) ;; THAI CHARACTER YO YING
    (#xAE ?\u0E0E) ;; THAI CHARACTER DO CHADA
    (#xAF ?\u0E0F) ;; THAI CHARACTER TO PATAK
    (#xB0 ?\u0E10) ;; THAI CHARACTER THO THAN
    (#xB1 ?\u0E11) ;; THAI CHARACTER THO NANGMONTHO
    (#xB2 ?\u0E12) ;; THAI CHARACTER THO PHUTHAO
    (#xB3 ?\u0E13) ;; THAI CHARACTER NO NEN
    (#xB4 ?\u0E14) ;; THAI CHARACTER DO DEK
    (#xB5 ?\u0E15) ;; THAI CHARACTER TO TAO
    (#xB6 ?\u0E16) ;; THAI CHARACTER THO THUNG
    (#xB7 ?\u0E17) ;; THAI CHARACTER THO THAHAN
    (#xB8 ?\u0E18) ;; THAI CHARACTER THO THONG
    (#xB9 ?\u0E19) ;; THAI CHARACTER NO NU
    (#xBA ?\u0E1A) ;; THAI CHARACTER BO BAIMAI
    (#xBB ?\u0E1B) ;; THAI CHARACTER PO PLA
    (#xBC ?\u0E1C) ;; THAI CHARACTER PHO PHUNG
    (#xBD ?\u0E1D) ;; THAI CHARACTER FO FA
    (#xBE ?\u0E1E) ;; THAI CHARACTER PHO PHAN
    (#xBF ?\u0E1F) ;; THAI CHARACTER FO FAN
    (#xC0 ?\u0E20) ;; THAI CHARACTER PHO SAMPHAO
    (#xC1 ?\u0E21) ;; THAI CHARACTER MO MA
    (#xC2 ?\u0E22) ;; THAI CHARACTER YO YAK
    (#xC3 ?\u0E23) ;; THAI CHARACTER RO RUA
    (#xC4 ?\u0E24) ;; THAI CHARACTER RU
    (#xC5 ?\u0E25) ;; THAI CHARACTER LO LING
    (#xC6 ?\u0E26) ;; THAI CHARACTER LU
    (#xC7 ?\u0E27) ;; THAI CHARACTER WO WAEN
    (#xC8 ?\u0E28) ;; THAI CHARACTER SO SALA
    (#xC9 ?\u0E29) ;; THAI CHARACTER SO RUSI
    (#xCA ?\u0E2A) ;; THAI CHARACTER SO SUA
    (#xCB ?\u0E2B) ;; THAI CHARACTER HO HIP
    (#xCC ?\u0E2C) ;; THAI CHARACTER LO CHULA
    (#xCD ?\u0E2D) ;; THAI CHARACTER O ANG
    (#xCE ?\u0E2E) ;; THAI CHARACTER HO NOKHUK
    (#xCF ?\u0E2F) ;; THAI CHARACTER PAIYANNOI
    (#xD0 ?\u0E30) ;; THAI CHARACTER SARA A
    (#xD1 ?\u0E31) ;; THAI CHARACTER MAI HAN-AKAT
    (#xD2 ?\u0E32) ;; THAI CHARACTER SARA AA
    (#xD3 ?\u0E33) ;; THAI CHARACTER SARA AM
    (#xD4 ?\u0E34) ;; THAI CHARACTER SARA I
    (#xD5 ?\u0E35) ;; THAI CHARACTER SARA II
    (#xD6 ?\u0E36) ;; THAI CHARACTER SARA UE
    (#xD7 ?\u0E37) ;; THAI CHARACTER SARA UEE
    (#xD8 ?\u0E38) ;; THAI CHARACTER SARA U
    (#xD9 ?\u0E39) ;; THAI CHARACTER SARA UU
    (#xDA ?\u0E3A) ;; THAI CHARACTER PHINTHU
    (#xDF ?\u0E3F) ;; THAI CURRENCY SYMBOL BAHT
    (#xE0 ?\u0E40) ;; THAI CHARACTER SARA E
    (#xE1 ?\u0E41) ;; THAI CHARACTER SARA AE
    (#xE2 ?\u0E42) ;; THAI CHARACTER SARA O
    (#xE3 ?\u0E43) ;; THAI CHARACTER SARA AI MAIMUAN
    (#xE4 ?\u0E44) ;; THAI CHARACTER SARA AI MAIMALAI
    (#xE5 ?\u0E45) ;; THAI CHARACTER LAKKHANGYAO
    (#xE6 ?\u0E46) ;; THAI CHARACTER MAIYAMOK
    (#xE7 ?\u0E47) ;; THAI CHARACTER MAITAIKHU
    (#xE8 ?\u0E48) ;; THAI CHARACTER MAI EK
    (#xE9 ?\u0E49) ;; THAI CHARACTER MAI THO
    (#xEA ?\u0E4A) ;; THAI CHARACTER MAI TRI
    (#xEB ?\u0E4B) ;; THAI CHARACTER MAI CHATTAWA
    (#xEC ?\u0E4C) ;; THAI CHARACTER THANTHAKHAT
    (#xED ?\u0E4D) ;; THAI CHARACTER NIKHAHIT
    (#xEE ?\u0E4E) ;; THAI CHARACTER YAMAKKAN
    (#xEF ?\u0E4F) ;; THAI CHARACTER FONGMAN
    (#xF0 ?\u0E50) ;; THAI DIGIT ZERO
    (#xF1 ?\u0E51) ;; THAI DIGIT ONE
    (#xF2 ?\u0E52) ;; THAI DIGIT TWO
    (#xF3 ?\u0E53) ;; THAI DIGIT THREE
    (#xF4 ?\u0E54) ;; THAI DIGIT FOUR
    (#xF5 ?\u0E55) ;; THAI DIGIT FIVE
    (#xF6 ?\u0E56) ;; THAI DIGIT SIX
    (#xF7 ?\u0E57) ;; THAI DIGIT SEVEN
    (#xF8 ?\u0E58) ;; THAI DIGIT EIGHT
    (#xF9 ?\u0E59) ;; THAI DIGIT NINE
    (#xFA ?\u0E5A) ;; THAI CHARACTER ANGKHANKHU
    (#xFB ?\u0E5B));; THAI CHARACTER KHOMUT
   documentation "Microsoft's encoding for Thai."
   aliases (cp874)))

(set-language-info-alist
 "Thai"
 '((coding-system tis-620 utf-8)
   (tutorial . "TUTORIAL.th")
   (tutorial-coding-system . tis-620)
   (coding-priority tis-620 utf-8 iso-2022-7bit)
   (sample-text . "สวัสดีครับ, สวัสดีค่ะ")
   (documentation . t)))

(provide 'thai)

;;; thai.el ends here
