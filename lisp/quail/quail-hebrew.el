;; Quail packages for inputting Greek characters.
;; Copyright (C) 1992 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of GNU Emacs).

;; Mule is free software distributed in the form of patches to GNU Emacs.
;; You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; Mule is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; 92.8.7   created for Mule Ver.0.9.5 by T.Matsuzawa <mzw_t@yhp.hp.com>
;;; 93.1.7   modified for Mule ver.0.9.7 by Takahashi N. <ntakahas@etl.go.jp>
;;; 93.4.10  modified for Mule Ver.0.9.7.1 by K.Handa <handa@etl.go.jp>
;;;	lc-ascr2l is used for Hebrew punctuation characters.
;;; 93.6.11  modified for Mule Ver.0.9.8 by K.Handa <handa@etl.go.jp>
;;;	Modfied for visually mode.

(require 'quail)

(quail-define-package "hebrew" "HEBREW" nil "Hebrew (ISO 8859-8) encoding.

Based on Hebrew typewriter keys.
Hebrew letters are assigned to lowercases.
" nil t t t t nil nil '(visual-mode))

;;  1[2]![0] 2[2]@[0] 3[2]#[0] 4[2]$[0] 5[2]%[0] 6[2]^[0] 7[2]&[0] 8[2]*[0] 9[2]([0] 0[2])[0] [2]_-[0] [2]+=[0] [2]~;[0]
;;   [2]/[0]Q [2]'[0]W [2],Hw[0](BE [2],Hx[0](BR [2],H`[0](BT [2],Hh[0](BY [2],He[0](BU [2],Ho[0](BI [2],Hm[0](BO [2],Ht[0](BP [2]{[[0] [2]{][0]
;;    [2],Hy[0](BA [2],Hc[0](BS [2],Hb[0](BD [2],Hk[0](BF [2],Hr[0](BG [2],Hi[0](BH [2],Hg[0](BJ [2],Hl[0](BK [2],Hj[0](BL [2]:,Hs[0](B [2]",[0] [2]|\[0]
;;     [2],Hf[0](BZ [2],Hq[0](BX [2],Ha[0](BC [2],Hd[0](BV [2],Hp[0](BB [2],Hn[0](BN [2],Hv[0](BM [2]<,Hz[0](B [2]>,Hu[0](B [2]?.[0]
;;		[2]         [0]

(quail-defrule "1" ?1)
(quail-defrule "2" ?2)
(quail-defrule "3" ?3)
(quail-defrule "4" ?4)
(quail-defrule "5" ?5)
(quail-defrule "6" ?6)
(quail-defrule "7" ?7)
(quail-defrule "8" ?8)
(quail-defrule "9" ?9)
(quail-defrule "0" ?0)
(quail-defrule "-" (make-character lc-ascr2l ?-))
(quail-defrule "=" (make-character lc-ascr2l ?=))
(quail-defrule "`" (make-character lc-ascr2l ?;))
(quail-defrule "q" (make-character lc-ascr2l ?/))
(quail-defrule "w" (make-character lc-ascr2l ?'))
(quail-defrule "e" ?[2],Hw[0](B)
(quail-defrule "r" ?[2],Hx[0](B)
(quail-defrule "t" ?[2],H`[0](B)
(quail-defrule "y" ?[2],Hh[0](B)
(quail-defrule "u" ?[2],He[0](B)
(quail-defrule "i" ?[2],Ho[0](B)
(quail-defrule "o" ?[2],Hm[0](B)
(quail-defrule "p" ?[2],Ht[0](B)
(quail-defrule "[" (make-character lc-ascr2l ?\[))
(quail-defrule "]" (make-character lc-ascr2l ?\]))
(quail-defrule "a" ?[2],Hy[0](B)
(quail-defrule "s" ?[2],Hc[0](B)
(quail-defrule "d" ?[2],Hb[0](B)
(quail-defrule "f" ?[2],Hk[0](B)
(quail-defrule "g" ?[2],Hr[0](B)
(quail-defrule "h" ?[2],Hi[0](B)
(quail-defrule "j" ?[2],Hg[0](B)
(quail-defrule "k" ?[2],Hl[0](B)
(quail-defrule "l" ?[2],Hj[0](B)
(quail-defrule ";" ?[2],Hs[0](B)
(quail-defrule "'" (make-character lc-ascr2l ?,))
(quail-defrule "\\" (make-character lc-ascr2l ?\\))
(quail-defrule "z" ?[2],Hf[0](B)
(quail-defrule "x" ?[2],Hq[0](B)
(quail-defrule "c" ?[2],Ha[0](B)
(quail-defrule "v" ?[2],Hd[0](B)
(quail-defrule "b" ?[2],Hp[0](B)
(quail-defrule "n" ?[2],Hn[0](B)
(quail-defrule "m" ?[2],Hv[0](B)
(quail-defrule "," ?[2],Hz[0](B)
(quail-defrule "." ?[2],Hu[0](B)
(quail-defrule "/" (make-character lc-ascr2l ?.))

(quail-defrule "!" (make-character lc-ascr2l ?!))
(quail-defrule "@" (make-character lc-ascr2l ?@))
(quail-defrule "#" (make-character lc-ascr2l ?#))
(quail-defrule "$" (make-character lc-ascr2l ?$))
(quail-defrule "%" (make-character lc-ascr2l ?%))
(quail-defrule "^" (make-character lc-ascr2l ?^))
(quail-defrule "&" (make-character lc-ascr2l ?&))
(quail-defrule "*" (make-character lc-ascr2l ?*))
(quail-defrule "(" (make-character lc-ascr2l ?())
(quail-defrule ")" (make-character lc-ascr2l ?)))
(quail-defrule "_" (make-character lc-ascr2l ?_))
(quail-defrule "+" (make-character lc-ascr2l ?+))
(quail-defrule "~" (make-character lc-ascr2l ?~))
(quail-defrule "Q" ?Q)
(quail-defrule "W" ?W)
(quail-defrule "E" ?E)
(quail-defrule "R" ?R)
(quail-defrule "T" ?T)
(quail-defrule "Y" ?Y)
(quail-defrule "U" ?U)
(quail-defrule "I" ?I)
(quail-defrule "O" ?O)
(quail-defrule "P" ?P)
(quail-defrule "{" (make-character lc-ascr2l ?{))
(quail-defrule "}" (make-character lc-ascr2l ?}))
(quail-defrule "A" ?A)
(quail-defrule "S" ?S)
(quail-defrule "D" ?D)
(quail-defrule "F" ?F)
(quail-defrule "G" ?G)
(quail-defrule "H" ?H)
(quail-defrule "J" ?J)
(quail-defrule "K" ?K)
(quail-defrule "L" ?L)
(quail-defrule ":" (make-character lc-ascr2l ?:))
(quail-defrule "\"" (make-character lc-ascr2l ?\"))
(quail-defrule "|" (make-character lc-ascr2l ?|))
(quail-defrule "Z" ?Z)
(quail-defrule "X" ?X)
(quail-defrule "C" ?C)
(quail-defrule "V" ?V)
(quail-defrule "B" ?B)
(quail-defrule "N" ?N)
(quail-defrule "M" ?M)
(quail-defrule "<" (make-character lc-ascr2l ?<))
(quail-defrule ">" (make-character lc-ascr2l ?>))
(quail-defrule "?" (make-character lc-ascr2l ??))
(quail-defrule " " (make-character lc-ascr2l ? ))

(quail-setup-current-package)
