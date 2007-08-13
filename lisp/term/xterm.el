;;; xterm.el --- define function key sequences for xterm

;; Author: FSF
;; Keywords: terminals

;; Copyright (C) 1995 Free Software Foundation, Inc.
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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; Code:

;; Termcap or terminfo should set these next four?
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])

(define-key function-key-map "\eO\000" [home])
(define-key function-key-map "\eOe" [end])
(define-key function-key-map "\e[1~" [find])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[3~" [delete])
(define-key function-key-map "\e[4~" [select])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key function-key-map "\e[11~" [f1])
(define-key function-key-map "\e[12~" [f2])
(define-key function-key-map "\e[13~" [f3])
(define-key function-key-map "\e[14~" [f4])
(define-key function-key-map "\e[15~" [f5])
(define-key function-key-map "\e[17~" [f6])
(define-key function-key-map "\e[18~" [f7])
(define-key function-key-map "\e[19~" [f8])
(define-key function-key-map "\e[20~" [f9])
(define-key function-key-map "\e[21~" [f10])
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[25~" [f13])
(define-key function-key-map "\e[26~" [f14])
(define-key function-key-map "\e[28~" [help])
(define-key function-key-map "\e[29~" [menu])
; FSF's xterm.el defines this like this:
; (I don't know which one is more correct)
;(define-key function-key-map "\e[29~" [print])
(define-key function-key-map "\e[31~" [f17])
(define-key function-key-map "\e[32~" [f18])
(define-key function-key-map "\e[33~" [f19])
(define-key function-key-map "\e[34~" [f20])

;; Termcap or terminfo should set these next four.
(define-key function-key-map "\eOA" [up])
(define-key function-key-map "\eOB" [down])
(define-key function-key-map "\eOC" [right])
(define-key function-key-map "\eOD" [left])

(define-key function-key-map "\eOp" [kp_0])
(define-key function-key-map "\eOq" [kp_1])
(define-key function-key-map "\eOr" [kp_2])
(define-key function-key-map "\eOs" [kp_3])
(define-key function-key-map "\eOt" [kp_4])
(define-key function-key-map "\eOu" [kp_5])
(define-key function-key-map "\eOv" [kp_6])
(define-key function-key-map "\eOw" [kp_7])
(define-key function-key-map "\eOx" [kp_8])
(define-key function-key-map "\eOy" [kp_9])

(define-key function-key-map "\eOk" [kp_add])
(define-key function-key-map "\eOm" [kp_subtract])
(define-key function-key-map "\eOM" [kp_enter])
(define-key function-key-map "\eOj" [kp_multiply])
(define-key function-key-map "\eOo" [kp_divide])
(define-key function-key-map "\eOn" [kp_decimal])

;; If you use the X resource -- XTerm*sunFunctionKeys: on -- you may
;; need these.
(define-key function-key-map "\e[224z" [f1])
(define-key function-key-map "\e[225z" [f2])
(define-key function-key-map "\e[226z" [f3])
(define-key function-key-map "\e[227z" [f4])
(define-key function-key-map "\e[228z" [f5])
(define-key function-key-map "\e[229z" [f6])
(define-key function-key-map "\e[230z" [f7])
(define-key function-key-map "\e[231z" [f8])
(define-key function-key-map "\e[232z" [f9])
(define-key function-key-map "\e[233z" [f10])
(define-key function-key-map "\e[234z" [f11])
(define-key function-key-map "\e[235z" [f12])
(define-key function-key-map "\e[194z" [(shift f1)])
(define-key function-key-map "\e[195z" [(shift f2)])
(define-key function-key-map "\e[196z" [(shift f3)])
(define-key function-key-map "\e[197z" [(shift f4)])
(define-key function-key-map "\e[198z" [(shift f5)])
(define-key function-key-map "\e[199z" [(shift f6)])
(define-key function-key-map "\e[200z" [(shift f7)])
(define-key function-key-map "\e[201z" [(shift f8)])
(define-key function-key-map "\e[208z" [(shift f9)])
(define-key function-key-map "\e[209z" [(shift f10)])
(define-key function-key-map "\e[210z" [(shift f11)])
(define-key function-key-map "\e[211z" [(shift f12)])
(define-key function-key-map "\e[2z" [insert])
(define-key function-key-map "\e[5z" [prior])
(define-key function-key-map "\e[6z" [next])
