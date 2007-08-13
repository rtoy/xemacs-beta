;;; linux.el --- define function key sequences for the Linux console

;; Author: Ben Wing
;; Keywords: terminals

;; Copyright (C) 1996 Ben Wing.
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

;;;; Code:

;; Termcap or terminfo should set these next four?
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\e[D" [left])

(define-key function-key-map "\e[[A" [f1])
(define-key function-key-map "\e[[B" [f2])
(define-key function-key-map "\e[[C" [f3])
(define-key function-key-map "\e[[D" [f4])
(define-key function-key-map "\e[[E" [f5])
(define-key function-key-map "\e[17~" [f6])
(define-key function-key-map "\e[18~" [f7])
(define-key function-key-map "\e[19~" [f8])
(define-key function-key-map "\e[20~" [f9])
(define-key function-key-map "\e[21~" [f10])
(define-key function-key-map "\e[23~" [f11])
(define-key function-key-map "\e[24~" [f12])
(define-key function-key-map "\e[25~" [f13])
(define-key function-key-map "\e[26~" [f14])
(define-key function-key-map "\e[28~" [f15])
(define-key function-key-map "\e[29~" [f16])
(define-key function-key-map "\e[31~" [f17])
(define-key function-key-map "\e[32~" [f18])
(define-key function-key-map "\e[33~" [f19])
(define-key function-key-map "\e[34~" [f20])

(define-key function-key-map "\e[1~" [home])
(define-key function-key-map "\e[2~" [insert])
(define-key function-key-map "\e[3~" [delete])
(define-key function-key-map "\e[4~" [end])
(define-key function-key-map "\e[5~" [prior])
(define-key function-key-map "\e[6~" [next])
(define-key function-key-map "\e[G" [kp_5])

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

(define-key function-key-map "\eOl" [kp_add])
(define-key function-key-map "\eOS" [kp_subtract])
(define-key function-key-map "\eOM" [kp_enter])
(define-key function-key-map "\eOR" [kp_multiply])
(define-key function-key-map "\eOQ" [kp_divide])
(define-key function-key-map "\eOn" [kp_decimal])
(define-key function-key-map "\eOP" [kp_numlock])

