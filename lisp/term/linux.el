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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 21.0.103.
;;; (All the define-keys are our own.)

;;; Commentary:

;;; Code:

;; The Linux console handles Latin-1 by default.

(if-fboundp 'set-terminal-coding-system
    (unless (declare-fboundp (terminal-coding-system))
      (set-terminal-coding-system 'iso-8859-1)))

;; Make Latin-1 input characters work, too.
;; Meta will continue to work, because the kernel
;; turns that into Escape.

(let ((value (current-input-mode)))
  ;; The third arg only matters in that it is not t or nil.
  (set-input-mode (nth 0 value) (nth 1 value) 'iso-latin-1 (nth 3 value)))

;; The defines below seem to get automatically set in recent Termcaps.
;; It was probably the case that in 1996, there was no good Linux termcap,
;; which is why such a file was needed.

; ;; Termcap or terminfo should set these next four?
; (define-key function-key-map "\e[A" [up])
; (define-key function-key-map "\e[B" [down])
; (define-key function-key-map "\e[C" [right])
; (define-key function-key-map "\e[D" [left])

; (define-key function-key-map "\e[[A" [f1])
; (define-key function-key-map "\e[[B" [f2])
; (define-key function-key-map "\e[[C" [f3])
; (define-key function-key-map "\e[[D" [f4])
; (define-key function-key-map "\e[[E" [f5])
; (define-key function-key-map "\e[17~" [f6])
; (define-key function-key-map "\e[18~" [f7])
; (define-key function-key-map "\e[19~" [f8])
; (define-key function-key-map "\e[20~" [f9])
; (define-key function-key-map "\e[21~" [f10])
; (define-key function-key-map "\e[23~" [f11])
; (define-key function-key-map "\e[24~" [f12])
; (define-key function-key-map "\e[25~" [f13])
; (define-key function-key-map "\e[26~" [f14])
; (define-key function-key-map "\e[28~" [f15])
; (define-key function-key-map "\e[29~" [f16])
; (define-key function-key-map "\e[31~" [f17])
; (define-key function-key-map "\e[32~" [f18])
; (define-key function-key-map "\e[33~" [f19])
; (define-key function-key-map "\e[34~" [f20])

;; But they come out f13-f20 (see above), which are not what we
;; normally call the shifted function keys.  F11 = Shift-F1, F2 =
;; Shift-F2.  What a mess, see below.
(define-key function-key-map "\e[25~" [(shift f3)])
(define-key function-key-map "\e[26~" [(shift f4)])
(define-key function-key-map "\e[28~" [(shift f5)])
(define-key function-key-map "\e[29~" [(shift f6)])
(define-key function-key-map "\e[31~" [(shift f7)])
(define-key function-key-map "\e[32~" [(shift f8)])
(define-key function-key-map "\e[33~" [(shift f9)])
(define-key function-key-map "\e[34~" [(shift f10)])

;; I potentially considered these.  They would make people's Shift-F1 and
;; Shift-F2 bindings work -- but of course they would fail to work if the
;; person also put F11 and F12 bindings.  It might also be confusing because
;; the person with no bindings who hits f11 gets "error shift-f1 unbound".
;; #### If only there were a proper way around this.
;(define-key global-map 'f11 [(shift f1)])
;(define-key global-map 'f12 [(shift f2)])

; (define-key function-key-map "\e[1~" [home])
 ;; seems to not get handled correctly automatically
 (define-key function-key-map "\e[2~" [insert])
; (define-key function-key-map "\e[3~" [delete])
; (define-key function-key-map "\e[4~" [end])
; (define-key function-key-map "\e[5~" [prior])
; (define-key function-key-map "\e[6~" [next])
; (define-key function-key-map "\e[G" [kp-5])

; (define-key function-key-map "\eOp" [kp-0])
; (define-key function-key-map "\eOq" [kp-1])
; (define-key function-key-map "\eOr" [kp-2])
; (define-key function-key-map "\eOs" [kp-3])
; (define-key function-key-map "\eOt" [kp-4])
; (define-key function-key-map "\eOu" [kp-5])
; (define-key function-key-map "\eOv" [kp-6])
; (define-key function-key-map "\eOw" [kp-7])
; (define-key function-key-map "\eOx" [kp-8])
; (define-key function-key-map "\eOy" [kp-9])

; (define-key function-key-map "\eOl" [kp-add])
; (define-key function-key-map "\eOS" [kp-subtract])
; (define-key function-key-map "\eOM" [kp-enter])
; (define-key function-key-map "\eOR" [kp-multiply])
; (define-key function-key-map "\eOQ" [kp-divide])
; (define-key function-key-map "\eOn" [kp-decimal])
; (define-key function-key-map "\eOP" [kp-numlock])

;;; linux.el ends here

