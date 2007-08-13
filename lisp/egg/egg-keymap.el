;; Usefull key binding for Sun Function keys 
;; Coded by Yutaka Ishikawa at ETL (yisikawa@etl.junet)

;; This file is part of Egg on Mule (Multilingual Environment)

;; Egg is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Egg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Mule; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; 92.4.16  modified for Mule Ver.0.9.3 by K.Handa <handa@etl.go.jp>

;;
;; Table of Function key codes:
;;
;;;		x11term		xterm
;;;
;;; XK_F1:	[f1]		"\e[11~"
;;; XK_F2:	[f2]		"\e[12~"
;;; XK_F3:	[f3]		"\e[13~"
;;; XK_F4:	[f4]		"\e[14~"
;;; XK_F5:	[f5]		"\e[15~"
;;; XK_F6:	[f5]		"\e[17~"
;;; XK_F7:	[f7]		"\e[18~"
;;; XK_F8:	[f8]		"\e[19~"
;;; XK_F9:	[f9]		"\e[20~"
;;;
;;; XK_F10:	[f10]		"\e[21~"
;;; XK_F11:	[f11]		"\e[23~"
;;; XK_F12:	[f12]		"\e[24~"
;;; XK_F13:	[f13]		"\e[25~"
;;; XK_F14:	[f14]		"\e[26~"
;;; XK_F15:	[f15]		"\e[28~"
;;; XK_Help:	[help]		"\e[28~"
;;; XK_F16:	[f16]		"\e[29~"
;;; XK_Menu:	???		"\e[29~"
;;; XK_F17:	[f17]		"\e[31~"
;;; XK_F18:	[f18]		"\e[32~"
;;; XK_F19:	[f19]		"\e[33~"
;;; XK_F20:	[f20]		"\e[34~"
;;;	
;;; XK_Find :	[find]		"\e[1~"
;;; XK_Insert:	[insert]	"\e[2~"
;;; XK_Delete:	[delete]	"\e[3~"
;;; XK_Select:	???		"\e[4~"
;;; XK_Prior:	[prior]		"\e[5~"
;;; XK_Next:	[next]		"\e[6~"
;;;
;;; XK_Left:	[left]		"\eOC"(XK_R12)
;;; XK_Right:	[right]		"\eOD"(XK_R10)
;;; XK_Up:	[up]		"\eOA"(XK_R8)
;;; XK_Down:	[down]		"\eOB"(XK_R10)
;;;

;;;
;;; Key bindings for X11 terminals(x11term)
;;;

(define-key global-map [f1] 'set-file-coding-system)
(define-key global-map [f2] 'edit-dict-item)
(define-key global-map [f3] 'jis-code-input)
(define-key global-map [f4] 'toroku-region)
(define-key global-map [f5] 'zenkaku-region)
(define-key global-map [f6] 'hankaku-region)
(define-key global-map [f7] 'katakana-region)
(define-key global-map [f8] 'hiragana-region)
(define-key global-map [f9] 'henkan-region)

(define-key global-map [f11] 'insert-buffer)
(define-key global-map [f12] 'insert-file)
(define-key global-map [f13] 'eval-region)
(define-key global-map [f14] 'eval-current-buffer)
(define-key global-map [f15] 'enlarge-window)
(define-key global-map [f16] 'shrink-window)
(define-key global-map [f17] 'revert-buffer)
(define-key global-map [f18] 'revert-buffer)
(define-key global-map [f19] 'beginning-of-buffer)
(define-key global-map [f20] 'end-of-buffer)

;;;
;;; Key bindings for non X11 terminal([kx]term)
;;;

(defvar sun-fkeymap (make-keymap))
(fset 'sun-fprefix sun-fkeymap)

(define-key global-map "\e[" 'sun-fprefix)
(define-key sun-fkeymap "["   'backward-paragraph) ; old "\e[" assignment
(define-key sun-fkeymap "11~" 'set-file-coding-system)	; F1 92.4.16 by K.Handa
(define-key sun-fkeymap "12~" 'edit-dict-item)	        ; F2
(define-key sun-fkeymap "13~" 'jis-code-input)   	; F3
(define-key sun-fkeymap "14~" 'toroku-region)		; F4
(define-key sun-fkeymap "15~" 'zenkaku-region)		; F5
(define-key sun-fkeymap "17~" 'hankaku-region)		; F6
(define-key sun-fkeymap "18~" 'katakana-region)		; F7
(define-key sun-fkeymap "19~" 'hiragana-region)		; F8
(define-key sun-fkeymap "20~" 'henkan-region)		; F9

(define-key sun-fkeymap "23~" 'insert-buffer)		; F11 or         L1
(define-key sun-fkeymap "24~" 'insert-file)		; F12 or         L2
(define-key sun-fkeymap "25~" 'eval-region)		; F13 or         L3
(define-key sun-fkeymap "26~" 'eval-current-buffer)	; F14 or         L4
(define-key sun-fkeymap "28~" 'enlarge-window)		; F15 or Help or L5
(define-key sun-fkeymap "29~" 'shrink-window)		; F16 or Menu or L6
(define-key sun-fkeymap "31~" 'revert-buffer)		; F17 or         L7
(define-key sun-fkeymap "32~" 'revert-buffer)		; F18 or         L8
(define-key sun-fkeymap "33~" 'beginning-of-buffer)	; F19 or         L9
(define-key sun-fkeymap "34~" 'end-of-buffer)		; F20 or         L10
