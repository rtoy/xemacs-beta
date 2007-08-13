;;; japanese-hooks.el --- pre-loaded support for Japanese.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Synched up with: Mule 2.3.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JAPANESE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Syntax of Japanese characters.
(modify-syntax-entry 'katakana-jisx0201 "w")
(modify-syntax-entry 'japanese-jisx0212 "w")

(modify-syntax-entry 'japanese-jisx0208 "w")
(loop for row in '(33 34 40)
      do (modify-syntax-entry `[japanese-jisx0208 ,row] "_"))
(loop for char in '(?ー ?゛ ?゜ ?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-syntax-entry char "w"))
(modify-syntax-entry ?\（ "(）")
(modify-syntax-entry ?\［ "(］")
(modify-syntax-entry ?\｛ "(｝")
(modify-syntax-entry ?\「 "(」")
(modify-syntax-entry ?\『 "(』")
(modify-syntax-entry ?\） ")（")
(modify-syntax-entry ?\］ ")［")
(modify-syntax-entry ?\｝ ")｛")
(modify-syntax-entry ?\」 ")「")
(modify-syntax-entry ?\』 ")『")


;;; Character categories S, A, H, K, G, Y, and C
(define-category ?S "Japanese 2-byte symbol character.")
(modify-category-entry [japanese-jisx0208 33] ?S)
(modify-category-entry [japanese-jisx0208 34] ?S)
(modify-category-entry [japanese-jisx0208 40] ?S)
(define-category ?A "Japanese 2-byte Alphanumeric character.")
(modify-category-entry [japanese-jisx0208 35] ?A)
(define-category ?H "Japanese 2-byte Hiragana character.")
(modify-category-entry [japanese-jisx0208 36] ?H)
(define-category ?K "Japanese 2-byte Katakana character.")
(modify-category-entry [japanese-jisx0208 37] ?K)
(define-category ?G "Japanese 2-byte Greek character.")
(modify-category-entry [japanese-jisx0208 38] ?G)
(define-category ?Y "Japanese 2-byte Cyrillic character.")
(modify-category-entry [japanese-jisx0208 39] ?Y)
(define-category ?C "Japanese 2-byte Kanji characters.")
(loop for row from 48 to 126 do (modify-category-entry `[japanese-jisx0208 ,row] ?C))
(loop for char in '(?ー ?゛ ?゜)
      do (modify-category-entry char ?K)
         (modify-category-entry char ?H))
(loop for char in '(?ヽ ?ヾ ?ゝ ?ゞ ?〃 ?仝 ?々 ?〆 ?〇)
      do (modify-category-entry char ?C))
(modify-category-entry 'japanese-jisx0212 ?C)

(defvar japanese-word-regexp
  "\\cA+\\cH*\\|\\cK+\\cH*\\|\\cC+\\cH*\\|\\cH+\\|\\ck+\\|\\sw+"
  "Regular expression used to match a Japanese word.")

(set-word-regexp japanese-word-regexp)
(setq forward-word-regexp  "\\w\\>")
(setq backward-word-regexp "\\<\\w")

;;; Paragraph setting
(setq sentence-end
      (concat
       "\\("
       "\\("
       "[.?!][]\"')}]*"
       "\\|"
       "[．？！][］”’）｝〕〉》」』]*"
       "\\)"
       "\\($\\|\t\\|  \\)"
       "\\|"
       "。"
       "\\)"
       "[ \t\n]*"))
(setq paragraph-start    "^[ 　\t\n\f]")
(setq paragraph-separate "^[ 　\t\f]*$")

(make-coding-system
 'shift-jis 'shift-jis
 "Coding-system of Shift-JIS used in Japan."
 '(mnemonic "SJIS"))

(copy-coding-system 'shift-jis 'sjis)

(make-coding-system
 'iso-2022-jp 'iso2022
 "Coding-system used for communication with mail and news in Japan."
 '(charset-g0 ascii
   short t
   seven t
   input-charset-conversion ((latin-jisx0201 ascii)
			     (japanese-jisx0208-1978 japanese-jisx0208))
   mnemonic "Mail/Ja"
   ))

(copy-coding-system 'iso-2022-jp 'junet)

(make-coding-system
 'oldjis 'iso2022
 "Coding-system used for old JIS terminal."
 '(charset-g0 ascii
   short t
   seven t
   output-charset-conversion ((ascii latin-jisx0201)
			      (japanese-jisx0208 japanese-jisx0208-1978))
   mnemonic "Mail/Ja-old"
   ))

(make-coding-system
 'euc-japan 'iso2022
 "Coding-system of Japanese EUC (Extended Unix Code)."
 '(charset-g0 ascii
   charset-g1 japanese-jisx0208
   charset-g2 katakana-jisx0201
   charset-g3 japanese-jisx0212
   short t
   mnemonic "EUC/Ja"
   ))

(define-language-environment 'japanese
  "Japanese (includes JIS and EUC)"
  (lambda ()
    (set-coding-category-system 'iso-7   'iso-2022-jp)
    (set-coding-category-system 'iso-8-2 'euc-japan)
    (set-coding-priority-list '(iso-7 iso-8-2 shift-jis no-conversion))
    ;;'(iso-8-2 iso-8-designate iso-8-1 shift-jis big5)
    
    ;; EGG specific setup 97.02.05 jhod
    (when (featurep 'egg)
      (when (not (featurep 'egg-jpn))
	(provide 'egg-jpn)
	(setq wnn-server-type 'jserver)
	(load "its/its-hira")
	(load "its/its-kata")
	(load "its/its-hankaku")
	(load "its/its-zenkaku")
	(setq its:*standard-modes*
	      (append
	       (list (its:get-mode-map "roma-kana")
		     (its:get-mode-map "roma-kata")
		     (its:get-mode-map "downcase")
		     (its:get-mode-map "upcase")
		     (its:get-mode-map "zenkaku-downcase")
		     (its:get-mode-map "zenkaku-upcase"))
	       its:*standard-modes*)))
      (setq-default its:*current-map* (its:get-mode-map "roma-kana")))

    ;; Added by mrb, who doesn't speak japanese - so be sceptical...
    ;; (when (string-match "solaris\\|sunos" system-configuration)
    ;;(set-native-coding-system          'euc-japan) ; someday
    (set-pathname-coding-system 'euc-japan)
    (add-hook 'comint-exec-hook
              (lambda ()
                (let ((proc (get-buffer-process (current-buffer))))
                  (set-process-input-coding-system  proc 'euc-japan)
                  (set-process-output-coding-system proc 'euc-japan))))
    (set-buffer-file-coding-system-for-read 'autodetect)
    (set-default-buffer-file-coding-system  'euc-japan)
    (setq keyboard-coding-system            'euc-japan)
    (setq terminal-coding-system            'euc-japan)
    (when (eq 'x (device-type (selected-device)))
      (x-use-halfwidth-roman-font 'japanese-jisx0208 "jisx0201"))
    
    (when (eq system-type 'ms-dos)
      ;; Shift-JIS is the standard coding system under Japanese MS-DOS
      ;; This isn't really code - just a hint to future implementors
      (setq keyboard-coding-system           'shift-jis-dos)
      (setq terminal-coding-system           'shift-jis-dos)
      (set-default-buffer-file-coding-system 'shift-jis-dos)
      ;;(set-default-process-coding-system 'shift-jis-dos 'shift-jis-dos)
      )
    ))

(set-coding-category-system 'shift-jis 'shift-jis)

;; stuff for providing gramatic processing of Japanese text
;; something like this should probably be created for all environments...

(defvar aletter (concat "\\(" ascii-char "\\|" kanji-char "\\)"))
(defvar kanji-space-insertable (concat 
	   "、" aletter                   "\\|"
	   "。" aletter                   "\\|"
	   aletter "（"                   "\\|"
	   "）" aletter                   "\\|"
	   ascii-alphanumeric  kanji-kanji-char "\\|"
	   kanji-kanji-char    ascii-alphanumeric ))

(defvar space-insertable (concat " " aletter "\\|" kanji-space-insertable)
 "Regexp for finding points that can have spaces inserted into them for justification")
