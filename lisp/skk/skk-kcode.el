;;; skk-kcode.el --- 漢字コードを使った変換のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-kcode.el,v 1.1 1997/12/02 08:48:38 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:38 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Following people contributed modifications to skk.el (Alphabetical order):
;;       Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)

(defvar skk-input-by-code-menu-keys1 '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y)
  "*メニュー形式で JIS 文字を入力するときに使用する選択キーのリスト。
第 1 段階のメニューで使用する。
12 個のキー (char type) を含む必要がある。")

(defvar skk-input-by-code-menu-keys2
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u)
  "*メニュー形式で JIS 文字を入力するときに使用する選択キーのリスト。
第 2 段階のメニューで使用する。
16 個のキー (char type) を含む必要がある。")

(defvar skk-kcode-load-hook nil
  "*skk-kcode.el をロードした後にコールされるフック。" )

;; variables for the function skk-input-by-code-or-menu
(defconst skk-code-n1-min 161)
(defconst skk-code-n1-max 244)
(defconst skk-code-n2-min 161)
(defconst skk-code-n2-max 254)
(defconst skk-code-null 128)
(defvar skk-input-by-code-or-menu-jump-default skk-code-n1-min)
(skk-deflocalvar skk-kcode-charset
  (if (or skk-mule3 skk-xemacs)
      'japanese-jisx0208
    lc-jp)
  "skk-input-by-code-or-menu で使われる文字セット。" )
(defconst skk-kcode-definded-charsets
  (if (or skk-mule3 skk-xemacs)
      (mapcar '(lambda (x) (list (symbol-name x))) (charset-list))
    nil ))

;;;###skk-autoload
(defun skk-input-by-code-or-menu (&optional arg)
  "7bit もしくは 8bit もしくは 区点コードに対応する 2byte 文字を挿入する。"
  ;; The function skk-input-by-code-or-menu, which was used until version
  ;; 4.20, is now replaced by this new function.
  (interactive "*P")
  (if arg
      (let ((charset
	     (intern (completing-read (format "CHARSET(%s): " skk-kcode-charset)
				      skk-kcode-definded-charsets nil t ))))
	(cond ((null charset))
	      ((not (skk-charsetp charset))
	       (error "invalid charset"))
	      (t (setq skk-kcode-charset charset)) )))
  (let ((str
	 (read-string
	  (format
	   "7/8 bits or KUTEN code for %s (00nn or CR for Jump Menu): "
	   skk-kcode-charset )))
	(enable-recursive-mini-buffer t)
	n1 n2 )
    (if (string-match "\\(.+\\)-\\(.+\\)" str)
	(setq n1 (+ (string-to-number (match-string 1 str)) 32 128)
	      n2 (+ (string-to-number (match-string 2 str)) 32 128) )
      (setq n1 (if (string= str "") 128
		 (+ (* 16 (skk-jis-char-to-hex (aref str 0)))
		    (skk-char-to-hex (aref str 1)) ))
	    n2 (if (string= str "") 128
		 (+ (* 16 (skk-jis-char-to-hex (aref str 2)))
		    (skk-char-to-hex (aref str 3)) ))))
    (insert (if (> n1 160)
		(skk-make-string n1 n2)
	      (skk-input-by-code-or-menu-0 n1 n2) ))
    (if skk-henkan-active (skk-kakutei)) ))

(defun skk-char-to-hex (char)
  (cond ((> char 96) (- char 87)) ; a-f
        ((> char 64) (- char 55)) ; A-F
        ((> char 47) (- char 48)) ; 0-9
        (t
         ;; 物言わぬエラーは良くないが...。
         (error "") )))

(defun skk-jis-char-to-hex (char)
  (cond ((> char 96) (- char 87)) ; a-f
        ((> char 64) (- char 55)) ; A-F
        ((> char 47) (- char 40)) ; 0-9
        (t
         ;; 物言わぬエラーは良くないが...。
         (error "") )))

(defun skk-make-string (n1 n2)
  (char-to-string (skk-make-char skk-kcode-charset n1 n2)) )

(defun skk-next-n2-code (n)
  (if (<= (setq n (1+ n)) skk-code-n2-max) n skk-code-n2-min))

(defun skk-previous-n2-code (n)
  (if (<= skk-code-n2-min (setq n (1- n))) n skk-code-n2-max))

(defun skk-next-n1-code (n)
  (if (<= (setq n (1+ n)) skk-code-n1-max) n skk-code-n1-min))

(defun skk-previous-n1-code (n)
  (if (<= skk-code-n1-min (setq n (1- n))) n skk-code-n1-max))

(defun skk-input-by-code-or-menu-0 (n1 n2)
  (if (= n1 skk-code-null)
      (skk-input-by-code-or-menu-jump n2)
    (skk-input-by-code-or-menu-1 n1 n2)))

(defun skk-input-by-code-or-menu-jump (n)
  (let ((menu-keys1 ; 表示用のキーリストを組み立てる。
         (mapcar (function (lambda (char) (char-to-string (upcase char))))
                 skk-input-by-code-menu-keys1 ))
        kanji-char )
    (if (< n skk-code-n1-min) (setq n skk-input-by-code-or-menu-jump-default))
    (while (not kanji-char)
      (let ((n-org n)
            (chars
             (list
              (list (skk-make-string n skk-code-n1-min) n skk-code-n1-min)
              (list (skk-make-string n 177) n 177)
              (list (skk-make-string n 193) n 193)
              (list (skk-make-string n 209) n 209)
              (list (skk-make-string n 225) n 225)
              (list (skk-make-string n 241) n 241)
              (progn
                (setq n (skk-next-n1-code n))
                (list (skk-make-string n skk-code-n1-min) n
                      skk-code-n1-min ))
              (list (skk-make-string n 177) n 177)
              (list (skk-make-string n 193) n 193)
              (list (skk-make-string n 209) n 209)
              (list (skk-make-string n 225) n 225)
              (list (skk-make-string n 241) n 241))))
        (skk-save-point
          (let ((i 0) message-log-max str )
            (while (< i 12)
              (setq str (concat str (nth i menu-keys1) ":" (car (nth i chars))
                                "  " ))
              (setq i (1+ i)) )
            (message str) )
          (let ((char (skk-read-event))
                rest ch )
            (if (not (integerp char))
                (progn
                  (skk-message "\"%s\" は有効なキーではありません！"
                               "\"%s\" is not valid here!" (prin1 char) )
                  (sit-for 1)
                  (message "")
                  (setq n n-org) )
              (setq rest (or (memq char skk-input-by-code-menu-keys1)
                             (if (skk-lower-case-p char)
                                 (memq (upcase char) skk-input-by-code-menu-keys1)
                               (memq (downcase char) skk-input-by-code-menu-keys1) ))
                    ch (if rest
                           ;; 12 == (length skk-input-by-code-menu-keys1)
                           (nth (- 12 (length rest)) chars)
                         nil )
                    kanji-char
                    (cond
                     (ch)
                     ((eq char 120)     ; x
                      (if (< (setq n (- n-org 2)) skk-code-n1-min)
                          (setq n skk-code-n1-max))
                      nil)
                     ((eq char 32)      ; space
                      (setq n (skk-next-n1-code n))
                      nil)
                     ((eq char 63)      ; ?
                      (skk-message
                       (concat "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[何かキーを押してください]" )
                       (concat "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[Hit any key to continue]" )
                       (car (car chars))
                       n-org skk-code-n1-min n-org skk-code-n1-min
                       (- n-org 128) (- skk-code-n1-min 128)
                       (- n-org 128) (- skk-code-n1-min 128) )
                      (skk-read-event)
                      (setq n n-org)
                      nil)
                     (t
                      (skk-message "\"%c\" は有効なキーではありません！"
                                   "\"%c\" is not valid here!" char )
                      (sit-for 1)
                      (message "")
                      (setq n n-org)
                      nil ))))))))
    (setq skk-input-by-code-or-menu-jump-default (car (cdr kanji-char)))
    (skk-input-by-code-or-menu-1
     (car (cdr kanji-char)) (car (cdr (cdr kanji-char))) )))

(defun skk-input-by-code-or-menu-1 (n1 n2)
  (let ((menu-keys2 ; 表示用のキーリストを組み立てる。
         (mapcar (function (lambda (char) (char-to-string (upcase char))))
                 skk-input-by-code-menu-keys2 ))
        kanji-char )
    (while (not kanji-char)
      (let ((n1-org n1) (n2-org n2) (i 0)
            (chars (list (skk-make-string n1 n2))))
        ;; 16 == (length skk-input-by-code-menu-keys2)
        (while (< i 16)
          (nconc chars (list
                        (progn (setq n2 (skk-next-n2-code n2))
                               (if (= n2 skk-code-n2-min)
                                   (setq n1 (skk-next-n1-code n1)))
                               (skk-make-string n1 n2))))
          (setq i (1+ i)))
        (skk-save-point
          (let ((i 0) message-log-max str )
            (while (< i 16)
              (setq str (concat str (nth i menu-keys2) ":" (nth i chars) " "))
              (setq i (1+ i)) )
            (message str) )
          (let ((char (skk-read-event)))
            (if (not (integerp char))
                (progn
                  (skk-message "\"%s\" は有効なキーではありません！"
                               "\"%s\" is not valid here!" (prin1 char) )
                  (sit-for 1)
                  (message "")
                  (setq n1 n1-org n2 n2-org) )
              (setq rest
                    (or (memq char skk-input-by-code-menu-keys2)
                        (if (skk-lower-case-p char)
                            (memq (upcase char) skk-input-by-code-menu-keys2)
                          (memq (downcase char) skk-input-by-code-menu-keys2) ))
                    ch (if rest
                           ;; 16 == (length skk-input-by-code-menu-keys2)
                           (nth (- 16 (length rest)) chars) )
                    kanji-char
                    (cond
                     (ch)
                     ((eq char 120)     ; x
                      (if (< (setq n2 (- n2 31)) skk-code-n2-min)
                          (setq n2 (+ n2 94)
                                n1 (skk-previous-n1-code n1)))
                      nil )
                     ((eq char 32)      ; space
                      (if (= (setq n2 (skk-next-n2-code n2))
                             skk-code-n2-min)
                          (setq n1 (skk-next-n1-code n1)))
                      nil )
                     ((eq char 63)      ; ?
                      (skk-message
                       (concat "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[何かキーを押してください]" )
                       (concat "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d)  "
                               "[Hit any key to continue]" )
                       (car chars) n1-org n2-org n1-org n2-org
                       (- n1-org 128) (- n2-org 128)
                       (- n1-org 128) (- n2-org 128) )
                      (skk-read-event)
                      (setq n1 n1-org n2 n2-org)
                      nil )
                     ((eq char 62)      ; >
                      (if (= (setq n2 (skk-next-n2-code n2-org))
                             skk-code-n2-min)
                          (setq n1 (skk-next-n1-code n1-org))
                        (setq n1 n1-org))
                      nil )
                     ((eq char 60)      ; <
                      (if (= (setq n2 (skk-previous-n2-code n2-org))
                             skk-code-n2-max)
                          (setq n1 (skk-previous-n1-code n1-org))
                        (setq n1 n1-org))
                      nil )
                     (t
                      (skk-message "\"%c\" は有効なキーではありません！"
                                   "\"%c\" is not valid here!" char )
                      (sit-for 1)
                      (message "")
                      (setq n1 n1-org n2 n2-org)
                      nil ))))))))
    kanji-char ))

;;;###skk-autoload
(defun skk-display-code-for-char-at-point ()
  "ポイントにある文字の EUC コードと JIS コードを表示する。"
  (interactive)
  (if (eobp)
      (skk-error "カーソルがバッファの終端にあります"
                 "Cursor is at the end of the buffer" )
    (let ((str
           (skk-buffer-substring
            (point)
            (skk-save-point (forward-char 1) (point)))))
      (cond
       (skk-xemacs
        (let* ((char (string-to-char str))
               (charset (char-charset char)))
          (cond
           ((memq charset '(japanese-jisx0208 japanese-jisx0208-1978))
            (let* ((char1-j (char-octet char 0))
                   (char1-k (- char1-j 32))
                   (char1-e (+ char1-j 128))
                   (char2-j (char-octet char 1))
                   (char2-k (- char2-j 32))
                   (char2-e (+ char2-j 128)))
              (message
               "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d)"
               str char1-e char2-e char1-e char2-e
               char1-j char2-j char1-j char2-j char1-k char2-k)))
           ((memq charset '(ascii latin-jisx0201))
            (message "\"%s\"  %2x (%3d)"
                     str (char-octet char 0)  (char-octet char 0)))
           (t
            (skk-error "判別できない文字です"
                       "Cannot understand this character")))
          ))
       (skk-mule3
        (let* ((char (string-to-char str))
               (charset (char-charset char)))
          (cond
           ((memq charset '(japanese-jisx0208 japanese-jisx0208-1978))
            (let* ((char-list (mapcar (function +) str))
                   (char1-e (car (cdr char-list)))
                   (char1-j (- char1-e 128))
                   (char1-k (- char1-j 32))
                   (char2-e (car (cdr (cdr char-list))))
                   (char2-j (- char2-e 128))
                   (char2-k (- char2-j 32)))
              (message
               "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d)"
               str char1-e char2-e char1-e char2-e
               char1-j char2-j char1-j char2-j char1-k char2-k)))
           ((memq charset '(ascii latin-jisx0201))
            (message "\"%s\"  %2x (%3d)" char char char))
           (t
            (skk-error "判別できない文字です"
                       "Cannot understand this character")))
          ))
       (t ; skk-mule
        (let (;; 文字列を char に分解。
              ;; (mapcar '+ str) == (append str nil)
              (char-list (mapcar (function +) str)))
          (cond
           ((and (= (length char-list) 3)
                 (memq (car char-list) (list lc-jp lc-jpold)))
            (let* ((char1-e (car (cdr char-list)))
                   (char1-j (- char1-e 128))
                   (char1-k (- char1-j 32))
                   (char2-e (car (cdr (cdr char-list))))
                   (char2-j (- char2-e 128))
                   (char2-k (- char2-j 32)))
              (message
               "『%s』  EUC: %2x%2x (%3d, %3d), JIS: %2x%2x (%3d, %3d), KUTEN: (%2d, %2d)"
               str char1-e char2-e char1-e char2-e
               char1-j char2-j char1-j char2-j char1-k char2-k)))
           ((or (= (length char-list) 1) ; ascii character
                (memq (car char-list) (list lc-ascii lc-roman)))
            (let ((char (car char-list)))
              (message "\"%c\"  %2x (%3d)" char char char)))
           (t
            (skk-error "判別できない文字です"
                       "Cannot understand this character" ))
           )))
       ))))

(run-hooks 'skk-kcode-load-hook)

(provide 'skk-kcode)
;;; skk-kcode.el ends here
