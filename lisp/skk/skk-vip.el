;;; skk-vip.el --- vip related functions for skk.el
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-vip.el,v 1.1 1997/12/02 08:48:39 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:39 $

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

;; Following people contributed to skk-vip.el (Alphabetical order):
;;      Kiyotaka Sakai <ksakai@netwk.ntt-at.co.jp>
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;      Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;;; Change log:
;; version 1.1.4 released 1996.12.15 (derived from the skk.el)

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)
(require 'advice)

(condition-case nil
    ;; (require 'vip) すべきだが、vip 3.5 では (provide 'vip) されていない。
    (require 'vip)
  (error (if (not (boundp 'vip-current-mode)) (load "vip"))) )

(defvar skk-vip-load-hook nil
  "*skk-vip.el がロードされた後にコールされるフック。" )

;;; --- user variable.
(defvar skk-vip-mode-hook nil
  "*skk-use-vipが non-nil の場合で、skk-mode を起動したときにコールされるフック。" )

;; internal variable.
(defvar skk-vip-mode-invoked nil
  "Non-nil であれば、Emacs を起動後既に skk-vip-mode を起動したことを示す。" )

(if (boundp 'vip-ovwrt-limit)
    (defun skk-ovwrt-len (len)
      ;; 上書きして良い長さを返す。
      ;; vip-ovwrt-limit は vip version 4 で使用されている動的変数。
      ;; 可能であれば、このようなコーディングスタイルは避けるべき。
      (min (- vip-ovwrt-limit (point))
           (- (skk-save-point (end-of-line) (point)) (point)) len )))

;;;###skk-autoload
(defun skk-vip-mode ()
  ;; skk-mode のコール時に VIP 関連のセットアップを行う。
  (let (skk-mode) (vip-change-mode-to-insert))
  (if (not skk-vip-mode-invoked)
      (prog1 (run-hooks 'skk-vip-mode-hook)
        (setq skk-vip-mode-invoked t) )))

(defun skk-insert-str (str)
  ;; skk-insert のサブルーチン。STR を挿入する。必要であれば
  ;; self-insert-after-hook をコールする。overwrite-mode であれば、適切に上書き
  ;; を行う (<(skk.el/skk-insert-str)>)。
  (skk-cancel-undo-boundary)
  (skk-insert-and-inherit str)
  (if (and skk-henkan-on (not skk-henkan-active))
      (if (and skk-auto-start-henkan (not skk-okurigana))
          (skk-auto-start-henkan str) )
    (if (and (boundp 'self-insert-after-hook) self-insert-after-hook)
        (funcall self-insert-after-hook (- (point) (length str)) (point)))
    ;; (boundp 'vip-ovwrt-limit) means that this function is within the
    ;; dynamic scope of vip-overwrite-execute
    (if (or overwrite-mode (boundp 'vip-ovwrt-limit))
        (skk-del-char-with-pad (skk-ovwrt-len (string-width str))) )))

(defun skk-kakutei-cleanup-henkan-buffer ()
  ;; 確定直後のバッファの整形を行なう 
  ;; (<(skk.el/skk-kakutei-cleanup-henkan-buffer)>)。
  (if skk-okurigana
      (progn
        (skk-delete-okuri-mark)
        (if (and skk-katakana skk-convert-okurigana-into-katakana)
            (skk-katakana-region skk-henkan-end-point (point)) )))
  (skk-delete-henkan-markers)
  (if (and (boundp 'self-insert-after-hook) self-insert-after-hook)
      (funcall self-insert-after-hook skk-henkan-start-point (point)) )
  (if (or overwrite-mode (boundp 'vip-ovwrt-limit))
      (skk-del-char-with-pad
       (skk-ovwrt-len
        (string-width
         (skk-buffer-substring skk-henkan-start-point (point)) )))))

(defadvice vip-ESC (before skk-add activate)
  "▽モード、▼モードだったら確定する。"
  (and skk-mode skk-henkan-on (skk-kakutei)) )

(defadvice vip-insert (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-Insert (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-open-line (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-Open-line (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-append (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-Append (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

(defadvice vip-overwrite (after skk-ad activate)
  "skk-mode だったらかなモードにする。"
  (if skk-mode (skk-j-mode-on)) )

;;;; こりゃダメっすね。
;;;;(defadvice vip-replace-char (after skk-ad activate)
;;;;  "skk-mode だったらかなモードにする。"
;;;;  (if skk-mode (skk-j-mode-on)) )

;;(defadvice delete-backward-char (around skk-ad activate)
;;  (cond ((and skk-henkan-on (>= skk-henkan-start-point (point)))
;;         (setq skk-henkan-count 0)
;;         (skk-kakutei) )
;;        (skk-henkan-active
;;         (if (and (not skk-delete-implies-kakutei)
;;                  (= skk-henkan-end-point (point)) )
;;             (skk-previous-candidate)
;;           ;;(if skk-use-face (skk-henkan-face-off))
;;           (if overwrite-mode
;;               (progn
;;                 (backward-char (prefix-numeric-value count))
;;                 (delete-char (prefix-numeric-value count)) )
;;             ad-do-it )
;;           (if (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
;;        ((and skk-henkan-on overwrite-mode)
;;         (backward-char (prefix-numeric-value count))
;;         (delete-char (prefix-numeric-value count)) )
;;        (t ad-do-it) ))

(run-hooks 'skk-vip-load-hook)

(provide 'skk-vip)
;; skk-vip.el ends here
