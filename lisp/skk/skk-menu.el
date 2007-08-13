;;; skk-menu.el --- SKK Menul related functions.
;; Copyright (C) 1996, 1997 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-menu.el,v 1.1 1997/12/02 08:48:38 steve Exp $
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

;;; Change log:
;;; derived from skk.el 9.6.

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)

(defvar skk-menu-annotation-buffer "*SKK Menu Annotation*"
  "SKK メニューのための註釈を表示するバッファ。" )

(defun skk-menu-setup-annotation-buffer (annotation)
  ;; skk-menu-annotation-buffer を作り、ANNOTATION を表示する。
  (if (and annotation (not (string= annotation "")))
      (save-current-buffer
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create skk-menu-annotation-buffer))
        (delete-region (point-min) (point-max))
        (insert annotation)
        (goto-char (point-min)) )))

(defun skk-menu-change-user-option (var on-alist off-alist)
  ;; VAR のドキュメントを表示してユーザーの指示に従いその値に non-nil/nil を代
  ;; 入する。
  ;; ON-ALIST には、オプション VAR の値を non-nil にするときに設定する変数を、
  ;; OFF-ALIST は VAR の値を nil にする場合の設定を
  ;;   '((変数名0 . 値0) (変数名1 . 値1) ... (変数名n . 値n))
  ;; の形で連想リストで指定する。ON-ALIST, OFF-ALIST には VAR 自身の設定も指定
  ;; する必要がある。
  (let (
        ;; ダイアログボックスがフレームの中央に出て annotation バッファが読め
        ;; ないので、ダイアログボックスを出さないようにする。
        (last-nonmenu-event t)
        (on (symbol-value var))
        answer )
    (save-window-excursion
      (skk-menu-setup-annotation-buffer
       (concat (format "現在の %S の値は、%S です。\n\n" var on)
               (documentation-property var 'variable-documentation) ))
      ;; y-or-n-p でも良いのだが、y-or-n-p はミニバッファを利用していてミニバッ
      ;; ファを使ってないので、annotation buffer にカーソルを移し、文字列をコ
      ;; ピーしたりできなくなってしまう。
      (setq answer (yes-or-no-p (format
                                 (if skk-japanese-message-and-error
                                     "このオプションを %S にしますか？"
                                   "Turn %S this option?" )
                                 (if on "off" "on") )))
      (if answer
          (if on
              ;; turn off
              (skk-menu-change-user-option-1 off-alist)
            ;; turn on
            (skk-menu-change-user-option-1 on-alist) )))))

(defun skk-menu-change-user-option-1 (alist)
  ;; ALIST を skk-menu-modified-user-option の最後方に連結し、ALIST の値の要素
  ;; の car に cdr の値を代入する。
  (let ((n 0)
        cell modified )
    (while (setq cell (nth n alist))
      (setq n (1+ n)
            modified (assq (car cell) skk-menu-modified-user-option) )
      (if modified
          (setq skk-menu-modified-user-option
                ;; 既に同じ変数をモディファイしていたら、古いものを削除する。
                (delq modified skk-menu-modified-user-option) ))
      (set (car cell) (cdr cell)) )
    (setq skk-menu-modified-user-option
          (nconc skk-menu-modified-user-option alist) )))

;;;###skk-autoload
(defun skk-menu-save-modified-user-option ()
  ;; SKK のメニューで変更されたユーザーオプションを skk-init-file の末尾に保存
  ;; する。
  (if (and
       skk-menu-modified-user-option
       (skk-yes-or-no-p
        "SKK オプションが起動後変更されています。この値を保存しますか？"
        "Changed user options after SKK invoked.  Save the variables?" ))
      (progn
        (skk-menu-save-modified-user-option-1)
        (skk-message
         "SKK のオプション設定を %s に保存しました"
         "Save user options of SKK in %s"
         skk-init-file )
        (sit-for 1) )))

(defun skk-menu-save-modified-user-option-1 ()
  ;; skk-menu-save-modified-user-option-1 のサブルーチン。~/.skk に
  ;; skk-menu-modified-user-option の値を書き込み次に skk.el が起動されたとき
  ;; でも変更された値を有効にする。
  ;; 過去に既にこの関数により式が書き込まれていたら、既定値との調整も行なう。
  (save-match-data
    (with-current-buffer (find-file-noselect (expand-file-name skk-init-file))
      (let (
            ;; デコレーションなし。
            (hilit-auto-highlight-maxout 0)
            (font-lock-maximum-size 0)
            (require-final-newline t)
            buffer-read-only
            start first-kiss cell )
        (goto-char (point-min))
        (setq first-kiss
              (not
               (re-search-forward
                "^;; 下記の式は SKK によって自動的に書き込まれたものです。$"
                nil t )))
        (if first-kiss
            (progn
              (setq start (goto-char (point-max)))
              (insert
               ";; 下記の式は SKK によって自動的に書き込まれたものです。\n"
               ))
          (let ((alist skk-menu-modified-user-option)
                var)
            (setq start (point))
            (re-search-forward
             "^;; 上記の式は SKK によって自動的に書き込まれたものです。$"
             nil )
            (while (setq var (car (car alist)))
              (skk-save-point
                (and (re-search-backward (prin1-to-string var) start t)
                     (delete-region (progn (beginning-of-line) (point))
                                    (progn (forward-line 1) (point)) )))
              (setq alist (cdr alist)) )
            (beginning-of-line) ))
        ;; さて、ここからは共通の処理です。
        (while skk-menu-modified-user-option
          (setq cell (car skk-menu-modified-user-option)
                skk-menu-modified-user-option
                (cdr skk-menu-modified-user-option) )
          (insert "(setq " (prin1-to-string (car cell)) " "
                  (prin1-to-string (cdr cell)) ")\n" ))
        ;;(delete-char -1)
        (if first-kiss
            (insert
             ";; 上記の式は SKK によって自動的に書き込まれたものです。\n"
             ))
        (save-buffer)
        (kill-buffer (current-buffer)) ))))

;;;###skk-autoload
(defun skk-menu-process-okuri-early ()
  "skk-process-okuri-early をスイッチオン/オフする。
両立できないオプションの値を調整する。"
  (interactive)
  (skk-menu-change-user-option
   'skk-process-okuri-early
   ;; on-alist
   '((skk-process-okuri-early . t)
     (skk-auto-okuri-process . nil)
     (skk-henkan-okuri-strictly . nil)
     (skk-henkan-strict-okuri-precedence . nil)
     (skk-kakutei-early . nil) )
   ;; off-alist
   '((skk-process-okuri-early . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-henkan-okuri-strictly ()
  "skk-henkan-okuri-strictly をスイッチオン/オフする。
両立できないオプションの値を調整する。"
  (interactive)
  (skk-menu-change-user-option
   'skk-henkan-okuri-strictly
   ;; on-alist
   '((skk-henkan-okuri-strictly . t)
     (skk-process-okuri-early . nil) )
   ;; off-alist
   '((skk-henkan-okuri-strictly . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-henkan-strict-okuri-precedence ()
  "skk-henkan-strict-okuri-precedence をスイッチオン/オフする。
両立できないオプションの値を調整する。"
  (interactive)
  (skk-menu-change-user-option
   'skk-henkan-strict-okuri-precedence
   ;; on-alist
   '((skk-henkan-strict-okuri-precedence . t)
     (skk-henkan-okuri-strictly . nil)
     (skk-process-okuri-early . nil) )
   ;; off-alist
   '((skk-henkan-strict-okuri-precedence . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-auto-okuri-process ()
  "skk-auto-okuri-process をスイッチオン/オフする。
両立できないオプションの値を調整する。"
  (interactive)
  (skk-menu-change-user-option
   'skk-auto-okuri-process
   ;; on-alist
   '((skk-auto-okuri-process . t)
     (skk-process-okuri-early . nil) )
   ;; off-alist
   '((skk-auto-okuri-process . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-kakutei-early ()
  "skk-kakutei-early をスイッチオン/オフする。
両立できないオプションの値を調整する。"
  (interactive)
  (skk-menu-change-user-option
   'skk-kakutei-early
   ;; on-alist
   '((skk-kakutei-early . t)
     (skk-process-okuri-early . nil) )
   ;; off-alist
   '((skk-kakutei-early . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-egg-like-newline ()
  "skk-egg-like-newline をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-egg-like-newline
   '((skk-egg-like-newline . t))
   '((skk-egg-like-newline . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-delete-implies-kakutei ()
  "skk-delete-implies-kakutei をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-delete-implies-kakutei
   '((skk-delete-implies-kakutei . t))
   '((skk-delete-implies-kakutei . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-allow-spaces-newlines-and-tabs ()
  "skk-allow-spaces-newlines-and-tabs をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-allow-spaces-newlines-and-tabs
   '((skk-allow-spaces-newlines-and-tabs . t))
   '((skk-allow-spaces-newlines-and-tabs . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-convert-okurigana-into-katakana ()
  "skk-convert-okurigana-into-katakana をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-convert-okurigana-into-katakana
   '((skk-convert-okurigana-into-katakana . t))
   '((skk-convert-okurigana-into-katakana . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-delete-okuri-when-quit ()
  "skk-delete-okuri-when-quit をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-delete-okuri-when-quit
   '((skk-delete-okuri-when-quit . t))
   '((skk-delete-okuri-when-quit . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-echo ()
  "skk-echo をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-echo
   '((skk-echo . t))
   '((skk-echo . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-use-numeric-conversion ()
  "skk-use-numeric-conversion をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-use-numeric-conversion
   '((skk-use-numeric-conversion . t))
   '((skk-use-numeric-conversion . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-use-overlay ()
  "skk-use-face をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-use-face
   '((skk-use-face . t))
   '((skk-use-face . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-auto-insert-paren ()
  "skk-auto-insert-paren をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-auto-insert-paren
   '((skk-auto-insert-paren . t))
   '((skk-auto-insert-paren . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-japanese-message-and-error ()
  "skk-japanese-message-and-error をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-japanese-message-and-error
   '((skk-japanese-message-and-error . t))
   '((skk-japanese-message-and-error . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
;;(defun skk-menu-byte-compile-init-file ()
;;  "skk-byte-compile-init-file をスイッチオン/オフする。"
;;  (interactive)
;;  (skk-menu-change-user-option
;;   'skk-byte-compile-init-file
;;   '((skk-byte-compile-init-file . t))
;;   '((skk-byte-compile-init-file . nil)) )
;;  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-count-private-jisyo-entries-exactly ()
  "skk-count-private-jisyo-candidates-exactly をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-count-private-jisyo-candidates-exactly
   '((skk-count-private-jisyo-candidates-exactly . t))
   '((skk-count-private-jisyo-candidates-exactly . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-auto-henkan ()
  "skk-auto-start-henkan をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-auto-start-henkan
   '((skk-auto-start-henkan . t)
     (skk-auto-okuri-process . t) )
   '((skk-auto-start-henkan . nil)) )
  (skk-set-cursor-properly) )

;; for skk-comp.el
;;;###skk-autoload
(defun skk-menu-dabbrev-like-completion ()
  "skk-dabbrev-like-completion をスイッチオン/オフする。"
  (interactive)
  (require 'skk-comp)
  (skk-menu-change-user-option
   'skk-dabbrev-like-completion
   '((skk-dabbrev-like-completion . t))
   '((skk-dabbrev-like-completion . nil)) )
  (skk-set-cursor-properly) )

;; for skk-gadget.el
;;;###skk-autoload
(defun skk-menu-date-ad ()
  "skk-date-ad をスイッチオン/オフする。"
  (interactive)
  (require 'skk-gadget)
  (skk-menu-change-user-option
   'skk-date-ad
   '((skk-date-ad . t))
   '((skk-date-ad . nil)) )
  (skk-set-cursor-properly) )

;; for skk-kakasi.el
;;;###skk-autoload
(defun skk-menu-romaji-*-by-hepburn ()
  "skk-romaji-*-by-hepburn をスイッチオン/オフする。"
  (interactive)
  (require 'skk-kakasi)
  (skk-menu-change-user-option
   'skk-romaji-*-by-hepburn
   '((skk-romaji-*-by-hepburn . t))
   '((skk-romaji-*-by-hepburn . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-use-kakasi ()
  "skk-use-kakasi をスイッチオン/オフする。"
  (interactive)
  (require 'skk-kakasi)
  (skk-menu-change-user-option
   'skk-use-kakasi
   '((skk-use-kakasi . t))
   '((skk-use-kakasi . nil)) )
  (skk-set-cursor-properly) )

;; for skk-num.el
;;;###skk-autoload
(defun skk-menu-numeric-conversion-float-num ()
  "skk-numeric-conversion-float-num をスイッチオン/オフする。"
  (interactive)
  (require 'skk-num)
  (skk-menu-change-user-option
   'skk-numeric-conversion-float-num
   '((skk-numeric-conversion-float-num . t))
   '((skk-numeric-conversion-float-num . nil)) )
  (skk-set-cursor-properly) )

;; for skk-server.el
;;;###skk-autoload
(defun skk-menu-report-server-response ()
  "skk-report-server-response をスイッチオン/オフする。"
  (interactive)
  (require 'skk-server)
  (skk-menu-change-user-option
   'skk-report-server-response
   '((skk-report-server-response . t))
   '((skk-report-server-response . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-server-debug ()
  "skk-server-debug をスイッチオン/オフする。"
  (interactive)
  (require 'skk-server)
  (skk-menu-change-user-option
   'skk-server-debug
   '((skk-server-debug . t))
   '((skk-server-debug . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-compare-jisyo-size-when-saving ()
  "skk-compare-jisyo-size-when-saving をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-compare-jisyo-size-when-saving
   '((skk-compare-jisyo-size-when-saving . t))
   '((skk-compare-jisyo-size-when-saving . nil)) )
  (skk-set-cursor-properly) )

;;;###skk-autoload
(defun skk-menu-use-color-cursor ()
  "skk-use-color-cursor をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-use-color-cursor
   '((skk-use-color-cursor . t))
   '((skk-use-color-cursor . nil)) )
  (skk-set-cursor-properly) )

(defun skk-menu-uniq-numerals ()
  "skk-uniq-numerals をスイッチオン/オフする。"
  (interactive)
  (skk-menu-change-user-option
   'skk-uniq-numerals
   '((skk-uniq-numerals . t))
   '((skk-uniq-numerals . nil)) ))

;;(defun skk-menu- ()
;;  "skk- をスイッチオン/オフする。"
;;  (interactive)
;;  (skk-menu-change-user-option
;;   'skk-
;;   '((skk- . t))
;;   '((skk- . nil)) ))

;;; skk-menu.el ends here
