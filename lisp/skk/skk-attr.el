;; -*-byte-compile-dynamic: t;-*-
;;; skk-attr.el --- SKK 単語属性メンテナンスプログラム
;; Copyright (C) 1997 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-attr.el,v 1.1 1997/12/02 08:48:37 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:37 $

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

;;; Code:
(eval-when-compile (require 'skk))

;;;###skk-autoload
(defvar skk-attr-file (if (eq system-type 'ms-dos) "~/_skk-attr" "~/.skk-attr")
  "*SKK の単語の属性を保存するファイル。")

;;;###skk-autoload
(defvar skk-attr-backup-file
  (if (eq system-type 'ms-dos) "~/_skk-attr.BAK" "~/.skk-attr.BAK" )
  "*SKK の単語の属性を保存するファイル。")

;;;###skk-autoload
(defvar skk-attr-search-function nil
  "*skk-search-jisyo-file が候補を見つけたときにコールされる関数。
見出し語、送り仮名、エントリーの 3 引数を伴なって、
skk-attr-default-update-function がコールされた後にコールされる。" )

;;;###skk-autoload
(defvar skk-attr-default-update-function
  (function (lambda (midasi okurigana word purge)
              (or skk-attr-alist (skk-attr-read))
              (if purge
                  (skk-attr-purge midasi okurigana word)
                ;; time 属性に current-time の返り値を保存する。                
                (skk-attr-put midasi okurigana word 'time (current-time)) )))
  "*skk-search-jisyo-file が候補を見つけたときにコールされる関数。
見出し語、送り仮名、エントリーの 3 引数を伴なって、
skk-attr-default-update-function がコールされる前にコールされる。" )

;;;###skk-autoload
(defvar skk-attr-update-function nil
  "*skk-update-jisyo の中でコールされる関数。
見出し語、送り仮名、エントリー、パージの 4 引数を伴なってコールされる。" )

;;;###skk-autoload
(defvar skk-attr-alist nil
  "SKK 属性を展開するエーリスト。" )

;; data structure
;; とりあえず、各変換毎に属性の更新を行ない易いように、見出し語から各属性を引
;; 出し易いようにする。こうやってしまうと、ある属性を持つ単語を抜き出すのが面
;; 倒になるが、止むを得ないか...。
;;
;; 高速化のために 2 つのハッシュキーを持つようにする。1 つは okuri-ari か
;; okuri-nasi か。2 つめは見出し語の先頭の文字。
;;
;; '((okuri-ari . (("あ" . ("あt" .
;;                          ("当" . (okurigana . ("た" "て"))
;;                                  (time . (13321 10887 982100))
;;                                  (anything . ...) )
;;                          ("合" . (okurigana . ("って" "った"))
;;                                  (time . (13321 10953 982323)
;;                                  (anything . ...) )
;;                          ("会" . (okurigana . ("って"))
;;                                  (time . (13321 10977 312335))
;;                                  (anything . ...) ))
;;                         ("あつかw" . ...) )
;;                 ("い" . ...) )
;;   (okuri-nasi . (("あ" . ...) ("い" . ...))) )
;; 
;; しかし、こういうものを作ると、.skk-jisyo と .skk-attr の両方を持つ意味が薄
;; れてしまうんだよね...。上手く動けば .skk-attr に統合しても良いけど、辞書の
;; メンテナンスが面倒になるか...。

(defsubst skk-attr-get-table (okuri-ari)
  (assq (if okuri-ari 'okuri-ari 'okuri-nasi) skk-attr-alist) )

(defsubst skk-attr-get-table-for-midasi (midasi okurigana)
  ;; get all entries for MIDASI.
  ;; e.g.
  ;;  ("あt" . ("当" . (okurigana . ("た" "て"))
  ;;                   (time . (13321 10887 982100))
  ;;                   (anything . ...) )
  ;;           ("合" . (okurigana . ("って" "った"))
  ;;                   (time . (13321 10953 982323)
  ;;                   (anything . ...) )
  ;;           ("会" . (okurigana . ("って"))
  ;;                   (time . (13321 10977 312335))
  ;;                   (anything . ...) ))
  (assoc midasi (cdr (assoc (skk-substring-head-character midasi)
                            (cdr (skk-attr-get-table okurigana)) ))))

(defsubst skk-attr-get-table-for-word (midasi okurigana word)
  ;; get a table for WORD.
  ;; e.g.
  ;;  ("当" . (okurigana . ("た" "て")) (time . (13321 10887 982100))
  ;;          (anything . ...) )
  (assoc word (cdr (skk-attr-get-table-for-midasi midasi okurigana))) )

(defsubst skk-attr-get-all-attrs (midasi okurigana word)
  ;; get all attributes for MIDASI and WORD.
  ;; e.g.
  ;; ((okurigana . "た" "て") (time . (13321 10887 982100)) (anything . ...))
  (cdr (skk-attr-get-table-for-word midasi okurigana word)) )

(defsubst skk-attr-get (midasi okurigana word name)
  (assq name (skk-attr-get-all-attrs midasi okurigana word)) )
  
(defun skk-attr-put (midasi okurigana word name attr)
  ;; add attribute ATTR for MIDASI, WORD and NAME.
  ;; e.g.
  ;; table := ("あt" . ("当" . (okurigana . ("た" "て"))
  ;;                           (time . (13321 10887 982100))
  ;;                           (anything . ...) )
  ;;                   ("合" . (okurigana . (("って" "った"))
  ;;                           (time . (13321 10953 982323))
  ;;                           (anything . ...) )
  ;;                   ("会" . (okurigana . ("って"))
  ;;                           (time . (13321 10977 312335))
  ;;                           (anything . ...) ))
  ;; entry := ("当" . (okurigana . ("た" "て")) (time . (13321 10887 982100))
  ;;                  (anything . ...) )
  ;; oldattr := (time . (13321 10887 982100))
  ;;
  (let* ((table (skk-attr-get-table-for-midasi midasi okurigana))
         (entry (assoc word (cdr table)))
         (oldattr (assq name (cdr entry))) )
    (cond (oldattr
           (cond ((eq name 'okurigana) ; anything else?
                  (setcdr oldattr (cons attr (delete attr (nth 1 oldattr)))) )
                 (t (setcdr oldattr attr)) ))
          (entry (setcdr entry (cons (cons name attr) (cdr entry))))
          ;; new entry
          (t (skk-attr-put-1 midasi okurigana word name attr) ))))

(defun skk-attr-put-1 (midasi okurigana word name attr)
  ;; header := "あ"
  ;; table := ((okuri-ari . (("あ" . ("あt" .
  ;;                            ("当" . (okurigana . ("た" "て"))
  ;;                                    (time . (13321 10887 982100))
  ;;                                    (anything . ...) )
  ;;                            ("合" . (okurigana . ("って" "った"))
  ;;                                    (time . (13321 10953 982323))
  ;;                                    (anything . ...) )
  ;;                            ("会" . (okurigana . ("って"))
  ;;                                    (time . (13321 10977 312335))
  ;;                                    (anything . ...) )))
  ;; table2 := ("あ" . ("あt" .
  ;;                            ("当" . (okurigana . ("た" "て"))
  ;;                                    (time . (13321 10887 982100))
  ;;                                    (anything . ...) )
  ;;                            ("合" . (okurigana . ("って" "った"))
  ;;                                    (time . (13321 10953 982323)
  ;;                                    (anything . ...) )
  ;;                            ("会" . (okurigana . ("って"))
  ;;                                    (time . (13321 10977 312335))
  ;;                                    (anything . ...) )))
  (let* ((table (skk-attr-get-table okurigana))
         (header (skk-substring-head-character midasi))
         (table2 (assoc header (cdr table)))
         (add (cons midasi (list
                            (cons word
                                  (if okurigana
                                      ;; default attribute for okuri-ari
                                      (list (cons 'okurigana (list okurigana))
                                            ;; default attribute
                                            ;;(cons 'midasi midasi)
                                            ;; and new one
                                            (cons name attr) )
                                    (list
                                     ;; default attribute
                                     ;;(cons 'midasi midasi)
                                     ;; and new one
                                     (cons name attr) )))))))
    (cond (table2
           ;; header あり
           (setcdr table2 (cons add (cdr table2))) )
          ;; header なし
          ((cdr table)
           (setcdr table (cons (cons header (list add)) (cdr table))) )
          (t (setcdr table (list (cons header (list add))))) )))

(defun skk-attr-remove (midasi okurigana word name)
  ;; delete attribute ATTR for MIDASI, WORD and NAME.
  ;; e.g.
  ;; attrs := ((okurigana . ("た" "て")) (time . (13321 10887 982100))
  ;;           (anything . ...) )
  ;; del := (time . (13321 10887 982100))
  ;;
  (let* ((table (skk-attr-get-all-attrs midasi okurigana word))
         (del (assq name table)) )
    (and del (setq table (delq del table))) ))

;;;###skk-autoload
(defun skk-attr-purge (midasi okurigana word)
  ;; purge a whole entry for MIDASI and WORD.
  (let* ((table (cdr (skk-attr-get-table-for-midasi midasi okurigana)))
         (del (assoc word table)) )
    (and del (setq del (delq del table))) ))
    
;;;###skk-autoload
(defun skk-attr-read (&optional nomsg)
  "skk-attr-file から属性を読み込む。"
  (interactive "P")
  (skk-create-file
   skk-attr-file
   (if (not nomsg)
       (if skk-japanese-message-and-error
           "SKK の属性ファイルを作りました"
         "I have created an SKK attributes file for you" )))
  (if (or (null skk-attr-alist)
          (skk-yes-or-no-p (format "%s を再読み込みしますか？" skk-attr-file)
                           (format "Reread %s?" skk-attr-file) ))
      (let (;;(coding-system-for-read 'euc-japan)
            enable-character-unification )
        (save-excursion
          (unwind-protect
              (progn
                (set-buffer (get-buffer-create " *SKK attr*"))
                (erase-buffer)
                (if (= (nth 1 (insert-file-contents skk-attr-file)) 0)
                    ;; bare alist
                    (insert "((okuri-ari) (okuri-nasi))") )
                (goto-char (point-min))
                (or nomsg
                    (skk-message "%s の SKK 属性を展開しています..."
                                 "Expanding attributes of %s ..."
                                 (file-name-nondirectory skk-attr-file) ))
                (setq skk-attr-alist (read (current-buffer)))
                (or nomsg
                    (skk-message
                     "%s の SKK 属性を展開しています...完了！"
                     "Expanding attributes of %s ...done"
                     (file-name-nondirectory skk-attr-file) )))
	    (message "%S" (current-buffer))
	    ;; Why?  Without this line, Emacs 20 deletes the
	    ;; buffer other than skk-attr's buffer.
            (kill-buffer (current-buffer)) ))
        skk-attr-alist )))

;;;###skk-autoload
(defun skk-attr-save (&optional nomsg)
  "skk-attr-file に属性を保存する."
  (interactive "P")
  (if (and (null skk-attr-alist) (not nomsg))
      (progn
        (skk-message "SKK 属性をセーブする必要はありません"
                     "No SKK attributes need saving" )
        (sit-for 1) )
    (save-excursion
      (if (not nomsg)
          (skk-message "%s に SKK 属性をセーブしています..."
                       "Saving SKK attributes to %s..." skk-attr-file ))
      (and skk-attr-backup-file
           (copy-file skk-attr-file skk-attr-backup-file
                      'ok-if-already-exists 'keep-date ))
      (set-buffer (find-file-noselect skk-attr-file))
      (if skk-mule3
          (progn
            (if (not (coding-system-p 'iso-2022-7bit-short))
                (make-coding-system
                 'iso-2022-7bit-short
                 2 ?J
                 "Like `iso-2022-7bit' but no ASCII designation before SPC."
                 '(ascii nil nil nil t t nil t) ))
            (set-buffer-file-coding-system 'iso-2022-7bit-short) ))
      (delete-region 1 (point-max))
      ;; This makes slow down when we have a long attributes alist, but good
      ;; for debugging.
      (if skk-debug (pp skk-attr-alist (current-buffer))
	(prin1 skk-attr-alist (current-buffer)) )
      (write-file skk-attr-file)
      (kill-buffer (current-buffer))
      (if (not nomsg)
          (skk-message "%s に SKK 属性をセーブしています...完了！"
                       "Saving attributes to %s...done" skk-attr-file )))))

;;(defun skk-attr-mapc (func seq)
;;  ;; funcall FUNC every element of SEQ.
;;  (let (e)
;;    (while (setq e (car seq))
;;      (setq seq (cdr seq))
;;      (funcall func e) )))
;;
;;(defun skk-attr-get-all-entries (okuri-ari)
;;  ;; remove hash tables of which key are headchar and midasi, and return all
;;  ;; entries.
;;  (let ((table (skk-attr-get-table okuri-ari))
;;        minitable val entry )
;;    (while table
;;      (setq minitable (cdr (car table)))
;;      (while minitable
;;        (setq val (cons (car (cdr minitable)) val)
;;              minitable (cdr minitable) ))
;;      (setq table (cdr table)) )
;;    val ))
    
;;;###skk-autoload
(defun skk-attr-purge-old-entries ()
  "直近の 30 日間アクセスがなかったエントリを個人辞書からパージする。"
  (interactive)
  (let ((table (cdr (skk-attr-get-table 'okuri-ari)))
        (oldday (skk-attr-relative-time (current-time) -2592000)) )
    (skk-attr-purge-old-entries-1 table oldday)
    (setq table (cdr (skk-attr-get-table nil)))
    (skk-attr-purge-old-entries-1 table oldday) ))

(defun skk-attr-purge-old-entries-1 (table oldday)
  ;; 30 days old
  (let (skk-henkan-okuri-strictly
        skk-henkan-strict-okuri-precedence
        skk-henkan-key
        skk-henkan-okurigana ;; have to bind it to nil
        skk-okuri-char
        skk-search-prog-list ;; not to work skk-public-jisyo-contains-p.
        minitable )
    ;; こういうのをもっと一般的に処理できるマクロ (関数でも良いけど) でも考え
    ;; なきゃならんな...
    (while table
      (setq minitable (cdr (car table)))
      (while minitable
        (setq minimini (cdr (car minitable)))
        (while minimini
          (setq e (car minimini))
          (if (skk-attr-time-lessp (cdr (assq 'time (cdr e))) oldday)
              (progn
                (setq skk-henkan-key (car (car minitable))
                      skk-okuri-char (substring skk-henkan-key -1)
                      ;; これじゃ消えないみたいね...。
                      minimini (delq e minimini) )
                (skk-update-jisyo (car e) 'purge) )
            (setq minimini (cdr minimini)) ))
        (setq minitable (cdr minitable)) )
      (setq table (cdr table)) )))

;; time utilities...
;;  from ls-lisp.el.  Welcome!
(defun skk-attr-time-lessp (time0 time1)
  (let ((hi0 (car time0))
	(hi1 (car time1))
	(lo0 (nth 1 time0))
	(lo1 (nth 1 time1)) )
    (or (< hi0 hi1) (and (= hi0 hi1) (< lo0 lo1))) ))

;; from timer.el.  Welcome!
(defun skk-attr-relative-time (time secs &optional usecs)
  ;; Advance TIME by SECS seconds and optionally USECS microseconds.
  ;; SECS may be a fraction.
  (let ((high (car time))
	(low (if (consp (cdr time)) (nth 1 time) (cdr time)))
	(micro (if (numberp (car-safe (cdr-safe (cdr time))))
		   (nth 2 time)
		 0)))
    ;; Add
    (if usecs (setq micro (+ micro usecs)))
    (if (floatp secs)
	(setq micro (+ micro (floor (* 1000000 (- secs (floor secs)))))))
    (setq low (+ low (floor secs)))

    ;; Normalize
    (setq low (+ low (/ micro 1000000)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536)))
    (setq low (logand low 65535))

    (list high low (and (/= micro 0) micro))))

;; from type-break.el.  Welcome!
(defun skk-attr-time-difference (a b)
  ;; Compute the difference, in seconds, between a and b, two structures
  ;; similar to those returned by `current-time'.
  ;; Use addition rather than logand since that is more robust; the low 16
  ;; bits of the seconds might have been incremented, making it more than 16
  ;; bits wide.
  ;;
  ;; elp.el version...maybe more precisely.
  ;;(+ (* (- (car end) (car start)) 65536.0)
  ;;   (- (nth 1 end) (nth 1 start))
  ;;   (/ (- (nth 2 end) (nth 2 start)) 1000000.0) )
  ;;
  (+ (lsh (- (car b) (car a)) 16)
     (- (nth 1 b) (nth 1 a)) ))

(add-hook 'skk-before-kill-emacs-hook 'skk-attr-save)

(provide 'skk-attr)
;;; skk-attr.el ends here
