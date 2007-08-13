;; skk-gadget.el -- 実行変換のためのプログラム
;; Copyright (C) 1995, 1996, 1997 Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Murata Shuuichirou  <mrt@mickey.ai.kyutech.ac.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-gadget.el,v 1.1 1997/12/02 08:48:37 steve Exp $
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
;; Following people contributed to skk-gadget.el (Alphabetical order):
;;      Kazuo Hirokawa <hirokawa@rics.co.jp>
;;      Kiyotaka Sakai <ksakai@netwk.ntt-at.co.jp>
;;      Koichi MORI <kmori@onsei2.rilp.m.u-tokyo.ac.jp>
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;
;; プログラム実行変換とは
;; ======================
;;
;;
;; 送り仮名のない辞書の変換の候補に Emacs Lisp のコードが書いてあれば,
;; SKK はそのコードを Lisp のプログラムとして実行し, その結果の文字列を画
;; 面に挿入する. 例えば, 辞書に
;;
;;
;;         now /(current-time-string)/
;;
;; という行があるとき, 『`/now '』とタイプすれば画面には現在の時刻が
;; 表示され, 【▼Fri Apr 10 11:41:43 1992】のようになる. このような項目の
;; 登録は通常の辞書登録により行うことができる.
;;
;; ここで使える Lisp のコードは改行を含んでいないものに限られる. またこの
;; コードは結果として文字列を返すようなものでなければならない。
;;
;; このファイルは実行変換プログラムを集めたものである。

;;; Change log:
;; version 1.2.3 released 1997.2.4 (derived from the skk.el 8.6)

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)
;; -- user variables

;;;###skk-autoload
(defvar skk-date-ad nil
  "*Non-nil であれば、skk-today, skk-clock で西暦表示する。
nil であれば、元号表示する。" )

;;;###skk-autoload
(defvar skk-number-style 1
  "*nil もしくは 0 であれば、skk-today, skk-clock の数字を半角で表示する。
t もしくは、1 であれば、全角表示する。
t, 0, 1 以外の non-nil 値であれば、漢数字で表示する。" )

(defvar skk-gadget-load-hook nil
  "*skk-gadget.el をロードした後にコールされるフック。" )

;; --internal variables
(defconst skk-week-alist
  '(("Sun" . "日") ("Mon" . "月") ("Tue" . "火") ("Wed" . "水") ("Thu" . "木")
    ("Fri" . "金") ("Sat" . "土") )
  "曜日名の連想リスト。\(英語表記文字列 . 日本語表記文字列\)" )

;; -- programs
;;;###skk-autoload
(defun skk-date (&optional and-time)
  ;; 現在の日時を日本語で返す。skk-today と skk-clock のサブルーチン。
  ;; オプショナル引数の AND-TIME を指定すると、時間も返す。
  (let* ((str (current-time-string))
         (year (if skk-date-ad
                   (skk-num (substring str 20 24))
                 (let ((y (- (string-to-number (substring str 20 24)) 1988)))
                   (if (eq y 1) "元" (skk-num (int-to-string y))) )))
         (month (skk-num (cdr (assoc (substring str 4 7) skk-month-alist))))
         (day (substring str 8 10))
         (day-of-week (cdr (assoc (substring str 0 3) skk-week-alist)))
         hour minute second )
    (if (eq (aref day 0) 32) (setq day (substring day 1)))
    (setq day (skk-num day))
    (concat (if skk-date-ad "" "平成") year "年"
            month "月" day "日" "\(" day-of-week "\)"
            (if and-time
                (progn
                  (setq hour (skk-num (substring str 11 13))
                        minute (skk-num (substring str 14 16))
                        second (skk-num (substring str 17 19)) )
                  (concat " " hour "時" minute "分" second "秒") ))) ))

;;;###skk-autoload
(defun skk-today (&optional and-time)
  "インタラクティブに起動すると現在の日時を日本語表記でポイントに挿入する。
オプショナル引数の AND-TIME を指定すると、日時に加え、時間も挿入する。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。"
  (interactive "*P")
  (insert (skk-date and-time)) )

;;;###skk-autoload
(defun skk-clock (&optional kakutei-when-quit time-signal)
  "デジタル時計をミニバッファに表示する。
quit するとその時点の日時を候補として挿入する。
quit したときに起動してからの経過時間をミニバッファに表示する。
interactive に起動する他、\"clock /(skk-clock)/\" などのエントリを SKK の辞書
に加え、\"/clock\"+ SPC で変換することによっても起動可。C-g で止まる。
実行変換で起動した場合は、C-g した時点の時点の日時を挿入する。
オプショナル引数の KAKUTEI-WHEN-QUIT が non-nil であれば C-g したときに確
定する。
オプショナル引数の TIME-SIGNAL が non-nil であれば、NTT の時報風に ding する。
それぞれ、\"clock /(skk-clock nil t)/\" のようなエントリを辞書に挿入すれば良い。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。"
  (interactive "*")
  (let ((start (current-time-string))
        ;; Hit any key としたいところだが、何故か上手くゆかない (;_;)...。
        ;;(now-map (if skk-emacs19 
        ;;             '(keymap (t . keyboard-quit))
        ;;           (fillarray (make-keymap) 'keyboard-quit) ))
        (overriding-terminal-local-map
         (fillarray (setcar (cdr (make-keymap)) (make-vector 256 nil))
                    'keyboard-quit ))
        finish mes expr1 expr2 )
    (cond ((or (not skk-number-style)
               (eq skk-number-style 0) )
           (setq expr1 "[789]秒"
                 expr2 "0秒" ))
          ((or (eq skk-number-style t)
               ;; skk-number-style に 数字と t 以外の non-nil 値を入れている場
               ;; 合、= を使うと Wrong type argument: number-or-marker-p, xxxx
               ;; になってしまう。
               (eq skk-number-style 1) )
           (setq expr1 "[７８９]秒"
                 expr2 "０秒" ))
          (t
           (setq expr1 "[七八九]秒"
                 expr2 "〇秒" )))
    (save-match-data
      (condition-case nil
          (let (case-fold-search
                inhibit-quit visible-bell
                skk-mode skk-ascii-mode
                skk-j-mode skk-abbrev-mode skk-zenkaku-mode )
            (while (not quit-flag)
              (setq mes (skk-date t))
              ;;(message (concat  mes "    Hit C-g quit"))
              (message (concat  mes "    Hit any key to quit"))
              (if time-signal
                  (if (string-match expr1 mes)
                      ;; [7890] のように正規表現を使わず、7 だけで全てのマシンが
                      ;; 着いてゆけば良いのだが...。丁度この関数実行時に Garbage
                      ;; collection が呼ばれても表示される数字が飛ぶ場合がある。
                      (ding)
                    (if (string-match expr2 mes)
                        ;; 0 だけ「ポ〜ン」といきたいところですが、マシンによっ
                        ;; て差がある。
                        ;; 386SX 25Mhz + Mule-2.x だと「ピッ、ピッ」という感じ。
                        ;; 付いてゆくのが非常に辛い。68LC040 33Mhz + NEmacs だと
                        ;; 「ピピッ」となり、音のタイミングは良いのだが、とき
                        ;; どき 1 秒分ついていけなくなる。Pentium 90Mhz +
                        ;; Mule-2.xだと「ピッ」という単音になってしまう... (;_;)。
                        (progn (ding)(ding)) )))
              (sit-for 1) ))
        (quit
         (prog2
             (setq finish (current-time-string))
             (skk-date t)
           (if kakutei-when-quit
               (setq skk-kakutei-flag t) )
           (message (concat "経過時間 :" (skk-time-diff start finish))) ))))))

(defun skk-time-diff (start finish)
  ;; (current-time-string) の返り値 START と FINISH の時間差を求め、
  ;; "時間:分:秒" の形式で返す。skk-clock のサブルーチン。
  (let ((s-hour (string-to-number (substring start 11 13)))
        (s-minute (string-to-number (substring start 14 16)))
        (s-second (string-to-number (substring start 17 19)))
        (f-hour (string-to-number (substring finish 11 13)))
        (f-minute (string-to-number (substring finish 14 16)))
        (f-second (string-to-number (substring finish 17 19)))
        second-diff minute-diff hour-diff )
    (if (not (string= (substring start 20) (substring finish 20)))
        (skk-error "違う年の時間差は計算できません"
                   "Year should be same" ))
    (setq second-diff (- f-second s-second))
    (if (> 0 second-diff)
        (setq f-minute (1- f-minute)
              second-diff (- (+ f-second 60) s-second) ))
    (setq minute-diff (- f-minute s-minute))
    (if (> 0 minute-diff)
        (setq f-hour (1- f-hour)
              minute-diff (- (+ f-minute 60) s-minute) ))
    (setq hour-diff (- f-hour s-hour))
    (if (> 0 hour-diff)
        (skk-error "第２引数は第１引数より後の時間でなければなりません"
                   "2nd arg should be later than 1st arg" ))
    (format "%02d:%02d:%02d" hour-diff minute-diff second-diff) ))

;;;###skk-autoload
(defun skk-convert-ad-to-gengo (&optional fstr lstr)
  ;; 西暦を元号に変換する。オプション引数の fstr が指定されていれば、年号と
  ;; 数字の間に、lstr が指定されていれば、数字の末尾に、それぞれの文字列を連結
  ;; する。
  ;; 辞書見出し例;
  ;; せいれき#ねん /(skk-convert-ad-to-gengo nil "年")/(skk-convert-ad-to-gengo " " " 年")/
  (let ((ad (string-to-number (car skk-num-list))))
    (concat (cond ((>= 1866 ad)
                   (skk-error "分りません" "Unkown year") )
                  ((>= 1911 ad)
                   (concat "明治" fstr (int-to-string (- ad 1867))) )
                  ((>= 1925 ad)
                   (concat "大正" fstr (int-to-string (- ad 1911))) )
                  ((>= 1988 ad)
                   (concat "昭和" fstr (int-to-string (- ad 1925))) )
                  (t (concat "平成" fstr (int-to-string (- ad 1988)))) )
            lstr )))

;;;###skk-autoload
(defun skk-convert-gengo-to-ad (&optional string)
  ;; 元号を西暦に変換する。オプション引数の string が指定されていれば、
  ;; その文字列を末尾に連結する。
  ;; 辞書見出し例;
  ;; しょうわ#ねん /(skk-convert-gengo-to-ad "年")/(skk-convert-gengo-to-ad " 年")/
  (save-match-data
    (let ((num (car skk-num-list))
          gengo )
      (string-match num skk-henkan-key)
      (setq gengo (substring skk-henkan-key 0 (match-beginning 0))
            num (string-to-number num) )
      (concat (int-to-string
               (+ num
                  (cond ((eq num 0)
                         (skk-error "0 年はあり得ない"
                                    "Cannot convert 0 year" ))
                        ((string= gengo "へいせい") 1988)
                        ((string= gengo "しょうわ")
                         (if (> 64 num)
                             1925
                           (skk-error "昭和は 63 年までです" 
                                      "The last year of Showa is 63" )))
                        ((string= gengo "たいしょう")
                         (if (> 15 num)
                             1911
                           (skk-error "大正は、14 年までです"
                                      "The last year of Taisyo is 14" )))
                        ((string= gengo "めいじ")
                         (if (> 45 num)
                             1867
                           (skk-error "明治は、44 年までです"
                                      "The last year of Meiji is 44" )))
                        (t (skk-error "判別不能な元号です！"
                                      "Unknown Gengo!" )))))
              string ))))

;(defun skk-calc (operator)
;  ;; 2 つの引数を取って operator の計算をする。
;  ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
;  ;; skk-calc に渡す。
;  ;; 辞書見出し例; #*# /(skk-calc '*)/
;  (int-to-string
;   (funcall operator (string-to-number (car skk-num-list))
;            (string-to-number (nth 1 skk-num-list)) )))

;;;###skk-autoload
(defun skk-calc (operator)
  ;; 2 つの引数を取って operator の計算をする。
  ;; 注意: '/ は引数として渡せないので (defalias 'div '/) などとし、別の形で
  ;; skk-calc に渡す。
  ;; 辞書見出し例; #*# /(skk-calc '*)/
  (int-to-string (apply operator (mapcar 'string-to-number skk-num-list))) )

;;;###skk-autoload
(defun skk-plus ()
  ;; 辞書見出し例; #+#+# /(skk-plus)/
  (int-to-string
   (apply '+ (mapcar 'string-to-number skk-num-list))))

;;;###skk-autoload
(defun skk-minus ()
  (int-to-string
   (apply '- (mapcar 'string-to-number skk-num-list))))

;;;###skk-autoload
(defun skk-times ()
  (int-to-string
   (apply '* (mapcar 'string-to-number skk-num-list))))

;;;###skk-autoload
(defun skk-ignore-dic-word (&rest no-show-list)
  ;; 共用辞書に登録されている、違っている/気に入らない変換を出さないようにす
  ;; る。
  ;; 辞書見出し例;
  ;;   るすばん /留守番/(skk-ignore-dic-word "留守電")/
  ;;   かくてい /(skk-ignore-dic-word "確定")/
  (let (new-word save-okurigana)
    ;; skk-ignore-dic-word 自身のエントリを消す。消すべき候補は
    ;; skk-henkan-list から直接抽出しているので delete ではなく delq で十分。
    (setq skk-henkan-list (delq (nth skk-henkan-count skk-henkan-list)
                                skk-henkan-list ))
    ;; 全候補を skk-henkan-list に入れる。
    (while skk-current-search-prog-list
      (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))) )
    ;; 不要な候補を捨てる。
    (while no-show-list
      (setq skk-henkan-list (delete (car no-show-list) skk-henkan-list)
            no-show-list (cdr no-show-list) ))
    ;; カレントの候補 (skk-ignore-dic-word 自身のエントリ) を消したので、
    ;; skk-henkan-count は次の候補を指している。
    (setq new-word (or (nth skk-henkan-count skk-henkan-list)
                       (progn (setq save-okurigana skk-okuri-char)
                              (skk-henkan-in-minibuff) )))
    ;; 候補がないとき。
    (if (not new-word)
        ;; 空文字列が登録されたら辞書登録の前の状態に戻す。
        ;; (nth -1 '(A B C)) は、A を返すので、n が負の数でないことをチェック
        ;; しておく必要がある。
        (if (> skk-henkan-count 0)
            (setq skk-henkan-count (- skk-henkan-count 1)
                  new-word (nth skk-henkan-count skk-henkan-list) )
          ;; (1- skk-henkan-count) == -1 になる。▽モードに戻す。
          (setq new-word (if save-okurigana
                             (substring skk-henkan-key 0
                                        (1- (length skk-henkan-key)) )
                             skk-henkan-key )
                skk-henkan-count -1
                ;; 下記の変数は、skk-henkan-in-minibuff の中で調整される。
                ;; skk-henkan-active nil
                ;; skk-okuri-char nil
                ;; skk-henkan-okurigana nil
                  )
          (if skk-use-face
              (setq skk-insert-new-word-function
                    'skk-henkan-face-off-and-remove-itself ))))
    new-word ))

;;;###skk-autoload
(defun skk-henkan-face-off-and-remove-itself ()
  ;; skk-insert-new-word-function にセットするための関数。カレントバッファの
  ;; 変換部分が Overlay の face 属性によって表示が変更されているのを戻し、その
  ;; 後自分自身を skk-insert-new-word-function から取り除く自爆関数。
  (skk-henkan-face-off)
  (setq skk-insert-new-word-function nil) )

(run-hooks 'skk-gadget-load-hook)

(provide 'skk-gadget)
;;; skk-gadget.el ends here
