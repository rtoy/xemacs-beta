;;; skk-num.el --- 数値変換のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-num.el,v 1.1 1997/12/02 08:48:38 steve Exp $
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

;;; Change log:

;; Following people contributed modifications to skk.el (Alphabetical order):
;;      Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;      Manabu Kawashima <kaw@lp.nm.fujitsu.co.jp>

;;; TODO
;; (1)skk-kanji-num-str2-subr のバグ修正。skk-kanji-num-str2-subr のコメント参照
;;    のこと。
;;
;; (2)skk-kanji-num-str3 の新設。

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)
(require 'cl)

;; user variables.
;;;###skk-autoload
(defvar skk-num-type-list
  '((?0 . identity)
    (?1 . skk-zenkaku-num-str)
    (?2 . skk-kanji-num-str)
    (?3 . skk-kanji-num-str2)
    ;;(?5 . skk-kanji-num-str3) ; 準備中
    (?4 . skk-recompute-numerals)
    (?9 . skk-shogi-num-str) )
  "*数字の変換のための、インデクスと変換に使用する関数とのドットペアのリスト。
各要素は、(数字の char-type . 関数名) という構成になっている。
car 部分は、例えば、見出し語が \"平成#1年\" のとき、# 記号の直後に表示される数
字 \"1\" を char-type で表わしたものを代入する。")

(defvar skk-numeric-conversion-float-num nil
  "*Non-nil であれば、浮動小数点数を使った見出し語に対応して変換を行なう。
この値を non-nil にすることで、\"#.# /#1．#1/#0月#0日/\" などの辞書見出しが使用
できなくなるので、注意。" )

;;;###skk-autoload
(defvar skk-uniq-numerals (or (assq ?4 skk-num-type-list)
                                  (and (assq ?2 skk-num-type-list)
                                       (assq ?3 skk-num-type-list) ))
  "*Non-nil であれば、異なる数値表現でも変換結果が同じ数値を重複して出力しない。" )

(defvar skk-num-load-hook nil
  "*skk-num.el をロードした後にコールされるフック。" )

;; internal constants and variables
(defconst skk-num-alist-type1
  '((?0 . "０") (?1 . "１") (?2 . "２") (?3 . "３")
    (?4 . "４") (?5 . "５") (?6 . "６") (?7 . "７")
    (?8 . "８") (?9 . "９")
    (?. . "．") ; 小数点。(?. . ".") の方が良い人もいるかも...。
    (?  . "") )
  "ascii 数字の char type と全角数字の string type の連想リスト。
\"1995\" -> \"１９９５\" のような文字列の変換を行う際に利用する。" )

(defconst skk-num-alist-type2
  '((?0 . "〇") (?1 . "一") (?2 . "二") (?3 . "三")
    (?4 . "四") (?5 . "五") (?6 . "六") (?7 . "七")
    (?8 . "八") (?9 . "九") (?  . "") )
  "ascii 数字の char type と漢数字の string type の連想リスト。
\"1995\" -> \"一九九五\" のような文字列の変換を行う際に利用する。" )

;;; 準備中
;;;(defconst skk-num-alist-type3
;;;  '((?1 . "壱") (?2 . "弐") (?3 . "参")
;;;    (?4 . "四") (?5 . "伍") (?6 . "六") (?7 . "七")
;;;    (?8 . "八") (?9 . "九") (?  . "") )
;;;  "ascii 数字の char type と漢数字の string type の連想リスト。
;;;\"1995\" -> \"壱阡九百九拾伍\" のような文字列の変換を行う際に利用する。" )

;;;###skk-autoload
(skk-deflocalvar skk-num-list nil
  "skk-henkan-key の中に含まれる数字を表す文字列のリスト。
例えば、\"▽へいせい7ねん10がつ\" の変換を行うとき、skk-henkan-key は
\"へいせい7ねん10がつ\" であり、skk-num-list は \(\"7\" \"10\"\) となる。" )

;;;###skk-autoload
(skk-deflocalvar skk-recompute-numerals-key nil
  "#4 タイプのキーにより数値の再計算を行なったときの検索キー。" )

;;;###skk-autoload
(defun skk-compute-numeric-henkan-key (key)
  ;; KEY の中の連続する数字を現わす文字列を "#" に置き換えた文字列を返す。"12"
  ;; や "０９" など連続する数字を 1 つの "#" に置き換えることに注意。
  ;; 置き換えた数字を skk-num-list の中にリストの形で保存する。
  ;; 例えば、KEY が "へいせい7年12がつ" であれば、"へいせい#ねん#がつ"
  ;; と変換し、skk-num-list に ("7" "12") というリストを代入する。
  ;; 辞書の見出し語の検索に使用する。
  (let ((numberrep (if skk-numeric-conversion-float-num
                       "[.0-9]+" "[0-9]+" ))
        (enable-multibyte-characters t) )
    ;;(setq skk-noconv-henkan-key key)
    (save-match-data
      ;; 全角数字を ascii 数字に変換する。
      (while (string-match "[０-９]" key)
        (let ((zen-num (match-string 0 key)))
          (setq key (concat (substring key 0 (match-beginning 0))
                            (skk-jisx0208-to-ascii zen-num)
                            (substring key (match-end 0)) ))))
      ;; ascii 数字を "#" に置き換え、その数字を skk-num-list の中に保存。
      (while (string-match numberrep key)
        (setq skk-num-list (nconc skk-num-list (list (match-string 0 key)))
              key (concat (substring key 0 (match-beginning 0))
                          "#"
                          (substring key (match-end 0)) )))))
  key )

;;(defun skk-compute-noconv-henkan-key (key)
;;  ;; 文字列 KEY の中に数値変換を表わす "#" があれば、その部分を削除し、
;;  ;; skk-num-list の中で該当する数字を挿入し、最初に skk-start-henkan に渡され
;;  ;; た文字列を返す。例えば、skk-num-list が ("1" "2" "3") で、KEY が
;;  ;; "#がつ#がつ#がつ" であるときは、文字列 "1がつ2がつ3がつ" を返す。
;;  (if skk-num-list
;;      (save-match-data
;;        (let ((num-list skk-num-list)
;;              str )
;;          (while (and num-list key (string-match "#" key))
;;            (setq str (concat str (substring key 0 (match-beginning 0))
;;                              (car num-list) )
;;                  key (substring key (match-end 0))
;;                  num-list (cdr num-list) ))
;;          (setq key (concat str key)) )))
;;  key )

;;;###skk-autoload
(defun skk-numeric-convert (key)
  (if (not key)
      nil
    (let ((numexp (if skk-numeric-conversion-float-num
                      "#[.0-9]+" "#[0-9]+" ))
          (n 0)
          (workkey key)
          num convnum string convlist current )
      (save-match-data
        (while (and (setq num (nth n skk-num-list))
                    (string-match numexp workkey) )
          (setq convnum (skk-num-exp num (string-to-char
                                          (substring workkey
                                                     (1+ (match-beginning 0))
                                                     (match-end 0) )))
                string (substring workkey 0 (match-beginning 0))
                workkey (substring workkey (match-end 0))
                n (1+ n) )
          (if (not (and (stringp convnum) (string= convnum "")
                        (string= string "") ))
              (setq convlist (nconc convlist (list string convnum))) ))
        (setq convlist (nconc convlist (list workkey)))
        (cond ((null convlist) nil)
              ((and (null (cdr convlist)) (stringp (car convlist)))
               (setq current (car convlist)) )
              ;; RAW-LIST の全要素が文字列。
              ((null (memq t (mapcar 'listp convlist)))
               (setq current (mapconcat 'identity convlist ""))
               (if (and (> skk-henkan-count -1)
                        (nth skk-henkan-count skk-henkan-list) )
                   ;; ("A" "#2" "C") -> ("A" ("一" . "#2") "C")
                   (setf (nth skk-henkan-count skk-henkan-list)
                         (cons key current) )
                 (setq skk-henkan-list
                       (nconc skk-henkan-list (list (cons key current))) )))
              ;; #4
              (t (let ((l (mapcar (function (lambda (e) (cons key e)))
                                  (skk-flatten-list (delete "" convlist)) )))
                   (setq current (cdr (car l)))
                   (if (and (> skk-henkan-count -1)
                            (nth skk-henkan-count skk-henkan-list) )
                       (progn
                         (setf (nth skk-henkan-count skk-henkan-list) (car l))
                         (setq skk-henkan-list (skk-middle-list
                                                skk-henkan-list
                                                (1+ skk-henkan-count)
                                                (cdr l) )))
                     (setq skk-henkan-list (nconc skk-henkan-list l)) ))))
        current ))))

;;;###skk-autoload
(defun skk-numeric-convert*7 ()
  (let ((skk-henkan-count skk-henkan-count)
        (n 7) )
    (while (and (> n 0) (nth skk-henkan-count skk-henkan-list))
      (skk-numeric-convert (skk-get-current-candidate))
      (setq skk-henkan-count (1+ skk-henkan-count)
            n (1- n) ))
    (if skk-recompute-numerals-key
        (skk-uniq-numerals) )))

(defun skk-raw-number-to-skk-rep (string)
  (setq string (skk-raw-number-to-skk-rep-1
                string "[０-９][一九五三四七二八六]" "#9" 0 ))
  (setq string (skk-raw-number-to-skk-rep-1
                string "\\(^\\|[^#0-9]\\)\\([0-9]+\\)" "#0" 2 ))
  (setq string (skk-raw-number-to-skk-rep-1
                string "[０-９]+" "#1" 0 ))
  (setq string (skk-raw-number-to-skk-rep-1
                string "\\([一九五三四七二八六十][十百千万億兆京]\\)+" "#3" 0 ))
  ;; (mapcar 'char-to-string
  ;;         (sort
  ;;          '(?一 ?二 ?三 ?四 ?五 ?六 ?七 ?八 ?九 ?〇) '<))
  ;;   --> ("〇" "一" "九" "五" "三" "四" "七" "二" "八" "六")
  ;;
  ;; [〇-九] という正規表現が使えないので、生のままつっこんでおく。
  (skk-raw-number-to-skk-rep-1 string "[〇一九五三四七二八六]+" "#2" 0))

(defun skk-raw-number-to-skk-rep-1 (string key type place)
  (let ((enable-multibyte-characters t))
    (save-match-data
      (while (string-match key string)
        (setq string (concat (substring string 0 (match-beginning place))
                             type
                             (substring string (match-end place)) )))
    string )))
  
(defun skk-flatten-list (list)
  ;; 与えられたリストの各要素から組み合せ可能な文字列の連接を作り、リストで返
  ;; す。
  ;; (("A" "B") "1" ("X" "Y")) -> ("A1X" "A1Y" "B1X" "B1Y")
  (do ((result
        (if (atom (car list)) (list (car list)) (car list))
        (mapcan (function
                 (lambda (a)
                   (mapcar (function (lambda (b) (concat a b)))
                           (if (atom (car tail)) (list (car tail))
                             (car tail) ))))
                result ))
       (tail (cdr list) (cdr tail)) )
      ((null tail) result) ))

(defun skk-num-exp (num type)
  ;; ascii 数字の NUM を TYPE に従い変換し、変換後の文字列を返す。
  ;; TYPE は下記の通り。
  ;; 0 -> 無変換
  ;; 1 -> 全角数字へ変換
  ;; 2 -> 漢数字へ変換
  ;; 3 -> 漢数字へ変換 (位取りをする)
  ;; 4 -> その数字そのものをキーにして辞書を再検索
  ;; 9 -> 将棋で使用する数字 ("３四" など) に変換
  (let ((fun (cdr (assq type skk-num-type-list))))
    (if fun (funcall fun num)) ))

(defun skk-zenkaku-num-str (num)
  ;; ascii 数字の NUM を全角数字の文字列に変換し、変換後の文字列を返す。
  ;; 例えば "45" を "４５" に変換する。
  (let ((candidate
         (mapconcat (function (lambda (c) (cdr (assq c skk-num-alist-type1))))
                    num "" )))
    (if (not (string= candidate ""))
        candidate )))

(defun skk-kanji-num-str (num)
  ;; ascii 数字 NUM を漢数字の文字列に変換し、変換後の文字列を返す。
  ;; 例えば、"45" を "四五" に変換する。
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
        (let ((candidate
               (mapconcat (function (lambda (c)
                                      (cdr (assq c skk-num-alist-type2)) ))
                          num "" )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-kanji-num-str2 (num)
  ;; ascii 数字 NUM を漢数字の文字列に変換し (位取りをする)、変換後の文字列を
  ;; 返す。例えば "1021" を "千二十一" に変換する。
  (save-match-data
    (if (not (string-match "\\.[0-9]" num))
        (let ((str (skk-kanji-num-str2-subr num)))
          (if (string= "" str) "〇" str) ))))

(defun skk-kanji-num-str2-subr (num)
  ;; skk-kanji-num-str2 のサブルーチン。
  ;;
  ;; Known Bug; ▽ 100000000 を変換すると、"一億万" になってしまう...。でもそん
  ;; な変換を使う人はいないかな、と思うと直す気力が湧かない...。
  ;; --> Fixed のハズ...。
  (let ((len (length num))
        prevchar modulo )
    (mapconcat
     (function
      (lambda (char)
        ;; 位:     一   十    百     千  万   十万   百万   千万    億
        ;; modulo: 1 --> 2 --> 3 --> 0 -> 1 --> 2 ---> 3 ---> 0 ---> 1
        (setq modulo (mod len 4))
        (prog1
            (if (eq len 1)
                ;; 1 桁で 0 でない数。
                (if (not (eq char ?0))  ;?0
                    ;; 位を表わす漢数字以外の漢数字。
                    (cdr (assq char skk-num-alist-type2)) )
              (concat
               ;; 位を表わす漢数字以外の漢数字。
               (if (or
                    ;; 2 桁以上で、この位の数は 0, 1 以外の数字。
                    ;; ?0 == 48, ?1 == 49
                    (null (memq char '(?0 ?1)))
                    ;; 2 桁以上で、この位の数は 1 で、位がその位を表わす漢数字
                    ;; に "一" を併記すべき (例えば、"一億" など。"億" ではお
                    ;; かしい) とき。
                    (and (eq char ?1) (eq modulo 1)) )
                   (cdr (assq char skk-num-alist-type2)) )
               ;; 位を表わす漢数字。
               (if (and (not (eq prevchar ?0))
                        (not (and (eq char ?0) (not (eq modulo 1))) ))
                   (cond ((cdr (assq modulo '((2 . "十") (3 . "百") (0 . "千")))))
                         ((cdr (assq len '((5 . "万") (9 . "億") (13 . "兆")
                                           (17 . "京") ))))
                         (t (skk-error "桁が大きすぎます！"
                                       "Too big number!" ))))))
          (setq len (1- len)
                prevchar char ) )))
     num "" )))

(defun skk-shogi-num-str (num)
  ;; ascii 数字の NUM を将棋で使用される数字表記に変換する。
  ;; 例えば "34" を "３四" に変換する。
  (save-match-data
    (if (and (eq (length num) 2)
             (not (string-match "\\.[0-9]" num)) )
        (let ((candidate
               (concat (cdr (assq (aref num 0) skk-num-alist-type1))
                       (cdr (assq (aref num 1) skk-num-alist-type2)) )))
          (if (not (string= candidate ""))
              candidate )))))

(defun skk-recompute-numerals (num)
  ;; #4 の見出しに対し、skk-henkan-key に代入された数字そのものを再度検索する。
  (let (result)
    ;; with-temp-buffer だと何故上手くゆかない...？ 確定されてしまう。
    ;;(with-temp-buffer
    (save-excursion
      (set-buffer (get-buffer-create " *skk-work*"))
      ;; カレントバッファのバッファローカル変数に影響を及ぼさないよう、ワーキ
      ;; ングバッファへ一旦逃げる
      (let ((skk-current-search-prog-list skk-search-prog-list)
            (skk-henkan-key num)
            skk-henkan-okurigana skk-okuri-char skk-use-numeric-conversion )
        ;; カレントの変換は送りなし (skk-henkan-okurigana と skk-okuri-char は
        ;; いずれも nil) だが、別バッファ (work バッファ) に入っているので、念
        ;; のため、nil を入れておく。
        (while skk-current-search-prog-list
          (setq result (skk-nunion result (skk-search))) )))
    ;; ここで with-temp-buffer を出て変換を行なっているカレントバッファに戻る
    ;; (バッファローカル値である skk-henkan-list を操作したいため)。
    (setq skk-recompute-numerals-key num)
    (if result
        (if (null (cdr result)) ;;(eq (length result) 1)
            (car result)
          result )
      ;; 変換できなかったら元の数字をそのまま返しておく。
      num )))

;;;###skk-autoload
(defun skk-uniq-numerals ()
  (if (or (not skk-uniq-numerals) (null skk-henkan-list))
      nil
    (save-match-data
      (let ((n1 -1) n2 e1 e2 e3
            ;; 1 つでも 2 桁以上の数字があれば、#2 と #3 では uniq しない。
            (type2and3 (> 2 (apply 'max (mapcar 'length skk-num-list))))
            type2 type3 index2 index3 head2 head3 tail2 tail3
            kanji-flag mc-flag enable-multibyte-characters case-fold-search )
        (while (setq n1 (1+ n1) e1 (nth n1 skk-henkan-list))
          ;; cons cell でなければ skk-nunion で処理済みなので、重複はない。
          (if (consp e1)
              ;; (car e1) と equal のものが消えるのだから e1 自身が消えるこ
              ;; とはない。
              (setq skk-henkan-list (delete (car e1) skk-henkan-list)
                    skk-henkan-list (delete (cdr e1) skk-henkan-list) ))
          (if (not (and skk-recompute-numerals-key (consp e1)))
              nil
            ;; ("#4" . "xxx") を含む候補が skk-henkan-list の中にある。
            (setq n2 -1)
            (while (setq n2 (1+ n2) e2 (nth n2 skk-henkan-list))
              (if (and (not (= n1 n2)) (consp e2)
                       ;; 例えば ("#4" . "一") と ("#2" . "一") が並存してい
                       ;; る場合。
                       (string= (cdr e1) (cdr e2)) )
                  (setq skk-henkan-list (delq e2 skk-henkan-list)) )))
          (if (not type2and3)
              nil
            ;; 1 桁の数字を変換する際に、skk-henkan-list に #2 エントリと #3
            ;; エントリがあれば、#2 もしくは #3 エントリのうち、より後方にある
            ;; ものを消す。
            (setq e3 (if (consp e1) (car e1) e1))
            ;; e3 は "#2" のように数値変換を示す文字列のみとは限らないので、
            ;; member は使えない。
            (cond ((string-match "#2" e3)
                   (setq type2 e1
                         index2 n1
                         head2 (substring e3 0 (match-beginning 0))
                         tail2 (substring e3 (match-end 0)) ))
                  ((string-match "#3" e3)
                   (setq type3 e1
                         index3 n1
                         head3 (substring e3 0 (match-beginning 0))
                         tail3 (substring e3 (match-end 0)) )))))
        (if (and type2and3 type2 type3
                 ;; 数値変換を示す文字列 "#[23]" の前後の文字列も同一のと
                 ;; きのみ uniq を行なう。
                 (string= head2 head3) (string= tail2 tail3))
            (if (> index2 index3)
                ;; "#3" の方が前にある。
                (setq skk-henkan-list (delq type2 skk-henkan-list))
              ;; 変数 type[23] の値は、skk-henkan-list から直接抽出したも
              ;; のだから delete でなく、delq で十分。
              (setq skk-henkan-list (delq type3 skk-henkan-list)) ))))))

;;;###skk-autoload
(defun skk-adjust-numeric-henkan-data (key)
  (let (numexp orglen val)
    (if (or (and (string-match "#[012349]" key)
                 (setq numexp key) )
            (and (setq numexp (skk-raw-number-to-skk-rep key))
                 (not (string= key numexp)) ))
        (progn
          (setq orglen (length skk-henkan-list)
                ;; skk-henkan-list の調整は、skk-numeric-convert の中で行なっ
                ;; てくれる。
                val (skk-numeric-convert numexp) )
          (if (= (length skk-henkan-list) (1+ orglen))
              ;; #4 で複数の候補に変換できた場合は確定しない。
              (setq skk-kakutei-flag t) ))
      (setq skk-henkan-list (nconc skk-henkan-list (list key))
            skk-kakutei-flag t
            val key ))
    val ))

;;;###skk-autoload
(defun skk-init-numeric-conversion-variables ()
  ;; skk-use-numeric-convert 関連の変数を初期化する。
  (setq skk-num-list nil
        skk-recompute-numerals-key nil ))

;;;###skk-autoload
(defun skk-numeric-midasi-word ()
  ;; type4 の数値再変換が行なわれたときは、数値自身を返し、それ以外の数値変換
  ;; では、skk-henkan-key を返す。こんな小さな関数を作らなきゃならないのは、
  ;; skk-use-numeric-conversion に関連する変数を skk-num.el に集約した無理が出
  ;; た結果か...。
  (or skk-recompute-numerals-key skk-henkan-key) )

;;;###skk-autoload
(defun skk-update-jisyo-for-numerals (noconvword word &optional purge)
  ;; 数字自身を見出し語として辞書のアップデートを行なう。
  (if (and skk-recompute-numerals-key
           (save-match-data (string-match "#4" noconvword)) )
      (let ((skk-henkan-key skk-recompute-numerals-key))
	(message "%S" skk-recompute-numerals-key)
        (skk-update-jisyo word purge) )))

;;;###skk-autoload
(defun skk-num (str)
  ;; 数字を skk-number-style の値に従い変換する。
  ;; skk-date のサブルーチン。
  (mapconcat (function
	      (lambda (c)
		(cond ((or (not skk-number-style) (eq skk-number-style 0))
		       (char-to-string c) )
		      ((or (eq skk-number-style t) (eq skk-number-style 1))
		       (cdr (assq c skk-num-alist-type1)) )
		      (t (cdr (assq c skk-num-alist-type2))) )))
	     str "" ))

(run-hooks 'skk-num-load-hook)

(provide 'skk-num)
;;; skk-num.el ends here
