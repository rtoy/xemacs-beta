;;; skk-auto.el --- 送り仮名の自動処理のためのプログラム
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-auto.el,v 1.1 1997/12/02 08:48:37 steve Exp $
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
;; Following people contributed modifications to skk-server.el (Alphabetic
;; order):
;;
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>

;;; Change log:
;; version 1.0.6 released 1997.2.18 (derived from the skk.el 8.6)

;;; Code:
(require 'skk-foreword)
(require 'skk-vars)

;;; user variables
(defvar skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "x" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "*skk-remove-common で使用するかな文字からローマ字への変換ルール。
下記の該当するかな文字をその文字のローマ字プレフィックスで現わしたもの。
    ぁ  あ  ぃ  い  ぅ  う  ぇ  え  ぉ  お  か  が  き  ぎ  く  ぐ
    け  げ  こ  ご  さ  ざ  し  じ  す  ず  せ  ぜ  そ  ぞ  た  だ
    ち  ぢ  っ  つ  づ  て  で  と  ど  な  に  ぬ  ね  の  は  ば
    ぱ  ひ  び  ぴ  ふ  ぶ  ぷ  へ  べ  ぺ  ほ  ぼ  ぽ  ま  み  む
    め  も  ゃ  や  ゅ  ゆ  ょ  よ  ら  り  る  れ  ろ  ゎ  わ  ゐ
    ゑ  を  ん
それぞれのかな文字が送り仮名である場合にどのローマ字プレフィックスを対応させる
のかを指定することができる。「じ」、「ち」、「ふ」の文字について、対応するロー
マ字プレフィックスを \"z\", \"c\",\"f\" に変更を希望する場合もあるであろう。
skk-auto-okuri-process の値が non-nil のときのみ参照される。" )

(defvar skk-auto-load-hook nil
  "*skk-auto.el をロードした後にコールされるフック。" )

;; internal valriables
;;;###skk-autoload
(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "ミニバッファで辞書登録を行ったときにこのフラグが立つ。
skk-remove-common で参照される。" )

(skk-deflocalvar skk-okuri-index-min -1
  "skk-henkan-list のインデクスで自動送り処理で検索した最初の候補を指すもの。" )

(skk-deflocalvar skk-okuri-index-max -1
  "skk-henkan-list のインデクスで自動送り処理で検索した最後の候補を指すもの。" )

(defun skk-okuri-search ()
  ;; skk-auto-okuri-process が non-nil ならば "Uresii" のように送り仮名も含め
  ;; てタイプしても送りありの "嬉しい" を探し出す。
  (if (and skk-auto-okuri-process
           (not (or skk-abbrev-mode skk-process-okuri-early
                    skk-henkan-okurigana ))
           ;; we don't do auto-okuri-process if henkan key contains numerals.
           (not (skk-numeric-p))
           (> (length skk-henkan-key) skk-kanji-len) )
      (let (l)
        (setq skk-okuri-index-min (length skk-henkan-list)
              l (skk-okuri-search-subr)
              skk-okuri-index-max (+ skk-okuri-index-min (length l)) )
        l )))

(defun skk-okuri-search-subr ()
  ;; skk-okuri-search のサブルーチン。見つけたエントリのリストを返す。
  (let* ((henkan-key skk-henkan-key)
         (key (substring henkan-key 0 skk-kanji-len))
         (len (length henkan-key))
         (key1 (concat "\n" key))
         key2 len2 key3 len3 okuri3
         ;; 効率が良いように kanji-flag, mc-flag, enable-multibyte-characters
         ;; を nil にしておく。
         mc-flag
         ;; enable-multibyte-characters
         ;; case-fold-search は、辞書バッファでは常に nil。
         ;;case-fold-search
         (inhibit-quit t)
         key-cand-alist p q r s )
    (save-match-data
      (with-current-buffer (skk-get-jisyo-buffer skk-jisyo)
        (goto-char skk-okuri-ari-min)
        (while (search-forward key1 skk-okuri-ari-max t)
          (setq p (point)
                key2 (concat key (skk-buffer-substring
                                  p (- (search-forward " ") 2) ))
                len2 (length key2) )
          (if (not (and (<= len2 len)
                        (string= key2 (substring henkan-key 0 len2)) ))
              nil
            (let ((cont t))
              (skk-save-point
               (end-of-line)
               (setq q (point)) )
              (while (and cont (search-forward "/[" q t))
                (setq r (point))
                (setq okuri3 (skk-buffer-substring r (1- (search-forward "/")))
                      key3 (concat key2 okuri3)
                      len3 (length key3) )
                (if (not (and (<= len3 len)
                              (string= key3 (substring henkan-key 0 len3)) ))
                    nil
                  ;; finally found a candidate!
                  (let ((okuri
                         (concat okuri3 (substring henkan-key len3 len)) )
                        cand )
                    (while (not (eq (following-char) ?\]))
                      (setq cand
                            (concat
                             (skk-buffer-substring
                              (point)
                              (1- (search-forward "/" skk-okuri-ari-max t)) )
                             okuri ))
                      ;; 見出し語が違っても候補が同じことがあり得る。
                      ;;   かんz /感/[じ/感/]/
                      ;;   かんj /感/[じ/感/]/
                      ;; など。
                      (if (null (rassoc cand key-cand-alist))
                          (setq key-cand-alist (cons (cons key3 cand)
                                                     key-cand-alist ))))
                    ;; it is not necessary to seach for "\[" on this line
                    ;; any more
                    (setq cont nil) ))))))
        ;; key3 の長いもの順にソートして返す。
        (mapcar (function
                 (lambda (x) (cdr x)) )
                (sort (nreverse key-cand-alist)
                      (function (lambda (x y)
                                  (string< (car y) (car x)) ))))))))

;;;###skk-autoload
(defun skk-remove-common (word)
  ;; skk-henkan-key と word の間に共通の送り仮名を取り除き、送り仮名以外の部分
  ;; の文字列を返す。skk-henkan-key と skk-henkan-okurigana の値をセットする。
  ;; 例えば、word == 持ってきた であれば、skk-henkan-key := "もt",
  ;; skk-henkan-okurigana := "って", word := "持" のように分解し、word を返す。
  ;; skk-auto-okuri-process の値が non-nil であるときにこの関数を使用する。
  (if (and (not (skk-numeric-p)) (not skk-abbrev-mode)
           (or skk-henkan-in-minibuff-flag
               (and (<= skk-okuri-index-min skk-henkan-count)
                    (<= skk-henkan-count skk-okuri-index-max) )))
      (let ((midasi skk-henkan-key)
            (midasi-len (length skk-henkan-key))
            (word-len (length word))
            (kanji-len2 (* 2 skk-kanji-len))
            (mc-flag t)
            (enable-multibyte-characters t)
            (cont t)
            char pos pos2 midasi-tail word-tail new-word okuri-first
            new-skk-henkan-key )
        (if (not (and (>= midasi-len kanji-len2) (>= word-len kanji-len2)))
            nil
          ;; check if both midasi and word end with the same ascii char.
          (if (and (eq (aref midasi (1- midasi-len)) (aref word (1- word-len)))
                   (skk-alpha-char-p (aref midasi (1- midasi-len))) )
              ;; if so chop off the char from midasi and word
              (setq midasi (substring midasi 0 -1)
                    midasi-len (1- midasi-len)
                    word (substring word 0 -1)
                    word-len (1- word-len) ))
          (setq midasi-tail (substring midasi (- midasi-len skk-kanji-len)
                                       midasi-len )
                word-tail (substring word (- word-len skk-kanji-len)
                                     word-len ))
          ;; もう少し展開できそうだが、バイトコンパイラーがオプティマイズしや
          ;; すいように not を付けるだけにしておく。
          (if (not (and (string= midasi-tail word-tail)
                        (or (and (skk-string<= "ぁ" midasi-tail)
                                 (skk-string<= midasi-tail "ん") )
                            (member midasi-tail '("、" "。" "，" "．")) )))
              nil
            (setq pos (- word-len skk-kanji-len)
                  new-word new-skk-henkan-key )
            (while (and cont (> pos 0))
              (setq char (substring word (- pos skk-kanji-len) pos))
              (if (and (skk-string<= "亜" char) (skk-string<= char "瑤"))
                  ;; char is the right-most Kanji
                  (setq cont nil)
                (setq pos (- pos skk-kanji-len)) ))
            (setq pos2 (- midasi-len (- word-len pos)))
            ;; check if midasi and word has the same tail of length
            (if (not (string= (substring midasi pos2 midasi-len)
                              (substring word pos word-len) ))
                nil
              (setq okuri-first (substring word pos (+ pos skk-kanji-len)))
              (setq skk-henkan-okurigana
                    (if (and (string= okuri-first "っ")
                             (<= (+ pos kanji-len2) word-len) )
                        ;; in this case okuriga consits of two
                        ;; characters, e.g., 「残った」
                        (substring word pos (+ pos kanji-len2))
                      okuri-first ))
              (setq new-word (substring word 0 pos))
              (setq new-skk-henkan-key
                    (concat
                     (substring midasi 0 pos2)
                     (cond ((string= okuri-first "ん")
                            "n" )
                           ((string= okuri-first "っ")
                            (aref skk-kana-rom-vector
                                  (- (string-to-char
                                      (substring
                                       skk-henkan-okurigana
                                       (1- kanji-len2) kanji-len2 ))
                                     161 )))
                           (t (aref skk-kana-rom-vector
                                    (- (string-to-char
                                        (substring
                                         skk-henkan-okurigana
                                         (1- skk-kanji-len)
                                         skk-kanji-len ))
                                       161 ))))))
              (if (not skk-henkan-in-minibuff-flag)
                  (setq word new-word
                        skk-henkan-key new-skk-henkan-key )
                ;; ask if register as okuri-ari word.
                (let (inhibit-quit) ; allow keyboard quit
                  (if (y-or-n-p
                       (format
                        (if skk-japanese-message-and-error
                            "%s /%s/ を送りありエントリとして登録しますか？"
                          "Shall I register this as okuri-ari entry: %s /%s/ ? " )
                        new-skk-henkan-key new-word ))
                      (setq word new-word
                            skk-henkan-key new-skk-henkan-key )
                    (setq skk-henkan-okurigana nil
                          skk-okuri-char nil )
                    (message "") ))))))))
  ;; 分解した word (送り仮名部分を除いたもの) を返す。
  word )

;;;###skk-autoload
(defun skk-init-auto-okuri-variables ()
  ;; skk-auto.el の内部変数を初期化する。
  (setq skk-henkan-in-minibuff-flag nil
        skk-okuri-index-min -1
        skk-okuri-index-max -1 ))

;;;###skk-autoload
(defun skk-adjust-search-prog-list-for-auto-okuri ()
  ;; skk-auto-okuri-process が nil であれば、skk-search-prog-list から 
  ;; '(skk-okuri-search) を消し、non-nil であれば加える。
  ;;
  ;; '(skk-okuri-search) を加える位置については、skk-jisyo の後が最良かどうか
  ;; は分らないので、オプションで変更できるようにすべきだが...。
  (if (not skk-auto-okuri-process)
      (setq skk-search-prog-list
            (delete '(skk-okuri-search) skk-search-prog-list) )
      (if (null (member '(skk-okuri-search) skk-search-prog-list))
          (let ((pl skk-search-prog-list)
                (n 0) dic mark )
            (while pl
              (setq dic (car pl))
              (if (eq (nth 1 dic) 'skk-jisyo)
                  (setq mark n
                        pl nil)
                (setq pl (cdr pl)
                      n (1+ n) )))
            (skk-middle-list skk-search-prog-list
                             (1+ mark) '((skk-okuri-search)) )))))

;;(add-hook 'skk-mode-hook 'skk-adjust-search-prog-list-for-auto-okuri)

(run-hooks 'skk-auto-load-hook)
(provide 'skk-auto)
;;; skk-auto.el ends here
