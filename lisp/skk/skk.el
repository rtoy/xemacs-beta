;; -*-byte-compile-dynamic: t;-*-
;;; skk.el --- SKK (Simple Kana to Kanji conversion program)
;; Copyright (C) 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>

;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: Murata Shuuichirou  <mrt@mickey.ai.kyutech.ac.jp>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk.el,v 1.1 1997/12/02 08:48:40 steve Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1997/12/02 08:48:40 $

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
;;      Chikanobu Toyofuku <unbound@papaya.juice.or.jp>
;;      FURUE Hideyuki <furue@kke.co.jp>
;;      GUNJI Takao <gunji@lisa.lang.osaka-u.ac.jp>
;;      Haru Mizuno <mizu@cs3.cs.oki.co.jp>
;;      Hideki Sakurada <sakurada@kuis.kyoto-u.ac.jp>
;;      Hisao Kuroda <kuroda@msi.co.jp>
;;      Hitoshi SUZUKI <h-suzuki@ael.fujitsu.co.jp>
;;      IIDA Yosiaki <iida@secom-sis.co.jp>
;;      Jun-ichi Nakamura <nakamura@pluto.ai.kyutech.ac.jp>
;;      Katuya Tomioka <tomioka@culle.l.chiba-u.ac.jp>
;;      Kazuo Hirokawa <hirokawa@rics.co.jp>
;;      Kazushi Marukawa <kazushi@kubota.co.jp>
;;      Kimura Chikahiro <kimura@oa1.kb.nec.co.jp>
;;      Kiyotaka Sakai <ksakai@netwk.ntt-at.co.jp>
;;      Koichi MORI <kmori@onsei2.rilp.m.u-tokyo.ac.jp>
;;      MINOURA Itsushi <minoura@uni.zool.s.u-tokyo.ac.jp>
;;      MIYOSHI Tsutomu <minkov@fuzzy.or.jp>
;;      Makoto MATSUSHITA <matusita@ics.es.osaka-u.ac.jp>
;;      Masahiko Suzuki <suzmasa@sm.sony.co.jp>
;;      Masahiro Doteguchi <xdote@rp.open.cs.fujitsu.co.jp>
;;      Masakazu Takahashi <masaka-t@ascii.co.jp>
;;      Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>
;;      Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;      Motohiko Mouri <mouri@jaist.ac.jp>
;;      Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;      中津山 恒 <hisashi@rst.fujixerox.co.jp>
;;      NAMBA Seiich <pi9s-nnb@asahi-net.or.jp>
;;      Naoki HAMADA <nao@mimo.jaist-east.ac.jp>
;;      Ryoichi Hashimoto <gnu@ipri.go.jp>
;;      Sekita Daigo <sekita@mri.co.jp>
;;      進藤裕志 <shindo@super.ees.saitama-u.ac.jp>
;;      Shuji Ashizawa <ashizawa@zuken.co.jp>
;;      Takeshi OHTANI <ohtani@iias.flab.fujitsu.co.jp>
;;      Tomoyuki Hiro <hiro@momo.it.okayama-u.ac.jp>
;       柘植 正大 (ma-tsuge@kdd.co.jp)
;;      Tsugutomo Enami <enami@ptgd.sony.co.jp>
;;      Wataru Matsui <matsui@atr-rd.atr.co.jp>
;;      Yoshida Toyonobu <toyono-y@is.aist-nara.ac.jp>

;;; Change log:

;;; Code:
(require 'advice)
;; Elib 1.0 is required
;; queue-m.el and string.el are now distributed with SKK; this seems
;; to be the primary source for XEmacs
(require 'queue-m)
(require 'string)
(require 'skk-foreword)
(require 'skk-vars)

(defconst skk-version "10.38")

;;;###skk-autoload
(defconst skk-month-alist
  '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4") ("May" . "5")
    ("Jun" . "6") ("Jul" . "7") ("Aug" . "8") ("Sep" . "9") ("Oct" . "10")
    ("Nov" . "11") ("Dec" . "12") )
  "英語の月名と算用数字の連想リスト。

算用数字から英語の月名のみを出力するのであれば、ベクターを使った方が高速だが、
英語の月名から算用数字を出力するのであれば連想リストでなければ無理なので、多
目的に使用できるよう連想リストの形態を取る。

Alist of English month abbreviations and numerical values.

Although it is faster to use a vector if we only want to output
month abbreviations given the ordinal, without the alist it's
unreasonable [sic] to output the ordinal given the abbreviation,
so for multi-purpose utility we use the alist form."
)

;;;###skk-autoload
(defun skk-version ()
  (interactive)
  (if (not (interactive-p))
      skk-version
    (save-match-data
      (let* ((raw-date "$Date: 1997/12/02 08:48:40 $")
             (year (substring raw-date 7 11))
             (month (substring raw-date 12 14))
             (date (substring raw-date 15 17)) )
        (if (string-match "^0" month)
            (setq month (substring month (match-end 0))) )
        (if (string-match "^0" date)
            (setq date (substring date (match-end 0))) )
        (message "SKK version %s of %s"
                 skk-version
                 (concat (car (rassoc month skk-month-alist))
                         " " date ", " year ))))))

;;;; variables declaration
;;; user variables
(defvar skk-debug nil)

;;;###skk-autoload
(defvar skk-init-file (if (eq system-type 'ms-dos) "~/_skk" "~/.skk")
  "*SKK の初期設定ファイル名。
skk.el 9.x より ~/.emacs でのカスタマイズが可能となった。

Name of the SKK initialization file.
From skk.el 9.x on all customization may be done in ~/.emacs."
)

;;;###skk-autoload
(defvar skk-special-midashi-char-list '(?> ?< ??)
  "*接頭辞、接尾辞の入力のためのプレフィックスキー、サフィックスキーのリスト。

List of prefix and suffix keys for entering `setsutoji' and `setsuoji'."
;#SJT# What are `setsutoji' and `setsuoji'?
)

;#SJT# Is this hook also run in skk-auto-fill-mode?  Before or after?
;;;###skk-autoload
(defvar skk-mode-hook nil
  "*SKK を起動したときのフック。
他に、skk-auto-fill-mode-hook、skk-load-hook, skk-init-file でもカスタ
マイズが可能。

Hook run at SKK startup.

`skk-auto-fill-mode-hook', `skk-load-hook', and skk-init-file may also
be used for customization."
)

;;;###skk-autoload
(defvar skk-auto-fill-mode-hook nil
  "*skk-auto-fill-mode を起動したときのフック。
他に、skk-mode-hook, skk-load-hook, skk-init-file でもカスタマイズが可
能。

Hook run at startup of skk-auto-fill-mode.

`skk-mode-hook', `skk-load-hook', and `skk-init-file' may also be
used for customization."
)

;;;###skk-autoload
(defvar skk-load-hook nil
  "*skk.el をロードしたときのフック。
他に、skk-mode-hook, skk-auto-fill-mode-hook, skk-init-file でもカスタ
マイズが可能。

Hook run when SKK is loaded.

`skk-auto-fill-mode-hook', `skk-mode-hook', and `skk-init-file' may
also be used for customization."
)

;;;###skk-autoload
(defvar skk-kakutei-jisyo nil
  "*最初に検索する辞書。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
見出し語は、ソートされていなければならない。
各見出し語の最初のエントリしか検索しない (複数のエントリがあっても 2 番目以降の
エントリは無視される)。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。

The first dictionary to be searched.
If non-nil, and this variable is used as a component of
`skk-search-prog-list', the indicated dictionary is read into a
buffer and searched.
The keys must be sorted.
Only the first entry in each key is checked; if several entries are
present the second and following entries are ignored.
By setting the value of `skk-search-prog-list' the dictionaries
searched and the order of search can be changed."
)

;;;###skk-autoload
(defvar skk-initial-search-jisyo nil
  "*ユーザー辞書の検索の前に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。

This dictionary is searched before the user's personal dictionary.
The keys must be sorted.
If non-nil, and this variable is used as a component of
`skk-search-prog-list', the indicated dictionary is read into a
buffer and searched.
By setting the value of `skk-search-prog-list' the dictionaries
searched and the order of search can be changed."
)

;;;###skk-autoload
(defvar skk-large-jisyo nil
  "*ユーザー辞書の検索の後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
指定された辞書を検索のためバッファに読み込み、検索を行なう。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。

Dictionary searched after the user dictionary.
Keys must be sorted.
If non-nil and this variable is used as a component of
`skk-search-prog-list', the indicated dictionary is read into a buffer 
for search, and the search is executed.
By setting the value of `skk-search-prog-list' the dictionaries
searched and the order of search can be changed."
)

;;;###skk-autoload
(defvar skk-aux-large-jisyo nil
  "*SKK サーバーで最後に検索する辞書。
見出し語は、ソートされていなければならない。
Non-nil で、かつ skk-search-prog-list の要素の中にこの変数が使用されていれば、
SKK サーバーを使い検索を行う。
SKK サーバーが active でなければ、指定された辞書をバッファに読み込む。
skk-search-prog-list の値を設定することにより、検索対象の辞書の変更、検索の順
序の変更が可能。
この値を設定することにより、skk-server.el が autoload される。

Last dictionary to be searched by the SKK server.
Keys must be sorted.

If non-nil and this variable is used as a component of
`skk-search-prog-list', the SKK server is used to execute the search.
If the server is not active, the indicated dictionary is read into a
buffer for search, and the search is executed.
By setting the value of `skk-search-prog-list' the dictionaries
searched and the order of search can be changed.
According to the value of this variable the skkserv.el will be
autoloaded."
)

;;;###skk-autoload
(defvar skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    ;; skk-auto.el をロードすると下記の要素がプラスされる。
    ;;(skk-okuri-search)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    ;; skk-server.el をロードすると下記の要素がプラスされる。
    ;;(skk-search-server skk-aux-large-jisyo 10000)
    ;; skk-server-host もしくは skk-servers-list を指定すると、skk-server.el 
    ;; が autoload される。
    )
  "*検索関数、検索対象の辞書を決定するためのリスト。
変換した候補を返す S 式をリストの形に表記したもの。
skk-search 関数が skk-search-prog-list の car から後方向へ順番に S 式の評価を
行い変換を行なう。

This list determines the search functions used and the dictionaries
searched.
A list of S-expressions returning conversion candidates.
The function `skk-search' performs conversions by evaluating each S-
expression in order, starting with the car of `skk-search-prog-list'."
)

;;;###skk-autoload
(defvar skk-jisyo (if (eq system-type 'ms-dos) "~/_skk-jisyo" "~/.skk-jisyo")
  "*SKK のユーザー辞書。

SKK's dictionary of user-specified conversions." )

;;;###skk-autoload
(defvar skk-backup-jisyo
  (if (eq system-type 'ms-dos) "~/_skk-jisyo.BAK" "~/.skk-jisyo.BAK")
  "*SKK のユーザー辞書のバックアップファイル。

Name of user dictionary backup (a file name as a string)."
)

;;;###skk-autoload
(defvar skk-jisyo-code nil
  "*Non-nil であれば、その値で辞書バッファの漢字コードを設定する。
Mule では、*euc-japan*, *sjis*, *junet*。
また、\"euc\", \"ujis\", \"sjis\", \"jis\" などの文字列によっても指定が
可能。

If non-nil, the value sets the kanji code used in dictionary buffers.
In Mule, the symbols *euc-japan*, *sjis*, or *junet*.  Can also be
specified as a string such as \"euc\", \"ujis\", \"sjis\", or \"jis\"."
)

;;;###skk-autoload
(defvar skk-keep-record t
  "*Non-nil であれば、変換に関する記録を skk-record-file に取る。

If non-nil, a record of conversions is kept in `skk-record-file'.")

;;;###skk-autoload
(defvar skk-record-file
  (if (eq system-type 'ms-dos) "~/_skk-record" "~/.skk-record")
  "*ユーザー辞書の統計を取るファイル。
辞書セーブの時刻、単語の登録数、確定を行った回数、確定率、全体の語数の
情報を収める。

File containing statistics about the user dictionary.

At the time the dictionary is saved, the number of words registered,
number of conversions accepted, rate of acceptance, and the total
number of words are collected." )

;;;###skk-autoload
(defvar skk-kakutei-key "\C-j"
  "*確定動作 (\"skk-kakutei\") を行うキー。

The key that executes conversion confirmation (\"skk-kakutei\").")

;;;###skk-autoload
(defvar skk-use-vip nil
  "*Non-nil であれば、VIP に対応する。

If non-nil, VIP compatibility mode." )

;;;###skk-autoload
(defvar skk-use-viper nil
  "*Non-nil であれば、VIPER に対応する。。

If non-nil, VIPER compatibility mode." )

;;;###skk-autoload
(defvar skk-henkan-okuri-strictly nil
  "*Non-nil であれば、見出し語と送り仮名が一致したときだけ候補として出力する。
例えば、下記のような辞書エントリが、skk-jisyo \(プライベート辞書\) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、\"多く\" のみを出力し、\"大く\" を出力しない。

SKK-JISYO.[SML] の送り仮名エントリは上記の形式になっていないので、skk-jisyo の
送りありの辞書エントリがこの形式のものをあまり含んでいない場合は、このオプショ
ンを on にすることで、すぐに単語登録に入ってしまうので注意すること。

skk-process-okuri-early の値が nil ならば上記の形式で skk-jisyo が作られる。

Emacs 19 ベースの Mule ならば、下記のフォームを評価することで、単語登録に入っ
たときだけ一時的にこのオプションを nil にすることができる。

    \(add-hook 'minibuffer-setup-hook
              \(function
               \(lambda \(\)
                 \(if \(and \(boundp 'skk-henkan-okuri-strictly\)
                          skk-henkan-okuri-strictly
                          \(not \(eq last-command 'skk-purge-from-jisyo\)\) \)
                     \(progn
                       \(setq skk-henkan-okuri-strictly nil\)
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil t\) \)\)\)\)\)

    \(add-hook 'minibuffer-exit-hook
              \(function
               \(lambda \(\)
                 \(if \(get 'skk-henkan-okuri-strictly 'temporary-nil\)
                     \(progn
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil nil\)
                       \(setq skk-henkan-okuri-strictly t\) \)\)\)\)\)

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される\)。

If non-nil, only when the key and its inflected suffix are given
together in the dictionary will they be output as a candidate.  For
example, if the following entry is in `skk-jisyo' (the provate
dictionary),

  \"おおk /大/多/[く/多/]/[き/大/]/\"

then when converting \"▽おお*く\", only \"多く\" wil be output; \"大く
\" will not be offered as a candidate.

The inflected suffixes in SKK-JISYO.[SML] are not given in the above
way, so if very few of the entries in skk-jisyo are given in that
form, then when this option is set `on', \"word registration mode\" will 
be entered extremely often.

If the value of `skk-process-okuri-early' is `nil', new entries in
`skk-jisyo' will be created in the form above.

If using a Mule based on Emacs 19 or later, you can arrange for this
option to be temporarily set to `nil' by evaluating the following
form:

    \(add-hook 'minibuffer-setup-hook
              \(function
               \(lambda \(\)
                 \(if \(and \(boundp 'skk-henkan-okuri-strictly\)
                          skk-henkan-okuri-strictly
                          \(not \(eq last-command 'skk-purge-from-jisyo\)\) \)
                     \(progn
                       \(setq skk-henkan-okuri-strictly nil\)
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil t\) \)\)\)\)\)

    \(add-hook 'minibuffer-exit-hook
              \(function
               \(lambda \(\)
                 \(if \(get 'skk-henkan-okuri-strictly 'temporary-nil\)
                     \(progn
                       \(put 'skk-henkan-okuri-strictly 'temporary-nil nil\)
                       \(setq skk-henkan-okuri-strictly t\) \)\)\)\)\)

When using this option, `skk-process-okuri-early' must be `nil'.
(When using customize from the menubar this will automatically
temporarily be set to `nil'.)" )

;;;###skk-autoload
(defvar skk-henkan-strict-okuri-precedence nil
  "*Non-nil であれば、見出し語と送り仮名が一致した候補を優先して表示する。
例えば、下記のような辞書エントリが、skk-jisyo \(プライベート辞書\) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、まず\"多く\" を出力し、
次に \"大く\" を出力する。

\"大く\"などの候補はうっとうしいが、すぐに単語登録にはいってしまうのも
嫌なひとにおすすめ。

このオプション利用時は、skk-process-okuri-early の値は nil でならない。
また skk-henkan-okuri-strictly が non-nil のときは、この変数は無視される。
\(メニューバー を利用してカスタマイズした場合は自動的に調整される\)。")
 
;;;###skk-autoload
(defvar skk-auto-okuri-process nil
  "*Non-nil であれば、送り仮名部分を自動認識して変換を行う。
例えば、

    \"Uresii (\"UreSii\" ではなく) -> 嬉しい\"

のように変換される。但し、skk-jisyo 辞書 \(プライベート辞書\) が、

    \"うれs /嬉/[し/嬉/]/\"

のような形式になっていることが必要である \(SKK-JISYO.[SML] はこの形式に対応し
ていないので、skk-jisyo にこのエントリがなければならない\)。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される\)。" )

;;;###skk-autoload
(defvar skk-process-okuri-early nil
  "*Non-nil であれば、送り仮名のローマ字プレフィックスの入力時点で変換を開始する。
例えば、

    \"UgoK -> ▼動k\"。

送り仮名が分らないまま変換していることになるので、skk-jisyo が送り仮名に対応し
た形に成長しない。つまり

    \"うごk /動/\"

のような形態のままとなる。ただし、既に

    \"うごk /動/[く/動/]/[か/動/]/[け/動/]/[き/動/]/[こ/動/]/\"

のようなエントリが skk-jisyo にあれば、それを破壊しない。

nil であれば、送り仮名の入力が完了した時点で変換が開始する。例えば、

    \"UgoK -> ▽うご*k\", \"UgoKu -> ▼動く\"

このオプションを on にして skk-mode を起動すると、両立できないオプションである
skk-kakutei-early, skk-auto-okuri-process, skk-henkan-okuri-strictly は nil に
セットされる。" )

;;;###skk-autoload
(defvar skk-egg-like-newline nil
  "*Non-nil であれば、▼モードで改行をタイプしても確定するのみで改行しない。" )

;;;###skk-autoload
(defvar skk-kakutei-early t
  "*Non-nil であれば skk-kana-input が呼ばれたときに現在の候補を確定する。
例えば、

    \"▽かくてい -> ▼確定 -> 確定s -> 確定す\"

のように変換後、「す」の prefix である \"s\" を入力した時点で確定する。
nil であれば、例えば

    \"▽かくてい -> ▼確定 -> ▼確定s -> ▼確定する -> 確定する。\"

のように skk-kakutei を直接、間接にコールするまで \(句読点を入力したり、新たな
▽モードに入ったりすると間接的に skk-kakutei をコールする\) は、確定しないので、
その間は、変換候補を選びなおすことなどが可能。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される\)。" )

;;;###skk-autoload
(defvar skk-delete-implies-kakutei t
  "*Non-nil であれば、▼モードで BS を押すと、前の一文字を削除し確定する。
nil であれば、一つ前の候補を表示する。")

;;;###skk-autoload
(defvar skk-allow-spaces-newlines-and-tabs t
  "*Non-nil であれば、skk-henkan-key にスペース、タブ、改行があっても変換可能。
例えば、下記のように skk-henkan-key の中に改行が入っていても変換が可能である。

     \"▽か
  な\"
   -> \"仮名\"

この値が nil であれば、最初のスペースで skk-henkan-key を切り詰めてしまい、
以降のスペース、タブ、改行は無視される。
この値は、skk-start-henkan, skk-ascii-henkan, skk-katakana-henkan,
skk-hiragana-henkan, skk-zenkaku-henkan 及び skk-backward-and-set-henkan-point
の動作に影響する。")

;;;###skk-autoload
(defvar skk-convert-okurigana-into-katakana nil
  "*Non-nil であれば、カタカナモードで変換したときに送り仮名もカタカナに変換する。" )

;;;###skk-autoload
(defvar skk-delete-okuri-when-quit nil
  "*Non-nil であれば、送りありの変換中に \"C-g\" を押すと送り仮名を消し▽モードに入る。
例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" ->▽な\"

nil であれば、送り仮名を含めた見出し語をそのまま残し、■モードに入る。例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" -> なく\"" )

;;;###skk-autoload
(defvar skk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "*メニュー形式で候補を選択するときの選択キーのリスト。
\"x\", \" \" 及び \"C-g\" 以外の 7 つのキー (char type) を含む必要があ
る。\"x\", \" \" 及び \"C-g\" は候補選択時にそれぞれ特別な仕事に割り当
てられているので、このリストの中には含めないこと。")

;;;###skk-autoload
(defvar skk-ascii-mode-string " SKK"
  "*SKK が ascii モードであるときにモードラインに表示される文字列。" )

;;;###skk-autoload
(defvar skk-hirakana-mode-string " かな"
  "*ひらがなモードであるときにモードラインに表示される文字列。")

;;;###skk-autoload
(defvar skk-katakana-mode-string " カナ"
  "*カタカナモードであるときにモードラインに表示される文字列。")

;;;###skk-autoload
(defvar skk-zenkaku-mode-string " 全英"
  "*全英モードであるときにモードラインに表示される文字列。")

;;;###skk-autoload
(defvar skk-abbrev-mode-string " aあ"
  "*SKK abbrev モードであるときにモードラインに表示される文字列。")

;;;###skk-autoload
(defvar skk-echo t
  "*Non-nil であれば、仮名文字のプレフィックスを表示する。" )

;;;###skk-autoload
(defvar skk-use-numeric-conversion t
  "*Non-nil であれば、数値変換を行う。" )

;;;###skk-autoload
(defvar skk-char-type-vector
  [0 0 0 0 0 0 0 0
   5 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0
   0 4 4 4 4 4 4 4
   4 4 4 4 0 4 4 4
   4 0 4 4 4 4 4 4
   0 4 4 0 0 0 0 0
   0 3 1 1 1 3 1 1
   1 3 1 1 0 1 2 3
   1 0 1 1 1 3 1 1
   2 1 1 0 0 0 0 5]
  "*skk-kana-input で参照するかな文字変換のための char type ベクター。
各要素の数字の意味は下記の通り。

0 ローマ文字よりかな文字への変換を中止する (現在のところ使用していない)。
1 促音の一部分となり得る子音。
2 上記 1 以外の子音 (n, x)
3 母音
4 skk-mode で、skk-set-henkan-point に割り付けられている文字。
5 プレフィックスを消去する" )

;;;###skk-autoload
(defvar skk-standard-rom-kana-rule-list
  '(("b" "b" nil) ("by" "by" nil)
    ("c" "c" nil) ("ch" "ch" nil) ("cy" "cy" nil)
    ("d" "d" nil) ("dh" "dh" nil)
    ("dy" "dy" nil)
    ("f" "f" nil) ("fy" "fy" nil)
    ("g" "g" nil) ("gy" "gy" nil)
    ("h" "h" nil) ("hy" "hy" nil)
    ("j" "j" nil) ("jy" "jy" nil)
    ("k" "k" nil) ("ky" "ky" nil)
    ("m" "m" nil) ("my" "my" nil)
    ("n" "n" nil) ("ny" "ny" nil)
    ("p" "p" nil) ("py" "py" nil)
    ("r" "r" nil) ("ry" "ry" nil)
    ("s" "s" nil) ("sh" "sh" nil)
    ("sy" "sy" nil)
    ("t" "t" nil) ("th" "th" nil)
    ("ts" "ts" nil) ("ty" "ty" nil)
    ("v" "v" nil) ("w" "w" nil)
    ("x" "x" nil) ("xk" "xk" nil) ("xt" "xt" nil)
    ("xw" "xw" nil) ("xy" "xy" nil)
    ("y" "y" nil)
    ("z" "z" nil) ("zy" "zy" nil)

    ("bb" "b" ("ッ" . "っ"))
    ("cc" "c" ("ッ" . "っ"))
    ("dd" "d" ("ッ" . "っ"))
    ("ff" "f" ("ッ" . "っ"))
    ("gg" "g" ("ッ" . "っ"))
    ("hh" "h" ("ッ" . "っ"))
    ("jj" "j" ("ッ" . "っ"))
    ("kk" "k" ("ッ" . "っ"))
    ("mm" "m" ("ッ" . "っ"))
    ;;("nn" "n" ("ッ" . "っ"))
    ("pp" "p" ("ッ" . "っ"))
    ("rr" "r" ("ッ" . "っ"))
    ("ss" "s" ("ッ" . "っ"))
    ("tt" "t" ("ッ" . "っ"))
    ("vv" "v" ("ッ" . "っ"))
    ("ww" "w" ("ッ" . "っ"))
    ("xx" "x" ("ッ" . "っ"))
    ("yy" "y" ("ッ" . "っ"))
    ("zz" "z" ("ッ" . "っ"))

    ("a" nil ("ア" . "あ"))
    ("ba" nil ("バ" . "ば")) ("bya" nil ("ビャ" . "びゃ"))
    ("cha" nil ("チャ" . "ちゃ")) ("cya" nil ("チャ" . "ちゃ"))
    ("da" nil ("ダ" . "だ")) ("dha" nil ("デャ" . "でゃ"))
    ("dya" nil ("ヂャ" . "ぢゃ"))
    ("fa" nil ("ファ" . "ふぁ")) ("fya" nil ("フャ" . "ふゃ"))
    ("ga" nil ("ガ" . "が")) ("gya" nil ("ギャ" . "ぎゃ"))
    ("ha" nil ("ハ" . "は")) ("hya" nil ("ヒャ" . "ひゃ"))
    ("ja" nil ("ジャ" . "じゃ")) ("jya" nil ("ジャ" . "じゃ"))
    ("ka" nil ("カ" . "か")) ("kya" nil ("キャ" . "きゃ"))
    ("ma" nil ("マ" . "ま")) ("mya" nil ("ミャ" . "みゃ"))
    ("na" nil ("ナ" . "な")) ("nya" nil ("ニャ" . "にゃ"))
    ("pa" nil ("パ" . "ぱ")) ("pya" nil ("ピャ" . "ぴゃ"))
    ("ra" nil ("ラ" . "ら")) ("rya" nil ("リャ" . "りゃ"))
    ("sa" nil ("サ" . "さ")) ("sha" nil ("シャ" . "しゃ"))
    ("sya" nil ("シャ" . "しゃ"))
    ("ta" nil ("タ" . "た")) ("tha" nil ("テァ" . "てぁ"))
    ("tya" nil ("チャ" . "ちゃ"))
    ("va" nil ("ヴァ" . "う゛ぁ")) ("wa" nil ("ワ" . "わ"))
    ("xa" nil ("ァ" . "ぁ")) ("xka" nil ("ヵ" . "か"))
    ("xwa" nil ("ヮ" . "ゎ")) ("xya" nil ("ャ" . "ゃ"))
    ("ya" nil ("ヤ" . "や"))
    ("za" nil ("ザ" . "ざ")) ("zya" nil ("ジャ" . "じゃ"))

    ("i" nil ("イ" . "い"))
    ("bi" nil ("ビ" . "び")) ("byi" nil ("ビィ" . "びぃ"))
    ("chi" nil ("チ" . "ち")) ("cyi" nil ("チィ" . "ちぃ"))
    ("di" nil ("ヂ" . "ぢ")) ("dhi" nil ("ディ" . "でぃ"))
    ("dyi" nil ("ヂィ" . "ぢぃ"))
    ("fi" nil ("フィ" . "ふぃ")) ("fyi" nil ("フィ" . "ふぃ"))
    ("gi" nil ("ギ" . "ぎ")) ("gyi" nil ("ギィ" . "ぎぃ"))
    ("hi" nil ("ヒ" . "ひ")) ("hyi" nil ("ヒィ" . "ひぃ"))
    ("ji" nil ("ジ" . "じ")) ("jyi" nil ("ジィ" . "じぃ"))
    ("ki" nil ("キ" . "き")) ("kyi" nil ("キィ" . "きぃ"))
    ("mi" nil ("ミ" . "み")) ("myi" nil ("ミィ" . "みぃ"))
    ("ni" nil ("ニ" . "に")) ("nyi" nil ("ニィ" . "にぃ"))
    ("pi" nil ("ピ" . "ぴ")) ("pyi" nil ("ピィ" . "ぴぃ"))
    ("ri" nil ("リ" . "り")) ("ryi" nil ("リィ" . "りぃ"))
    ("si" nil ("シ" . "し")) ("shi" nil ("シ" . "し"))
    ("syi" nil ("シィ" . "しぃ"))
    ("ti" nil ("チ" . "ち")) ("thi" nil ("ティ" . "てぃ"))
    ("tyi" nil ("チィ" . "ちぃ"))
    ("vi" nil ("ヴィ" . "う゛ぃ")) ("wi" nil ("ウィ" . "うぃ"))
    ("xi" nil ("ィ" . "ぃ")) ("xwi" nil ("ヰ" . "ゐ"))
    ("zi" nil ("ジ" . "じ")) ("zyi" nil ("ジィ" . "じぃ"))

    ("u" nil ("ウ" . "う"))
    ("bu" nil ("ブ" . "ぶ")) ("byu" nil ("ビュ" . "びゅ"))
    ("chu" nil ("チュ" . "ちゅ")) ("cyu" nil ("チュ" . "ちゅ"))
    ("du" nil ("ヅ" . "づ")) ("dhu" nil ("デュ" . "でゅ"))
    ("dyu" nil ("ヂュ" . "ぢゅ"))
    ("fu" nil ("フ" . "ふ")) ("fyu" nil ("フュ" . "ふゅ"))
    ("gu" nil ("グ" . "ぐ")) ("gyu" nil ("ギュ" . "ぎゅ"))
    ("hu" nil ("フ" . "ふ")) ("hyu" nil ("ヒュ" . "ひゅ"))
    ("ju" nil ("ジュ" . "じゅ")) ("jyu" nil ("ジュ" . "じゅ"))
    ("ku" nil ("ク" . "く")) ("kyu" nil ("キュ" . "きゅ"))
    ("mu" nil ("ム" . "む")) ("myu" nil ("ミュ" . "みゅ"))
    ("nu" nil ("ヌ" . "ぬ")) ("nyu" nil ("ニュ" . "にゅ"))
    ("pu" nil ("プ" . "ぷ")) ("pyu" nil ("ピュ" . "ぴゅ"))
    ("ru" nil ("ル" . "る")) ("ryu" nil ("リュ" . "りゅ"))
    ("su" nil ("ス" . "す")) ("shu" nil ("シュ" . "しゅ"))
    ("syu" nil ("シュ" . "しゅ"))
    ("tu" nil ("ツ" . "つ")) ("thu" nil ("テュ" . "てゅ"))
    ("tsu" nil ("ツ" . "つ")) ("tyu" nil ("チュ" . "ちゅ"))
    ("vu" nil ("ヴ" . "う゛")) ("wu" nil ("ウ" . "う"))
    ("xu" nil ("ゥ" . "ぅ")) ("xtu" nil ("ッ" . "っ"))
    ("xtsu" nil ("ッ" . "っ")) ("xyu" nil ("ュ" . "ゅ"))
    ("yu" nil ("ユ" . "ゆ"))
    ("zu" nil ("ズ" . "ず")) ("zyu" nil ("ジュ" . "じゅ"))

    ("e" nil ("エ" . "え"))
    ("be" nil ("ベ" . "べ")) ("bye" nil ("ビェ" . "びぇ"))
    ("che" nil ("チェ" . "ちぇ")) ("cye" nil ("チェ" . "ちぇ"))
    ("de" nil ("デ" . "で")) ("dhe" nil ("デェ" . "でぇ"))
    ("dye" nil ("ヂェ" . "ぢぇ"))
    ("fe" nil ("フェ" . "ふぇ")) ("fye" nil ("フェ" . "ふぇ"))
    ("ge" nil ("ゲ" . "げ")) ("gye" nil ("ギェ" . "ぎぇ"))
    ("he" nil ("ヘ" . "へ")) ("hye" nil ("ヒェ" . "ひぇ"))
    ("je" nil ("ジェ" . "じぇ")) ("jye" nil ("ジェ" . "じぇ"))
    ("ke" nil ("ケ" . "け")) ("kye" nil ("キェ" . "きぇ"))
    ("me" nil ("メ" . "め")) ("mye" nil ("ミェ" . "みぇ"))
    ("ne" nil ("ネ" . "ね")) ("nye" nil ("ニェ" . "にぇ"))
    ("pe" nil ("ペ" . "ぺ")) ("pye" nil ("ピェ" . "ぴぇ"))
    ("re" nil ("レ" . "れ")) ("rye" nil ("リェ" . "りぇ"))
    ("se" nil ("セ" . "せ")) ("she" nil ("シェ" . "しぇ"))
    ("sye" nil ("シェ" . "しぇ"))
    ("te" nil ("テ" . "て")) ("the" nil ("テェ" . "てぇ"))
    ("tye" nil ("チェ" . "ちぇ"))
    ("ve" nil ("ヴェ" . "う゛ぇ")) ("we" nil ("ウェ" . "うぇ"))
    ("xe" nil ("ェ" . "ぇ")) ("xke" nil ("ヶ" . "け"))
    ("xwe" nil ("ヱ" . "ゑ"))
    ("ye" nil ("イェ" . "いぇ"))
    ("ze" nil ("ゼ" . "ぜ")) ("zye" nil ("ジェ" . "じぇ"))

    ("o" nil ("オ" . "お"))
    ("bo" nil ("ボ" . "ぼ")) ("byo" nil ("ビョ" . "びょ"))
    ("cho" nil ("チョ" . "ちょ")) ("cyo" nil ("チョ" . "ちょ"))
    ("do" nil ("ド" . "ど")) ("dho" nil ("デョ" . "でょ"))
    ("dyo" nil ("ヂョ" . "ぢょ"))
    ("fo" nil ("フォ" . "ふぉ")) ("fyo" nil ("フョ" . "ふょ"))
    ("go" nil ("ゴ" . "ご")) ("gyo" nil ("ギョ" . "ぎょ"))
    ("ho" nil ("ホ" . "ほ")) ("hyo" nil ("ヒョ" . "ひょ"))
    ("jo" nil ("ジョ" . "じょ")) ("jyo" nil ("ジョ" . "じょ"))
    ("ko" nil ("コ" . "こ")) ("kyo" nil ("キョ" . "きょ"))
    ("mo" nil ("モ" . "も")) ("myo" nil ("ミョ" . "みょ"))
    ("no" nil ("ノ" . "の")) ("nyo" nil ("ニョ" . "にょ"))
    ("po" nil ("ポ" . "ぽ")) ("pyo" nil ("ピョ" . "ぴょ"))
    ("ro" nil ("ロ" . "ろ")) ("ryo" nil ("リョ" . "りょ"))
    ("so" nil ("ソ" . "そ")) ("sho" nil ("ショ" . "しょ"))
    ("syo" nil ("ショ" . "しょ"))
    ("to" nil ("ト" . "と")) ("tho" nil ("テョ" . "てょ"))
    ("tyo" nil ("チョ" . "ちょ"))
    ("vo" nil ("ヴォ" . "う゛ぉ")) ("wo" nil ("ヲ" . "を"))
    ("xo" nil ("ォ" . "ぉ")) ("xyo" nil ("ョ" . "ょ"))
    ("yo" nil ("ヨ" . "よ"))
    ("zo" nil ("ゾ" . "ぞ")) ("zyo" nil ("ジョ" . "じょ"))

    ("nn" nil ("ン" . "ん"))
    ("n'" nil ("ン" . "ん"))

    ("z/" nil ("・" . "・")) ("z," nil ("‥" . "‥"))
    ("z." nil ("…" . "…")) ("z-" nil ("〜" . "〜"))
    ("zh" nil ("←" . "←")) ("zj" nil ("↓" . "↓"))
    ("zk" nil ("↑" . "↑")) ("zl" nil ("→" . "→"))
    ("z[" nil ("『" . "『")) ("z]" nil ("』" . "』")) )
  "SKK の標準のローマ字かな変換のオートマトンの状態遷移規則。
リストの各要素は、\(現在の状態@入力 次の状態 出力\) \(但し、\"@\" は連接\) を意味
する。
システム用なのでカスタマイズには skk-rom-kana-rule-list を利用してください。" )

;;;###skk-autoload
(defvar skk-rom-kana-rule-list
  nil
  "*ローマ字かな変換のオートマトンの状態遷移規則。
リストの各要素は、\(現在の状態@入力 次の状態 出力\) \(但し、\"@\" は連接\) を意味
する。カスタマイズには skk-standard-rom-kana-rule-list では無く、
こちらを利用してください。" )

;;;###skk-autoload
(defvar skk-fallback-rule-alist
  '(("n" "ン" . "ん"))
  "*ローマ字かな変換時に、skk-rom-kana-rule-list, skk-standard-rom-kana-rule-list の
あとに参照される規則。
リストの各要素は、\(現在の状態 出力\) を意味する。
この規則が適用された場合、入力はストリームに返される。" )

;;;###skk-autoload
(defvar skk-postfix-rule-alist
  '(("oh" "オ" . "お"))
  "*ローマ字かな変換時に、直前のかな文字を作るのに用いられた最後の入力と
現在の入力からかな文字を作りだすための規則。
リストの各要素は、\(直前の入力@入力 出力\) \(但し、\"@\" は連接\) を意味する。" )

;;;###skk-autoload
(defvar skk-previous-candidate-char
  ?x
  "*skk-previous-candidate を割当てたキャラクタ。" )

;;;###skk-autoload
(defvar skk-okuri-char-alist
  nil
  "*" )

;;;###skk-autoload
(defvar skk-downcase-alist
  nil
  "*" )

;;;###skk-autoload
(defvar skk-input-vector
  [nil  nil  nil  nil  nil  nil  nil  nil  ;7
   nil  nil  nil  nil  nil  nil  nil  nil  ;15
   nil  nil  nil  nil  nil  nil  nil  nil  ;23
   nil  nil  nil  nil  nil  nil  nil  nil  ;31
   nil  "！" nil  nil  nil  nil  nil  nil  ;39
   nil  nil  nil  nil  "、" "ー" "。" nil  ;47
   nil  nil  nil  nil  nil  nil  nil  nil  ;55
   nil  nil  "：" "；" nil  nil  nil  "？" ;63
   nil  nil  nil  nil  nil  nil  nil  nil  ;71
   nil  nil  nil  nil  nil  nil  nil  nil  ;79
   nil  nil  nil  nil  nil  nil  nil  nil  ;87
   nil  nil  nil  "「" nil  "」" nil  nil  ;95
   nil  nil  nil  nil  nil  nil  nil  nil  ;103
   nil  nil  nil  nil  nil  nil  nil  nil  ;111
   nil  nil  nil  nil  nil  nil  nil  nil  ;119
   nil  nil  nil  nil  nil  nil  nil  nil] ;127
  "*skk-self-insert で参照される文字テーブル。
キーに対応する位置に文字列があれば、ひらがなモードもしくはカタカナモードで、該
当のキーを押すことで、対応する文字が挿入される。
例えば、\"~\" キーに対応して、\"〜\" を挿入させるように変更したければ、skk.el 
のロード後 (もしくは skk-load-hook を利用して)、

  \(aset skk-input-vector 126 \"〜\"\)

とするか、もしくは、skk-input-vector の 126 番目 (0 番から数えて) の値を
\"〜\" とするような skk-input-vector を直接書き、setq で代入する \(126 は、?
{ を評価したときの値\)。" )

;;;###skk-autoload
(defvar skk-zenkaku-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "　"  "！" "”" "＃" "＄" "％" "＆" "’"
   "（" "）" "＊" "＋" "，" "−" "．" "／"
   "０" "１" "２" "３" "４" "５" "６" "７"
   "８" "９" "：" "；" "＜" "＝" "＞" "？"
   "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
   "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
   "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
   "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
   "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
   "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
   "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
   "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "〜" nil]
  "*skk-zenkaku-insert で参照される文字テーブル。
キーに対応する位置に文字列があれば、全英モードで該当のキーを押すことで、対応す
る文字が挿入される。
値の変更方法については、skk-input-vector を参照のこと。" )

;;;###skk-autoload
(defvar skk-use-face (or window-system (skk-terminal-face-p))
  "*Non-nil であれば、Emacs 19 の face の機能を使用して変換表示などを行なう。" )

;;;###skk-autoload
(defvar skk-henkan-face
  (if (and (or window-system (skk-terminal-face-p))
           (or (and (fboundp 'frame-face-alist)
		    (assq 'highlight (frame-face-alist (selected-frame))))
	       (and (fboundp 'face-list)
		    (memq 'highlight (face-list)))) )
      'highlight )
  "*変換候補の face 属性。skk-use-face が non-nil のときのみ有効。
Emacs 標準フェイスの default, modeline, region, secondary-selection,
highlight, underline, bold, italic, bold-italic の他、新たに face を作り指定す
ることも可能。
新たな face を作り指定するには skk-make-face を利用して、

      \(skk-make-face 'DimGray/PeachPuff1\)
      \(setq skk-henkan-face 'DimGray/PeachPuff1\)

のようにするのが手軽。foreground と background の色指定だけでない凝った face
を作る場合は、skk-make-face では対応できないので、Emacs の hilit19.el の
hilit-lookup-face-create などを利用する。色を付ける場合の配色は、canna.el の
canna:attribute-alist が良い例かもしれない。" )

;;;###skk-autoload
(defvar skk-use-color-cursor (and window-system
                                      (fboundp 'x-display-color-p)
                                      (x-display-color-p) )
  "*Non-nil であれば、SKK モードの入力モードに応じてカーソルに色を付ける。")

(defvar skk-default-cursor-color
  (if skk-xemacs
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
  "*SKK のオフを示すカーソル色。" )

;; 背景色を黒にして使用されている方で、良い配色があればお知らせ下さい。
;;;###skk-autoload
(defvar skk-hirakana-cursor-color (if (eq skk-background-mode 'light)
                                      "coral4"
                                    "pink" )
  "*かなモードを示すカーソル色。" )

;;;###skk-autoload
(defvar skk-katakana-cursor-color (if (eq skk-background-mode 'light)
                                          "forestgreen"
                                        "green" )
  "*カタカナモードを示すカーソル色。" )

;;;###skk-autoload
(defvar skk-zenkaku-cursor-color "gold"
  "*全角英字モードを示すカーソル色。" )

;;;###skk-autoload
(defvar skk-ascii-cursor-color (if (eq skk-background-mode 'light)
                                       "ivory4"
                                     "gray" )
  "*アスキーモードを示すカーソル色。" )

;;;###skk-autoload
(defvar skk-abbrev-cursor-color "royalblue"
  "*アスキーモードを示すカーソル色。" )

;;;###skk-autoload
(defvar skk-report-set-cursor-error t
  "*Non-nil であれば、カラーマップ切れが起きた場合、エラーメッセージを表示する。
nil であれば、表示しない。" )

;;;###skk-autoload
(defvar skk-use-cursor-change t
  "*Non-nil であれば、Ovwrt マイナーモード時にカーソルの幅を縮める。" )

;;;###skk-autoload
(defvar skk-auto-insert-paren nil
  "*Non-nil であれば、2 つの文字列をまとめて挿入し、その文字列の間にカーソルを移動する。
例えば、\"「\" を入力したときに \"」\" を自動的に挿入し、両かぎかっこの間に
カーソルを移動する。
挿入する文字列は、skk-auto-paren-string-alist で指定する。" )

;;;###skk-autoload
(defvar skk-auto-paren-string-alist
  '(("「" . "」") ("『" . "』") ("(" . ")") ("（" . "）")
    ("{" . "}")("｛" . "｝") ("〈" . "〉") ("《" . "》")
    ("[" . "]") ("［" . "］") ("〔" . "〕") ("【" . "】")
    ("\"" . "\"")("“" . "”")
    ;; skk-special-midashi-char-list の要素になっている文字は、
    ;; skk-auto-paren-string-alist に含めても削除される。
    ;;("<" . ">")
    )
  "*自動的に対になる文字列を入力するための連想リスト。
car の文字列が挿入されたときに cdr の文字列を自動的に挿入する。" )

;;;###skk-autoload
(defvar skk-japanese-message-and-error nil
  "*Non-nil であれば、SKK のメッセージとエラーを日本語で表示する。
nil であれば、英語で表示する。" )

;;;###skk-autoload
(defvar skk-ascii-mode-map nil "*ASCII モードのキーマップ。" )
(or skk-ascii-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map skk-kakutei-key 'skk-kakutei)
    (skk-define-menu-bar-map map)
    (setq skk-ascii-mode-map map)))

;;;###skk-autoload
(defvar skk-j-mode-map nil "*かなモードのキーマップ。" )
(or skk-j-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'skk-self-insert)
    (define-key map "#" 'skk-self-insert)
    (define-key map "$" 'skk-display-code-for-char-at-point)
    (define-key map "%" 'skk-self-insert)
    (define-key map "&" 'skk-self-insert)
    (define-key map "'" 'skk-self-insert)
    (define-key map "*" 'skk-self-insert)
    (define-key map "+" 'skk-self-insert)
    (define-key map "," 'skk-insert-comma)
    (define-key map "-" 'skk-self-insert)
    (define-key map "." 'skk-insert-period)
    (define-key map "/" 'skk-abbrev-mode)
    (define-key map "0" 'skk-self-insert)
    (define-key map "1" 'skk-self-insert)
    (define-key map "2" 'skk-self-insert)
    (define-key map "3" 'skk-self-insert)
    (define-key map "4" 'skk-self-insert)
    (define-key map "5" 'skk-self-insert)
    (define-key map "6" 'skk-self-insert)
    (define-key map "7" 'skk-self-insert)
    (define-key map "8" 'skk-self-insert)
    (define-key map "9" 'skk-self-insert)
    (define-key map ":" 'skk-self-insert)
    (define-key map ";" 'skk-self-insert)
    ;; "<", ">", "?" の 3 文字は、skk-special-midashi-char-list の値がディフォル
    ;; トのままであれば、skk-setup-special-midashi-char により
    ;; skk-set-henkan-point に再割り付けされるが、設定によりこれらの文字を指定し
    ;; ない場合は、skk-self-insert として動くのが望ましい。
    (define-key map "<" 'skk-self-insert)
    (define-key map "=" 'skk-self-insert)
    (define-key map ">" 'skk-self-insert)
    (define-key map "?" 'skk-self-insert)
    (define-key map "@" 'skk-today)
    (define-key map "A" 'skk-set-henkan-point)
    (define-key map "B" 'skk-set-henkan-point)
    (define-key map "C" 'skk-set-henkan-point)
    (define-key map "D" 'skk-set-henkan-point)
    (define-key map "E" 'skk-set-henkan-point)
    (define-key map "F" 'skk-set-henkan-point)
    (define-key map "G" 'skk-set-henkan-point)
    (define-key map "H" 'skk-set-henkan-point)
    (define-key map "I" 'skk-set-henkan-point)
    (define-key map "J" 'skk-set-henkan-point)
    (define-key map "K" 'skk-set-henkan-point)
    (define-key map "L" 'skk-zenkaku-mode)
    (define-key map "M" 'skk-set-henkan-point)
    (define-key map "N" 'skk-set-henkan-point)
    (define-key map "O" 'skk-set-henkan-point)
    (define-key map "P" 'skk-set-henkan-point)
    (define-key map "Q" 'skk-set-henkan-point-subr)
    (define-key map "R" 'skk-set-henkan-point)
    (define-key map "S" 'skk-set-henkan-point)
    (define-key map "T" 'skk-set-henkan-point)
    (define-key map "U" 'skk-set-henkan-point)
    (define-key map "V" 'skk-set-henkan-point)
    (define-key map "W" 'skk-set-henkan-point)
    (define-key map "X" 'skk-purge-from-jisyo)
    (define-key map "Y" 'skk-set-henkan-point)
    (define-key map "Z" 'skk-set-henkan-point)
    (define-key map "\ " 'skk-start-henkan)
    (define-key map "\"" 'skk-self-insert)
    (define-key map "\(" 'skk-self-insert)
    (define-key map "\)" 'skk-self-insert)
    ;;(define-key map "\177" 'skk-delete-backward-char)
    ;;(define-key map "\C-g" 'skk-keyboard-quit)
    ;;(define-key map "\C-m" 'skk-newline)
    (define-key map "\[" 'skk-self-insert)
    (define-key map "\\" 'skk-input-by-code-or-menu)
    (define-key map "\]" 'skk-self-insert)
    (or skk-use-vip
        (define-key map "\M-\ " 'skk-start-henkan-with-completion) )
    (or skk-use-vip
        (define-key map "\M-Q" 'skk-backward-and-set-henkan-point) )
    (define-key map "\t" 'skk-try-completion)
    (define-key map "\{" 'skk-self-insert)
    (define-key map "\}" 'skk-self-insert)
    (define-key map "^" 'skk-self-insert)
    (define-key map "_" 'skk-self-insert)
    (define-key map "`" 'skk-self-insert)
    (define-key map "a" 'skk-kana-input)
    (define-key map "b" 'skk-kana-input)
    (define-key map "c" 'skk-kana-input)
    (define-key map "d" 'skk-kana-input)
    (define-key map "e" 'skk-kana-input)
    (define-key map "f" 'skk-kana-input)
    (define-key map "g" 'skk-kana-input)
    (define-key map "h" 'skk-kana-input)
    (define-key map "i" 'skk-kana-input)
    (define-key map "j" 'skk-kana-input)
    (define-key map "k" 'skk-kana-input)
    (define-key map "l" 'skk-ascii-mode)
    (define-key map "m" 'skk-kana-input)
    (define-key map "n" 'skk-kana-input)
    (define-key map "o" 'skk-kana-input)
    (define-key map "p" 'skk-kana-input)
    (define-key map "q" 'skk-toggle-kana)
    (define-key map "r" 'skk-kana-input)
    (define-key map "s" 'skk-kana-input)
    (define-key map "t" 'skk-kana-input)
    (define-key map "u" 'skk-kana-input)
    (define-key map "v" 'skk-kana-input)
    (define-key map "w" 'skk-kana-input)
    (define-key map "x" 'skk-previous-candidate)
    (define-key map "y" 'skk-kana-input)
    (define-key map "z" 'skk-kana-input)
    (define-key map "|" 'skk-self-insert)
    (define-key map "~" 'skk-self-insert)
    (define-key map skk-kakutei-key 'skk-kakutei)
    (skk-define-menu-bar-map map)
    (setq skk-j-mode-map map)))

;;;###skk-autoload
(defvar skk-zenkaku-mode-map nil "*全角モードのキーマップ。" )
(or skk-zenkaku-mode-map
  (let ((map (make-sparse-keymap))
        (i 0) )
    (while (< i 128)
      (if (aref skk-zenkaku-vector i)
          (define-key map (char-to-string i) 'skk-zenkaku-insert) )
      (setq i (1+ i)) )
    (define-key map skk-kakutei-key 'skk-kakutei)
    (or skk-use-vip
        (define-key map "\M-Q" 'skk-backward-and-set-henkan-point) )
    (define-key map "\C-q" 'skk-ascii-henkan)
    (skk-define-menu-bar-map map)
    (setq skk-zenkaku-mode-map map)))

;;;###skk-autoload
(defvar skk-abbrev-mode-map nil "*SKK abbrev モードのキーマップ。" )
(or skk-abbrev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," 'skk-abbrev-comma)
    (define-key map "." 'skk-abbrev-period)
    (define-key map "\ " 'skk-start-henkan)
    ;;(define-key map "\177" 'skk-delete-backward-char)
    ;;(define-key map "\C-g" 'skk-keyboard-quit)
    ;;(define-key map "\C-m" 'skk-newline)
    (define-key map "\C-q" 'skk-zenkaku-henkan)
    (or skk-use-vip
        (define-key map "\M-\ " 'skk-start-henkan-with-completion) )
    (define-key map "\t" 'skk-try-completion)
    (define-key map skk-kakutei-key 'skk-kakutei)
    (skk-define-menu-bar-map map)
    (setq skk-abbrev-mode-map map)))

;;;###skk-autoload
(defvar skk-jisyo-save-count 50
  "*数値であれば、その回数辞書が更新されたときに辞書を自動的にセーブする。
  nil であれば、辞書のオートセーブを行なわない。" )

;;;###skk-autoload
(defvar skk-byte-compile-init-file t
  "*Non-nil であれば、skk-mode 起動時に skk-init-file をバイトコンパイルする。
正確に言うと、

  (1)skk-init-file をバイトコンパイルしたファイルがないか、
  (2)skk-init-file とそのバイトコンパイル済ファイルを比較して、前者の方が新し
     いとき

に skk-init-file をバイトコンパイルする。
nil であれば、skk-init-file とそのバイトコンパイル済みファイルを比較して 
skk-init-file の方が新しいときは、そのバイトコンパイル済ファイルを消す。" )

;;;###skk-autoload
(defvar skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil であれば、Emacs を終了するときに正確に個人辞書の候補数を数える。
nil であれば、1 行に複数の候補があっても 1 候補として数える。
計算結果は、skk-record-file に保存される。" )

;;;###skk-autoload
(defvar skk-compare-jisyo-size-when-saving t
  "*Non-nil であれば、skk-jisyo のセーブ時にファイルサイズのチェックを行なう。
前回セーブした skk-jisyo と今回セーブしようとする辞書とのサイズ比較を行ない、
後者の方が大きいときにユーザーにセーブを続けるかどうかの確認を求める。" )

;;;###skk-autoload
(defvar skk-auto-start-henkan t
  "単語や文節の区切りを示す文字の打鍵により自動的に変換を開始する。
skk-auto-start-henkan-keyword-list により単語や文節の区切りを示す文字を指定する。" )

;;;###skk-autoload
(defvar skk-auto-start-henkan-keyword-list
  '("を" "、" "。" "．" "，" "？" "」" "！"
    "；" "：" ")" ";" ":" "）" "”" "】" "』"
    "》" "〉" "｝" "］" "〕" "}" "]" "?" "."
    "," "!" )
;; あまりキーワードが多くなると、通常の変換を困難にする？
  "自動変換を開始するキーワード。
このリストの要素の文字を挿入すると、SPC を押すことなく自動的に変換を開始する。" )

;;;###skk-autoload
(defvar skk-search-excluding-word-pattern-function nil
  "*個人辞書に取り込まない文字列のパターンを検索する関数を指定する。
確定した文字列を引数に渡して funcall される。

SKK では変換、確定を行なった文字列は全て個人辞書に取り込まれるが、この変数で指
定された関数が non-nil を返すとその文字列は個人辞書に取り込まれない。例えば、
この変数に下記のような指定すると、SKK abbrev mode での変換を除き、カタカナのみ
からなる文字列を変換により得て確定しても、それを個人辞書に取り込まない。

カタカナを変換により求めたいが、個人辞書にはカタカナのみの候補を取り込みたくな
い、など、個人辞書が必要以上に膨れるのを抑える目的に使用できる。

個人辞書に取り込まない文字列については補完が効かないので、注意すること。

  \(setq skk-search-excluding-word-pattern-function
        \(function
         \(lambda \(kakutei-word\)
         ;; この関数が t を返したときは、その文字列は個人辞書に取り込まれない。
           \(save-match-data
             \(and
            ;; 送りなし変換で、
              \(not skk-okuri-char\)
            ;; 確定語がカタカナのみから構成されていて、
              \(string-match \"^[ーァ-ン]+$\" kakutei-word\)
            ;; SKK abbrev mode 以外での変換か、
              \(or \(not skk-abbrev-mode\)
                ;; 見出し語がカタカナ、ひらがな以外のとき。
                ;; \(後で▽マークを付けたときは、見出し語が英文字でも、
                ;; skk-abbrev-modeが t になっていない\)。
                  \(not \(string-match \"^[^ーァ-ンぁ-ん]+$\" skk-henkan-key\)\) \)\)\)\)\)\) ")

;;; -- internal variables
;; ---- global variables
(defconst skk-ml-address "skk-develop@kuis.kyoto-u.ac.jp")

(defconst skk-coding-system-alist
  (if (or skk-mule3 skk-xemacs)
      '(("euc" . euc-japan)
        ("ujis" . euc-japan)
        ("sjis". sjis)
        ("jis" . junet) )
    '(("euc" . *euc-japan*)
      ("ujis" . *euc-japan*)
      ("sjis". *sjis*)
      ("jis" . *junet*) ))
  "coding-system の文字列表現と、シンボル表現の連想リスト。" )

(defconst skk-default-zenkaku-vector
  ;; note that skk-zenkaku-vector is a user variable.
  ;; skk.el ロード前に .emacs などで、skk-zenkaku-vector の別の値をユーザーが
  ;; 直接書いたり、skk.el ロード後にこの値を aset で直接いじったりしなければ
  ;; default-value で skk-zenkaku-vector にアクセスすることで
  ;; skk-default-zenkaku-vector の値を保持することもできようが、それは望めな
  ;; い...。
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "　"  "！" "”" "＃" "＄" "％" "＆" "’"
   "（" "）" "＊" "＋" "，" "−" "．" "／"
   "０" "１" "２" "３" "４" "５" "６" "７"
   "８" "９" "：" "；" "＜" "＝" "＞" "？"
   "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ"
   "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ"
   "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ"
   "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿"
   "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ"
   "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ"
   "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ"
   "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "〜" nil]
  "skk-zenkaku-region で参照する文字テーブル。
\"ascii\" -> \"ａｓｃｉｉ\" のような全角文字への変換を行う際に利用する。" )

;;;###skk-autoload
(defconst skk-kanji-len (length "あ")
  "漢字一文字の長さ。Mule では 3 になる。XEmacs では 1。" )

(defconst skk-hankaku-alist
  '((161 . 32) ; ?\ 
    (170 . 33) ;?\!
    (201 . 34) ;?\"
    (244 . 35) ;?\#
    (240 . 36) ;?\$
    (243 . 37) ;?\%
    (245 . 38) ;?\&
    (199 . 39) ;?\'
    (202 . 40) ;?\(
    (203 . 41) ;?\)
    (246 . 42) ;?\*
    (220 . 43) ;?\+
    (164 . 44) ;?\,
    (221 . 45) ;?\-
    (165 . 46) ;?\.
    (191 . 47) ;?\/
    (167 . 58) ;?\:
    (168 . 59) ;?\;
    (227 . 60) ;?\<
    (225 . 61) ;?\=
    (228 . 62) ;?\>
    (169 . 63) ;?\?
    (247 . 64) ;?\@
    (206 . 91) ;?\[
    (239 . 92) ;?\\
    (207 . 93) ;?\]
    (176 . 94) ;?^ 
    (178 . 95) ;?\_
    (208 . 123) ;?\{
    (195 . 124) ;?\|
    (209 . 125) ;?\}
    (177 . 126) ;?\~
    (198 . 96)) ;?` 
  "文字コードの 2 番目のバイトとその文字に対応する ascii 文字 \(char\) との連想リスト。
skk-ascii-region で参照する。Mule-2.3 添付の egg.el よりコピーした。" )

;;;###skk-autoload
(defvar skk-insert-new-word-function nil
  "候補を挿入したときに funcall される関数を保存する変数。" )

;;;###skk-autoload
(defvar skk-input-mode-string skk-hirakana-mode-string
  "SKK の入力モードを示す文字列。skk-mode 起動時は、skk-hirakana-mode-string。" )

;;;###skk-autoload
(defvar skk-isearch-message nil
  "skk-isearch 関数をコールするためのフラグ。
Non-nil であれば、skk-isearch-message 関数をコールする。" )

;;;###skk-autoload
(defvar skk-mode-invoked nil
  "Non-nil であれば、Emacs を起動後既に skk-mode を起動したことを示す。" )

(defvar skk-kakutei-count 0
  "変換候補を確定したカウントを保持する変数。
skk-record-file の \"確定:\" 項目のカウンター。" )

(defvar skk-touroku-count 0
  "辞書登録したカウントを保持する変数。
skk-record-file の \"登録:\" 項目のカウンター。" )

(defvar skk-update-jisyo-count 0
  "辞書を更新した回数。
このカウンターの数字が skk-jisyo-save-count 以上となったときにユーザー辞書のオー
トセーブが行なわれる。" )

(defvar skk-use-relation nil
  "*skk-relation を使用する。これは直前の変換を憶えておくことで、
変換効率を良くしようという試み。" )

(defvar skk-relation-length (* skk-kanji-len 10)
  "skk-relation 使用時に、何文字前の変換まで憶えておくかを指定する変数。" )

(defvar skk-relation-record-num 100
  "skk-relation 使用時に、何エントリまでファイルに記憶するかを示す。" )

;; ---- buffer local variables
;; <フラグ類>
;;;###skk-autoload
(skk-deflocalvar skk-mode nil
  "Non-nil であれば、カレントバッファで現在 skk-mode を起動していることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-ascii-mode nil
  "Non-nil であれば、入力モードが ASCII モードであることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-j-mode nil
  "Non-nil であれば、入力モードがかな・カナモードであることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-katakana nil
  "Non-nil であれば、入力モードがカナモードであることを示す。
\"(and (not skk-katakana) skk-j-mode))\" が t であれば、かなモードであることを
示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-zenkaku-mode nil
  "Non-nil であれば、入力モードが全英モードであることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil であれば、入力モードが SKK abbrev モードであることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-okurigana nil
  "Non-nil であれば、送り仮名部分が入力中であることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-on nil
  "Non-nil であれば、▽モード (変換対象の文字列決定のためのモード) であることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-active nil
  "Non-nil であれば、▼モード (変換中) であることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil なら確定して良い候補を見つけた状態であることを指す。
skk-henkan, skk-search-kakutei-jisyo-file, skk-henkan-show-candidates,
skk-henkan-in-minibuff と skk-kakutei-save-and-init-variables で変更、参照され
る。" )

(skk-deflocalvar skk-exit-show-candidates nil
  "ミニバッファで候補を次々に表示して、候補が尽きたときに non-nil となる。
その値はリストで、car に skk-henkan-show-candidate 関数で while ループを回っ
た回数を示す一時変数 loop の値を、cdr 部に最後にミニバッファに表示した 1 つ前
の候補群の最後の要素を指すインデクスが代入される。
skk-henkan-show-candidates, skk-henkan-in-minibuff と
skk-kakutei-save-and-init-variables で変更、参照される。" )

(skk-deflocalvar skk-last-henkan-result nil
  "" )

(skk-deflocalvar skk-last-henkan-point nil
  "" )

;; <キーマップ関連>

;; <辞書関連の変数>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK 辞書の送り有りエントリの開始点を示すバッファポイント。")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK 辞書の送り有りエントリの終了点を示すバッファポイント。
skk-jisyo のバッファでは辞書の更新の必要があるためにマーカーが代入される。" )

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK 辞書の送りなしエントリの開始点を示すバッファポイント。
skk-jisyo のバッファでは辞書の更新の必要があるためにマーカーが代入される。" )

;; <その他>
(skk-deflocalvar skk-mode-line nil
  "SKK のモードを示すモードラインの文字列。
skk-mode-string, skk-hirakana-mode-string, skk-katakana-mode-string
and skk-zenkaku-mode-string のいずれかが代入される。" )

;; "" に対応したエントリが skk-roma-kana-[aiue] にあるため、"" を nil で代用
;; できない。
;;;###skk-autoload
(skk-deflocalvar skk-prefix ""
  "入力するかなを決定するためのプレフィックス。
後で入力される母音に対応した skk-roma-kana-[aiue] 連想リストで、その
skk-prefix をキーにして入力すべきかな文字が決定される。
例えば、\"か\" のように \"k\" から始まる子音を入力しているときは、skk-prefix
は、\"k\" で、その次に入力された母音 \"a\" に対応する skk-roma-kana-a の中の
\"k\" をキーに持つ値、\"か\" もしくは \"カ\" が入力すべきかな文字となる。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-start-point nil
  "変換開始ポイントを示すマーカー。" )

(skk-deflocalvar skk-henkan-end-point nil
  "変換終了ポイントを示すマーカー。" )

;;;###skk-autoload
(skk-deflocalvar skk-kana-start-point nil
  "かな文字の開始ポイントを示すマーカー。" )

(skk-deflocalvar skk-okurigana-start-point nil
  "送り仮名の開始ポイントを示すマーカー。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-key nil
  "変換すべき見出し語。
例えば、\"▽かな\" を変換すれば、skk-henkan-key には \"かな\" が代入される。
\"▽わら*う\" のような送りありの変換の場合には、\"わらu\" のように、漢字部分の
読みがな + 送り仮名の最初の文字のローマ字のプレフィックスが代入される。" )

;;;###skk-autoload
(skk-deflocalvar skk-okuri-char nil
  "変換すべき語の送り仮名の部分のプレフィックス。
例えば、\"おく*り\" を変換するときは、skk-okuri-char は \"r\"。
skk-okuri-char が non-nil であれば、送りありの変換であることを示す。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-okurigana nil
  "現在の変換の送り仮名部分。
例えば、\"▽うまれ*る\" を変換すれば、skk-henkan-okurigana には \"る\" が代入
される。" )

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "確定辞書により最後に確定したときの見出し語。
確定辞書による確定の直後に x キーを押すと確定がアンドゥされて、確定前の状態で
この見出し語がカレントバッファに挿入される。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-list nil
  "変換結果の候補のリスト。
例えば、\"▽な*く\" という変換すれば、skk-henkan-list は
(\"鳴\" \"泣\" \"無\" \"亡\") のようになる。" )

;;;###skk-autoload
(skk-deflocalvar skk-henkan-count -1
  "skk-henkan-list のリストのインデクスで現在の候補を差すもの。" )

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "skk-self-insert などで連続入力した文字数を表わすカウンター。
Emacs のオリジナルの動作では、self-insert-command にバインドされたキー入力は、
連続 20 回までが 1 つのアンドゥの対象となる。この動作をエミュレートするための
カウンター。
skk-self-insert 以外では、skk-abbrev-comma, skk-abbrev-period, skk-insert-a,
skk-insert-comma, skk-insert-e, skk-insert-i, skk-insert-period, skk-insert-u,
skk-kana-input, skk-set-henkan-point, skk-zenkaku-insert のいずれかのコマンド
で入力された場合も連続した入力として扱われる。
このカウンターが、20 以下であるときは、入力のたびに cancel-undo-boundary がコー
ルされる。" )

;;;###skk-autoload
(skk-deflocalvar skk-current-search-prog-list nil
  "skk-search-prog-list の現在の値を保存するリスト。
最初の変換時は skk-search-prog-list の全ての値を保持し、変換を繰り返すたびに 1
つづつ短くなってゆく。" )

;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-key nil
  "skk-henkan-key の最後の値。skk-undo-kakutei で参照される。" )

(skk-deflocalvar skk-last-henkan-okurigana nil
  "skk-henkan-okurigana の最後の値。skk-undo-kakutei で参照される。" )

(skk-deflocalvar skk-last-henkan-list nil
  "skk-henkan-list の最後の値。skk-undo-kakutei で参照される。" )

(skk-deflocalvar skk-last-okuri-char nil
  "skk-okuri-char の最後の値。skk-undo-kakutei で参照される。" )

(skk-deflocalvar skk-henkan-overlay nil
  "候補を表示するときに使用する Overlay。" )

;;;###skk-autoload
(defvar skk-menu-modified-user-option nil
  "SKK メニューコマンドで変更されたユーザー変数保持するリスト。"  )

(or (assq 'skk-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(skk-mode skk-input-mode-string) minor-mode-alist) ))

;;      +----------------------+-------- skk-mode -----+----------------------+
;;      |                      |                       |                      |
;;      |                      |                       |                      |
;;  skk-j-mode           skk-ascii-mode          skk-zenkaku-mode      skk-abbrev-mode
;;                           ASCII                ZENKAKU EIMOJI          ABBREVIATION
;;                  (C-j wakes up skk-j-mode)
;;
;;  skk-j-mode-map       skk-ascii-mode-map      skk-zenkaku-mode-map  skk-abbrev-mode-map
;;   skk-katakana: nil 
;;    HIRAKANA
;;
;;  skk-j-mode-map
;;   skk-katakana: t
;;    KATAKANA

;; sub minor mode
;;(cond ((and skk-xemacs (local-variable-p 'minor-mode-map-alist nil t)))
;;      ((local-variable-p 'minor-mode-map-alist)
;;       (setq-default minor-mode-map-alist minor-mode-map-alist) ))

(or (assq 'skk-ascii-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'skk-ascii-mode skk-ascii-mode-map) minor-mode-map-alist) ))

(or (assq 'skk-abbrev-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'skk-abbrev-mode skk-abbrev-mode-map) minor-mode-map-alist) ))

(or (assq 'skk-j-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'skk-j-mode skk-j-mode-map) minor-mode-map-alist) ))

(or (assq 'skk-zenkaku-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'skk-zenkaku-mode skk-zenkaku-mode-map) minor-mode-map-alist) ))

;;;; aliases
(defalias 'skk-backward-char 'backward-char)
(defalias 'skk-eventp 'eventp)
(defalias 'skk-forward-char 'forward-char)
(defalias 'skk-insert-and-inherit 'insert-and-inherit)
(defalias 'skk-skip-chars-backward 'skip-chars-backward)

;;;; macros

;; Why I use non-intern temporary variable in the macro --- see comment in
;; save-match-data of subr.el of GNU Emacs. And should we use the same manner
;; in the save-current-buffer, with-temp-buffer and with-temp-file macro
;; definition?
;;;###skk-autoload
(defmacro skk-save-point (&rest body)
  (` (let ((skk-save-point (point-marker)))
       (unwind-protect
	   (progn (,@ body))
	 (goto-char skk-save-point)
         (skk-set-marker skk-save-point nil) ))))

;;;###skk-autoload
(defmacro skk-message (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示する。
  ;; ARG は message 関数の第２引数以降の引数として渡される。
  (list 'let (list (list 'mc-flag t) (list 'enable-multibyte-characters t))
        (append (list 'message (list 'if 'skk-japanese-message-and-error
                                     japanese english ))
                arg )))

;;;###skk-autoload
(defmacro skk-error (japanese english &rest arg)
  ;; skk-japanese-message-and-error が non-nil だったら JAPANESE を nil であれ
  ;; ば ENGLISH をエコーエリアに表示し、エラーを発生させる。
  ;; ARG は error 関数の第２引数以降の引数として渡される。
  (list 'let (list (list 'mc-flag t) (list 'enable-multibyte-characters t))
        (append (list 'error (list 'if 'skk-japanese-message-and-error
                                   japanese english ))
                arg )))

;;;###skk-autoload
(defmacro skk-yes-or-no-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして yes-or-no-p を実行する。
  ;; yes-or-no-p の引数のプロンプトが複雑に入れ込んでいる場合はこのマクロを使
  ;; うよりオリジナルの yes-or-no-p を使用した方がコードが複雑にならない場合が
  ;; ある。
  (list 'let (list (list 'mc-flag t) (list 'enable-multibyte-characters t))
        (list 'yes-or-no-p (list 'if 'skk-japanese-message-and-error
                                 japanese english ))))

;;;###skk-autoload
(defmacro skk-y-or-n-p (japanese english)
  ;; skk-japanese-message-and-error が non-nil であれば、japanese を nil であ
  ;; れば english をプロンプトとして y-or-n-p を実行する。
  (list 'let (list (list 'mc-flag t) (list 'enable-multibyte-characters t))
        (list 'y-or-n-p (list 'if 'skk-japanese-message-and-error
                              japanese english ))))

;;;###skk-autoload
(defmacro skk-set-marker (marker position &optional buffer)
  ;; バッファローカル値である skk-henkan-start-point, skk-henkan-end-point,
  ;; skk-kana-start-point, あるいは skk-okurigana-start-point が nil だったら、
  ;; 新規マーカーを作って代入する。
  ;;
  ;; skk.el のバッファローカル値の扱いには注意すべき点がある。
  ;; 例えば、あるバッファ Buffer A で下記のようなフォームを評価したとする。
  ;; ---------- Buffer A ---------------+--------------- Buffer B ----------
  ;; (setq test (make-marker))          |
  ;;  -> #<marker in no buffer>         |
  ;;                                    |
  ;; (make-variable-buffer-local 'test) |
  ;;                                    |
  ;; test                               | test
  ;;  -> #<marker in no buffer>         |  -> #<marker in no buffer>
  ;;                                    |
  ;; (set-marker test (point))          |
  ;;                                    |
  ;; test                               | test
  ;;  -> #<marker at 122 in A>          |  -> #<marker at 122 in A>
  ;;
  ;; バッファローカル値としての宣言をする前に non-nil 値を代入し、その non-nil
  ;; 値を直接書き変えるようなフォームを評価すると Buffer B から見えるディフォル
  ;; ト値まで書き変ってしまう。上記の例はマーカーだが、下記のようにリストに対し
  ;; て破壊的関数で操作したときも同様の結果となる。
  ;; ---------- Buffer A ---------------+--------------- Buffer B ----------
  ;; (setq test '(A B C))               |
  ;;  -> (A B C)                        |
  ;;                                    |
  ;; (make-variable-buffer-local 'test) |
  ;;                                    |
  ;; test                               | test
  ;;  -> (A B C)                        |  -> (A B C)
  ;;                                    |
  ;; (setcar test 'X)                   |
  ;;                                    |
  ;; test                               | test
  ;;  -> (X B C)                        |  -> (X B C)
  ;;
  ;; この現象で一番困るのは、漢字登録などでミニバッファに入ったとき
  ;; (skk-henkan-show-candidate のように単に「エコーエリア」を使用する関数では
  ;; 関係ない) に、もとのバッファとミニバッファとではそれぞれ別の変換を行なう
  ;; のが普通であるので、上記のように他のバッファのバッファローカル値まで書き
  ;; 変えてしまうと、変換を休止している他のバッファで正常な変換ができなくなる
  ;; 場合があることである。
  ;;
  ;; しかも SKK ではリカーシブミニバッファが使用できるので、 *Minibuf-0* と
  ;;  *Minibuf-1 の間 (あるいはもっと深いリカーシブミニバッファ同士の間) でバッ
  ;; ファローカル値の破壊的書き変えが行なわれてしまい、上位のミニバッファに戻っ
  ;; たときに正常な変換ができなくなる場合がある。
  ;;
  ;; ところが下記のように初期値を nil にして、バッファローカル値としての宣言後、
  ;; non-nil 値を代入すれば、以後そのバッファローカル値に破壊的操作をしてもその
  ;; バッファに固有の値しか変化しない。
  ;; ---------- Buffer A ---------------+--------------- Buffer B ----------
  ;; (setq test nil)                    |
  ;;                                    |
  ;; (make-variable-buffer-local 'test) |
  ;;                                    |
  ;; test                               | test
  ;;  -> nil                            |  -> nil
  ;;                                    |
  ;; (setq test (make-marker))          |
  ;;  -> #<marker in no buffer>         |
  ;;                                    |
  ;; (set-marker test (point))          |
  ;;                                    |
  ;; test                               | test
  ;;  -> #<marker at 122 in A>          |  -> nil
  ;;
  ;; skk.el 9.3 の時点では、skk-henkan-start-point, skk-henkan-end-point,
  ;; skk-kana-start-point 及び skk-okurigana-start-point の初期値
  ;; (make-variable-buffer-local がコールされる前の値) が make-marker の返り値
  ;; である #<marker in no buffer> であったので、リカーシブミニバッファに入っ
  ;; て変換したときに "▼" が消えない、などのトラブルがあったが、これらの初期
  ;; 値を nil にして使用時に make-marker の返り値を代入するようにし、この問題を
  ;; 解決した。
  (list 'progn
        (list 'if (list 'not marker)
              (list 'setq marker (list 'make-marker)) )
        (list 'set-marker marker position buffer) ))

;;;; inline functions
(defsubst skk-mode-off ()
  (setq skk-mode nil
        skk-abbrev-mode nil
        skk-ascii-mode nil
        skk-j-mode nil
        skk-zenkaku-mode nil
        ;; j's sub mode.
        skk-katakana nil )
  ;; initialize
  (setq skk-input-mode-string skk-hirakana-mode-string)
  (setq skk-last-henkan-result nil)
  (skk-set-marker skk-last-henkan-point nil)
  (skk-set-cursor-color skk-default-cursor-color)
  (force-mode-line-update) )

;;;###skk-autoload
(defsubst skk-j-mode-on (&optional katakana)
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-ascii-mode nil
        skk-j-mode t
        skk-zenkaku-mode nil
        ;; j's sub mode.
        skk-katakana katakana )
  ;; mode line
  (if katakana
      (progn
        (setq skk-input-mode-string skk-katakana-mode-string)
        (skk-set-cursor-color skk-katakana-cursor-color) ) 
    (setq skk-input-mode-string skk-hirakana-mode-string)
    (skk-set-cursor-color skk-hirakana-cursor-color) )
  (force-mode-line-update) )
        
;;;###skk-autoload
(defsubst skk-ascii-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-ascii-mode t
        skk-j-mode nil
        skk-zenkaku-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-ascii-mode-string )
  (skk-set-cursor-color skk-ascii-cursor-color)
  (force-mode-line-update) )

;;;###skk-autoload
(defsubst skk-zenkaku-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode nil
        skk-ascii-mode nil
        skk-j-mode nil
        skk-zenkaku-mode t
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-zenkaku-mode-string )
  (skk-set-cursor-color skk-zenkaku-cursor-color)
  (force-mode-line-update) )

;;;###skk-autoload
(defsubst skk-abbrev-mode-on ()
  (setq skk-mode t
        skk-abbrev-mode t
        skk-ascii-mode nil
        skk-j-mode nil
        skk-zenkaku-mode nil
        ;; j's sub mode.
        skk-katakana nil
        skk-input-mode-string skk-abbrev-mode-string )
  (skk-set-cursor-color skk-abbrev-cursor-color)
  (force-mode-line-update) )

;;;###skk-autoload
(defsubst skk-in-minibuffer-p ()
  ;; カレントバッファがミニバッファかどうかをチェックする。
  (window-minibuffer-p (selected-window)) )

;;;###skk-autoload
(defsubst skk-insert-prefix (&optional char)
  ;; skk-echo が non-nil であればカレントバッファに skk-prefix を挿入する。
  (if skk-echo
      ;; skk-prefix の挿入をアンドゥの対象としない。挿入したプレフィックスは、
      ;; かな文字を挿入する前に全て消去するので、その間、buffer-undo-list を
      ;; t にしてアンドゥ情報を蓄えなくとも問題がない。
      (let ((buffer-undo-list t))
        (insert (or char skk-prefix)) )))

;;;###skk-autoload
(defsubst skk-erase-prefix ()
  ;; skk-echo が non-nil であればカレントバッファに挿入された skk-prefix を消
  ;; す。
  (if skk-echo
      ;; skk-prefix の消去をアンドゥの対象としない。
      (let ((buffer-undo-list t))
        (delete-region skk-kana-start-point (point) ))))

(defsubst skk-string<= (str1 str2)
  ;; str1 が str2 と比較して、string< か string= であれば、t を返す。
  (or (string< str1 str2) (string= str1 str2)) )

(defsubst skk-jis-char-p (char)
  ;; char が JIS 文字だったら t を返す。
  (> char 127) )

(defsubst skk-alpha-char-p (char)
  ;; char が ascii 文字だったら t を返す。
  (<= char 127) )

(defsubst skk-lower-case-p (char)
  ;; char が小文字のアルファベットであれば、t を返す。
  (and (<= ?a char) (>= ?z char) ))

(defsubst skk-do-auto-fill ()
  ;; auto-fill-function に値が代入されておれば、do-auto-fill をコールする。
  (and auto-fill-function (funcall auto-fill-function)) )

;;;; from dabbrev.el.  Welcome!
;; 判定間違いを犯す場合あり。要改良。
(defsubst skk-minibuffer-origin ()
  (nth 1 (buffer-list)) )

;;;###skk-autoload
(defsubst skk-numeric-p ()
  (and skk-use-numeric-conversion (require 'skk-num)
       skk-num-list ))

(defsubst skk-substring-head-character (string)
  (char-to-string (string-to-char string)) )

(defsubst skk-get-simply-current-candidate (&optional noconv)
  (if (> skk-henkan-count -1)
      ;; (nth -1 '(A B C)) は、A を返すので、負でないかどうかチェックする。
      (let ((word (nth skk-henkan-count skk-henkan-list)))
        (and word
             (if (and (skk-numeric-p) (consp word))
                 (if noconv (car word) (cdr word))
               word )))))

(eval-after-load "font-lock"
  '(mapcar (function
            (lambda (pattern)
              (add-to-list
               'lisp-font-lock-keywords-2
               (cons pattern
                     '((1 font-lock-keyword-face)
                       (2 font-lock-variable-name-face) )))))
           '("^(\\(skk-deflocalvar\\)[ \t'\(]*\\(\\sw+\\)?"
             "^(\\(skk-defunsoft\\)[ \t'\(]*\\(\\sw+\\)?" )))

(defun skk-submit-bug-report ()
  "メールで SKK のバグレポートを送る。
reporter-mailer を設定することにより好みのメールインターフェイスを使用すること
ができる。例えば、Mew を使用したい場合は下記のように設定する。

    \(setq reporter-mailer '\(mew-send reporter-mail\)\)

reporter.el 3.2 では、変数 reporter-mailer がなくなった。このバージョンでは、

    \(setq mail-user-agent 'mew-user-agent\)

と指定する。"
  (interactive)
  (require 'reporter)
  (if (and (boundp 'mail-user-agent)
           (eq mail-user-agent 'mew-user-agent) )
      (define-mail-user-agent 'mew-user-agent
        'mew-send 'mew-draft-send-letter 'mew-draft-kill ))
  (and (y-or-n-p "Do you really want to submit a report on SKK? ")
       (reporter-submit-bug-report
        skk-ml-address
        (concat "skk.el " (skk-version)
                (if (or (and (boundp 'skk-server-host) skk-server-host)
                        (and (boundp 'skk-servers-list) skk-servers-list)
                        (getenv "SKKSERVER")
                        (getenv "SKKSERV") )
                    (progn
                      (require 'skk-server)
                      (concat ", skkserv; " (skk-server-version)
                              (if (getenv "SKKSERVER")
                                  (concat ",\nSKKSERVER; "
                                          (getenv "SKKSERVER") ))
                              (if (getenv "SKKSERV")
                                  (concat ", SKKSERV; "
                                          (getenv "SKKSERV") ))))))
        (let ((base (list 'window-system
                          'skk-auto-okuri-process
                          'skk-auto-start-henkan
                          'skk-egg-like-newline
                          'skk-henkan-okuri-strictly
                          'skk-henkan-strict-okuri-precedence
                          'skk-kakutei-early
                          'skk-process-okuri-early
                          'skk-search-prog-list
                          'skk-use-face
                          'skk-use-vip )))
          (if (boundp 'skk-henkan-face)
              (nconc base '(skk-henkan-face)) )
          (if (boundp 'skk-server-host)
              (nconc base '(skk-server-host)) )
          (if (boundp 'skk-server-prog)
              (nconc base '(skk-server-prog)) )
          (if (boundp 'skk-servers-list)
              (nconc base '(skk-servers-list)) )
          base ))))

;;;; defadvices.
;; defadvice で定義すると、後でユーザーが新規の機能を付けて更に defadvice して
;; もちゃんと動く。

;; cover to original functions.

(defadvice keyboard-quit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ keyboard-quit と同じ動作をする。"
  (cond ((not skk-henkan-on)
         (with-current-buffer (skk-minibuffer-origin)
           (skk-set-cursor-properly) )
         ad-do-it )
        (skk-henkan-active
         (setq skk-henkan-count 0)
         (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
             (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
               (skk-previous-candidate)
               ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
               (delete-backward-char count) )
           (skk-previous-candidate) ))
        (t (if (> (point) skk-henkan-start-point)
               (delete-region (point) skk-henkan-start-point) )
           (skk-kakutei) )))

(defadvice abort-recursive-edit (around skk-ad activate)
  "▼モードであれば、候補の表示をやめて▽モードに戻す (見出し語は残す)。
▽モードであれば、見出し語を削除する。
上記のどちらのモードでもなければ abort-recursive-edit と同じ動作をする。"
  (cond ((not skk-henkan-on)
         (with-current-buffer (skk-minibuffer-origin)
           (skk-set-cursor-properly) )
         ad-do-it )
        (skk-henkan-active
         (setq skk-henkan-count 0)
         (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
             (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
               (skk-previous-candidate)
               ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
               (delete-backward-char count) )
           (skk-previous-candidate) ))
        (t (if (> (point) skk-henkan-start-point)
               (delete-region (point) skk-henkan-start-point) )
           (skk-kakutei) )))

(defadvice newline (around skk-ad activate)
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((arg (ad-get-arg 0))
          ;; skk-kakutei を実行すると skk-henkan-on の値が無条件に nil になる
          ;; ので、保存しておく必要がある。
          (no-newline (and skk-egg-like-newline skk-henkan-on))
	  (auto-fill-function auto-fill-function) )
      (if (not (interactive-p))
	  (setq auto-fill-function nil) )
      (if (skk-kakutei)
          ;; skk-do-auto-fill によって行が折り返されたら arg を 1 つ減らす。
	  ;; fill されても nil が帰ってくる :-<
          (setq arg (1- arg)) )
      (if (not no-newline)
          (progn
            (ad-set-arg 0 arg)
            ad-do-it )))))

(defadvice newline-and-indent (around skk-ad activate)
  (if (and skk-egg-like-newline skk-henkan-on)
      (newline)
    ad-do-it))

(defadvice exit-minibuffer (around skk-ad activate)
  (if (not (or skk-j-mode skk-abbrev-mode))
      ad-do-it
    (let ((no-newline (and skk-egg-like-newline skk-henkan-on)))
      (if skk-mode (skk-kakutei))
      (or no-newline ad-do-it) )))

(defadvice delete-backward-char (around skk-ad activate)
  "現在のポイントから戻って COUNT 文字を消去する。"
  (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
    (cond ((and skk-henkan-on (>= skk-henkan-start-point (point)))
           (setq skk-henkan-count 0)
           (skk-kakutei) )
          (skk-henkan-active
           (if (and (not skk-delete-implies-kakutei)
                    (= skk-henkan-end-point (point)) )
               (skk-previous-candidate)
             ;;(if skk-use-face (skk-henkan-face-off))
             ;; overwrite-mode で全角文字全角文字に囲まれ、かつ直前の文字が全
             ;; 角文字であるポイントで delete-backward-char を使うと、全角文字
             ;; は消すが半角文字分しか backward 方向にポイントが戻らない
             ;; (Emacs 19.31 にて確認)。変換中の候補に対しては
             ;; delete-backward-char で必ず全角文字 1 文字分 backward 方向に戻っ
             ;; た方が良い。
             (if overwrite-mode
                 (progn
                   (backward-char count)
                   (delete-char count) )
               ad-do-it )
             (if (>= skk-henkan-end-point (point)) (skk-kakutei)) ))
          ;; 入力中の見出し語に対しては delete-backward-char で必ず全角文字 1
          ;; 文字分 backward 方向に戻った方が良い。
          ((and skk-henkan-on overwrite-mode)
           (backward-char count)
           (delete-char count) )
          (t ad-do-it) )))

(defadvice save-buffers-kill-emacs (before skk-ad activate)
  "SKK 辞書をセーブして、Emacs を終了する。
セーブ後、skk-before-kill-emacs-hook を実行してから Emacs をキルする。"
  ;; defadvice する際、被 advice 関数とは違う方法で引数渡しをしたい場合以外は、
  ;; interactive + descripter は要らないみたい。
  ;;(interactive "P")
  (skk-save-jisyo)
  (run-hooks 'skk-before-kill-emacs-hook) )

(defadvice picture-mode-exit (before skk-ad activate)
  "SKK のバッファローカル変数を無効にし、picture-mode-exit をコールする。
picture-mode から出たときにそのバッファで SKK を正常に動かすための処理。"
  (if skk-mode (skk-kill-local-variables)) )

(defadvice undo (before skk-ad activate)
  "SKK モードが on なら skk-self-insert-non-undo-count を初期化する。"
  (if skk-mode
      (setq skk-self-insert-non-undo-count 0) ))

(defadvice kill-buffer (around skk-ad activate)
  "SKK の▼モードだったら、確定してからバッファをキルする。
  バッファのキル後、SKK のモードに従いカーソルの色を変える。"
  (if skk-mode
      (and skk-henkan-on (skk-kakutei)) )
  ad-do-it
  ;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要
  ;; がある。
  (skk-set-cursor-properly) )

(defadvice overwrite-mode (after skk-ad activate)
  (if skk-use-cursor-change
      (skk-change-cursor-when-ovwrt) ))

(defadvice eval-expression (before skk-ad activate)
  (if skk-mode (skk-mode-off)) )

(defadvice query-replace-regexp  (before skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (add-hook 'minibuffer-setup-hook 'skk-setup-minibuffer) )

(defadvice query-replace (before skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (add-hook 'minibuffer-setup-hook 'skk-setup-minibuffer) )

(defadvice goto-line (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice yank (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice yank-pop (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice recenter (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要が
;; ある。
(defadvice bury-buffer (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (skk-set-cursor-properly) )

(defadvice switch-to-buffer (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (skk-set-cursor-properly) ) 

;; cover to hilit19 functions.
;; forward advice と automatic advice activation 機能があるから、hilit19.el 
;; のロード前に defadvice しても大丈夫。
;;(if (not (fboundp 'hilit-add-pattern))
;;    nil
(defadvice hilit-yank (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice hilit-yank-pop (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice hilit-recenter (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (and skk-mode (skk-set-cursor-properly)) )

(defadvice execute-extended-command (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (skk-set-cursor-properly) )

(defadvice pop-to-buffer (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (skk-set-cursor-properly) )

;; abort-recursive-edit では、after original command へ移行する前にアボート
;; してしまう。
;;(defadvice abort-recursive-edit (after skk-ad activate)
;;  "SKK のモードに従いカーソルの色を変える。"
;;  (skk-set-cursor-properly) )
;;
(defadvice abort-recursive-edit (before skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
   ;; 複数の window を開いている場合などは、誤動作の可能性あり。
  (with-current-buffer (skk-minibuffer-origin)
    (skk-set-cursor-properly) ))

(defadvice other-window (after skk-ad activate)
  "SKK のモードに従いカーソルの色を変える。"
  (skk-set-cursor-properly) )

(if skk-xemacs
    ;; XEmacs has minibuffer-keyboard-quit that has nothing to do with delsel.
    (defadvice minibuffer-keyboard-quit (around skk-ad activate)
      (cond ((or (string= skk-henkan-key "") (not skk-henkan-on))
             (with-current-buffer (skk-minibuffer-origin)
               (skk-set-cursor-properly) )
             ad-do-it )
            (skk-henkan-active
             (setq skk-henkan-count 0)
             (if (and skk-delete-okuri-when-quit skk-henkan-okurigana)
                 (let ((count (/ (length skk-henkan-okurigana) skk-kanji-len)))
                   (skk-previous-candidate)
                   ;; ここでは delete-backward-char に第二引数を渡さない方がベター？
                   (delete-backward-char count) )
               (skk-previous-candidate) ))
            (t (if (> (point) skk-henkan-start-point)
                   (delete-region (point) skk-henkan-start-point) )
               (skk-kakutei) )))
    (defadvice minibuffer-keyboard-quit (around skk-ad activate)
      ;; for delsel.el
      (if (and skk-mode
               (not (and
                     delete-selection-mode transient-mark-mode mark-active )))
          (keyboard-quit)
        ad-do-it )))

;;;; mode setup

;;;###skk-autoload
(defun skk-mode (&optional arg)
  "日本語入力モード。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
負の引数を与えると SKK モードから抜ける。

An input mode for Japanese, converting romanized phonetic strings to kanji.

A minor mode, it should not affect the use of any major mode or
orthogonal minor modes.

In the initial SKK mode, hiragana submode, the mode line indicator is 
「かな」.  Lowercase romaji entry is automatically converted to
hiragana where possible.  The lowercase characters `q' and `l' change
submodes of SKK, and `x' is used as a prefix indicating a small kana.

`q' is used to toggle between hiragana and katakana (mode line
indicator 「カナ」) entry submodes.

`l' is used to enter ASCII submode (mode line indicator \"SKK\").
Uppercase `L' enters zenkaku (wide) ASCII submode (mode line indicator 
「全英」).  `\C-j' returns to hiragana submode from either ASCII submode.

Kanji conversion is complex, but the basic principle is that the user
signals the appropriate stem to be matched against dictionary keys by
the use of uppercase letters.  Because SKK does not use grammatical
information, both the beginning and the end of the stem must be marked.

For non-inflected words (eg, nouns) consisting entirely of kanji, the
simplest way to invoke conversion is to enter the reading of the kanji,
the first character only in uppercase.  A leading 「▽」 indicates that
kanji conversion is in progress.  After entering the reading, press 
space.  This invokes dictionary lookup, and the hiragana reading will be
redisplayed in kanji as the first candidate.  Pressing space again gives
the next candidate.  Further presses of space produce further candidates,
as well as a list of the next few candidates in the minibuffer.  Eg,
\"Benri\" => 「▽べんり」, and pressing space produces 「▼便利」 (the solid 
triangle indicates that conversion is in progress).  Backspace steps 
through the candidate list in reverse.

A candidate can be accepted by pressing `\C-j', or by entering a
self-inserting character.  (Unlike other common Japanese input methods,
RET not only accepts the current candidate, but also inserts a line
break.)

Inflected words (verbs and adjectives), like non-inflected words, begin
entry with a capital letter.  However, for these words the end of the
kanji string is signaled by capitalizing the next mora.  Eg, \"TuyoI\"
=> 「▼強い」.  If no candidate is available at that point, the inflection
point will be indicated with an asterisk \"*\", and trailing characters
will be displayed until a candidate is recognized.  It will be
immediately displayed (pressing space is not necessary).  Space and
backspace are used to step forward and backward through the list of 
candidates.

For more information, see the `skk' topic in Info.  (Japanese only.)

A tutorial is available in Japanese or English via \"M-x skk-tutorial\".
Use a prefix argument to be prompted for the language.  The default is
system-dependent.
"

  (interactive "P")
  (setq skk-mode (cond ((null arg) (not skk-mode))
                       ;; - は -1 に変換される。
                       ((> (prefix-numeric-value arg) 0) t) ))
  (if (not skk-mode)
      ;; exit skk-mode
      (progn
        (let ((skk-mode t)) (skk-kakutei))
        (skk-mode-off) )
    ;; enter skk-mode
    (if (not skk-mode-invoked)
        ;; enter skk-mode for the first time in this session
        (progn
          (setq skk-mode-invoked t)
          (skk-setup-init-file)
          (load skk-init-file t)
          (if skk-keep-record
              (skk-create-file
               skk-record-file
               (if skk-japanese-message-and-error
                   "SKK の記録用ファイルを作りました"
                 "I have created an SKK record file for you" )))
          (skk-create-file
           skk-jisyo (if skk-japanese-message-and-error
                         "SKK の空辞書を作りました"
                       "I have created an empty SKK Jisyo file for you" ))
          (skk-get-jisyo-buffer skk-jisyo 'nomsg) ))
    ;;以下は skk-mode に入るたびに毎度コールされるコード。
    ;;(if (boundp 'disable-undo)
    ;;    (make-local-variable 'disable-undo) )
    (cond (skk-use-vip (skk-vip-mode))
          (skk-use-viper
           (require 'skk-viper)
           (funcall skk-viper-normalize-map-function) ))
    (if (and (not (featurep 'skk-server))
             (or (and (boundp 'skk-server-host) skk-server-host)
                 (and (boundp 'skk-servers-list) skk-servers-list)
                 (getenv "SKKSERVER")
                 (getenv "SKKSERV") ))
        (require 'skk-server) )
    ;; ユーザー変数に関するものは、ユーザーがいつ、それらの変数を変更するか予
    ;; 測が付かないので、skk-mode に入るたびに設定しなおしている。
    (if (featurep 'skk-server)
        ;; skk-search-server はサーバーが落ちても使えるので、外さない。
        (skk-adjust-search-prog-list-for-server-search 'non-del) )
    (if skk-auto-okuri-process
        (skk-adjust-search-prog-list-for-auto-okuri) )
    (if skk-use-relation
	(progn
	 (require 'skk-attr)
	 (setq skk-search-prog-list
	       (cons '(skk-search-relation) skk-search-prog-list) )))
    (skk-setup-delete-selection-mode)
    (skk-setup-special-midashi-char)
    (skk-setup-auto-paren)
    (skk-adjust-user-option)
    (define-key minibuffer-local-map skk-kakutei-key 'skk-kakutei)
    ;;(define-key minibuffer-local-map "\C-m" 'skk-newline)
    (define-key minibuffer-local-completion-map skk-kakutei-key 'skk-kakutei)
    ;;(define-key minibuffer-local-completion-map "\C-m" 'skk-newline)
    ;; XEmacs doesn't have minibuffer-local-ns-map
    (if (boundp 'minibuffer-local-ns-map)
        ;;(define-key minibuffer-local-ns-map "\C-m" 'skk-newline)
        (define-key minibuffer-local-ns-map skk-kakutei-key 'skk-kakutei) )
    (skk-j-mode-on)
    (run-hooks 'skk-mode-hook) ))

;;;###skk-autoload
(defun skk-auto-fill-mode (&optional arg)
  "日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に auto-fill-mode 及び SKK モードに入る。
負の引数を与えると auto-fill-mode 及び SKK モードから抜ける。"
  (interactive "P")
  (let ((auto-fill
         (cond ((null arg) (not auto-fill-function))
               ((> (prefix-numeric-value arg) 0) t) )))
    (auto-fill-mode (if auto-fill 1 -1))
    (skk-mode arg)
    (skk-set-cursor-color (if skk-mode
                              skk-hirakana-cursor-color
                            skk-default-cursor-color ))
    (run-hooks 'skk-auto-fill-mode-hook) ))

(defun skk-kill-emacs-without-saving-jisyo (&optional query)
  "SKK 辞書をセーブしないで、Emacs を終了する。
skk-before-kill-emacs-hook を実行してから Emacs をキルする。"
  (interactive "P")
  ;; format を引数に持たせた場合は、skk-yes-or-no-p を使うとかえって冗長になる。
  (if (yes-or-no-p
       (format (if skk-japanese-message-and-error
                   "辞書の保存をせずに %s を終了します。良いですか？"
                 "Do you really wish to kill %s without saving Jisyo? " )
               (cond (skk-mule "Mule")
                     (skk-xemacs "XEmacs")
                     (t "Emacs") )))
      (let ((buff (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
        (if buff
            (progn (set-buffer buff)
                   (set-buffer-modified-p nil)
                   (kill-buffer buff) ))
        (run-hooks 'skk-before-kill-emacs-hook)
        (ad-remove-advice 'save-buffers-kill-emacs 'before 'skk-ad)
        (ad-activate 'save-buffers-kill-emacs)
        (save-buffers-kill-emacs query) )))

(defun skk-setup-init-file ()
  ;; skk-byte-compile-init-file が non-nil の場合で、skk-init-file をバイトコ
  ;; ンパイルしたファイルが存在しないか、そのバイトコンパイル済ファイルより 
  ;; skk-init-file の方が新しいときは、skk-init-file をバイトコンパイルする。
  ;;
  ;; skk-byte-compile-init-file が nil の場合で、skk-init-file をバイトコンパ
  ;; イルしたファイルより skk-init-file の方が新しいときは、そのバイトコンパイ
  ;; ル済ファイルを消す。
  (save-match-data
    (let* ((init-file (expand-file-name skk-init-file))
           (elc (concat init-file 
                        (if (string-match "\\.el$" init-file)
                            "c"
                          ".elc" ))))
      (if skk-byte-compile-init-file
          (if (and (file-exists-p init-file)
                   (or (not (file-exists-p elc))
                       (file-newer-than-file-p init-file elc) ))
              (save-window-excursion ;; for keep window configuration.
                (skk-message "%s をバイトコンパイルします。"
                             "Byte-compile %s"
                             skk-init-file )
                (sit-for 2)
                (byte-compile-file init-file) ))
        (if (and (file-exists-p init-file)
                 (file-exists-p elc)
                 (file-newer-than-file-p init-file elc) )
            (delete-file elc) )))))

;;
;;(skk-setup-special-midashi-char skk-minibuff-map)

;;;###skk-autoload
(defun skk-emulate-original-map (arg)
  ;; キー入力に対して、SKK のモードではなく、Emacs のオリジナルのキー割り付けで
  ;; コマンドを実行する。
  (let ((prefix-arg arg)
        (keys (skk-command-key-sequence (this-command-keys) this-command)) )
    (if (not keys)
        ;; no alternative commands.  may be invoked by M-x.
        nil
      (let (skk-mode skk-ascii-mode skk-j-mode skk-abbrev-mode skk-zenkaku-mode
                     command )
        (setq command (key-binding keys))
        (if (eq command this-command)
            ;; avoid recursive calling of skk-emulate-original-map.
            nil
          ;; if no bindings are found, call `undefined'.  it's
          ;; original behaviour.
          (skk-cancel-undo-boundary)
          (command-execute (or command (function undefined))))))))

(defun skk-command-key-sequence (key command)
  ;; KEY から universal arguments を取り除き、COMMAND を実行するキーを返す。
  ;; `execute-extended-command' によってコマンドが実行された場合は、nil を返す。
  (while (not (or (zerop (length key))
                  (eq command (key-binding key))))
    (setq key (vconcat (cdr (append key nil)))))
  (and (not (zerop (length key))) key))

(defun skk-setup-special-midashi-char ()
  ;; skk-special-midashi-char-list に指定された char を skk-j-mode-map の
  ;; skk-set-henkan-point に割り付ける。skk-special-midashi-char-list に指定さ
  ;; れた char で、接頭辞、接尾辞の入力を可能にするための処理。
  (let ((strlist (mapcar 'char-to-string skk-special-midashi-char-list))
        ;; Use default-value for Viper.  It localizes minor-mode-map-alist.
        (map (cdr (assq 'skk-j-mode (default-value 'minor-mode-map-alist))))
        str )
    (while strlist
      (setq str (car strlist))
      (if (not (eq 'skk-set-henkan-point (lookup-key map str)))
          (define-key map str 'skk-set-henkan-point) )
      (setq strlist (cdr strlist)) )))

(defun skk-setup-delete-selection-mode ()
  ;; Delete Selection モードが SKK を使った日本語入力に対しても機能するように
  ;; セットアップする。
  (if (and (featurep 'delsel)
           (not (get 'skk-insert-a 'delete-selection)) )
      (progn
        ;;(put 'skk-delete-backward-char 'delete-selection 'supersede)
        (mapcar (function (lambda (func) (put func 'delete-selection t)))
                '(skk-input-by-code-or-menu
                  skk-insert-comma
                  skk-insert-period
                  skk-kana-input
                  ;;skk-newline
                  ;;skk-set-henkan-point-subr
                  skk-set-henkan-point
                  skk-self-insert
                  skk-today )))))

(defun skk-setup-auto-paren ()
  ;; skk-auto-paren-string-alist の中から、skk-special-midashi-char-list
  ;; の要素に関連するものを取り除く。
  ;; また、skk-auto-paren-string-alist の各要素の car の文字が ascii char であ
  ;; る場合は、skk-input-vector の該当の場所 (その ascii char を評価した数がイ
  ;; ンデクスとなる) にその文字を書き込む (本来は ascii char は
  ;; skk-input-vector に書く必要がないが、skk-auto-paren-string-alist に指定さ
  ;; れた対になる文字の挿入のためには、キーとなる文字を書いておく必要がある)。
  (if (null skk-auto-paren-string-alist)
      nil
    (let ((strlist (mapcar 'char-to-string skk-special-midashi-char-list))
          cell str alist )
      (while strlist
        (setq cell (assoc (car strlist) skk-auto-paren-string-alist))
        (if cell
            ;; assoc で抽出した cell を直接指定しているので、delete でなくとも大
            ;; 丈夫。
            (setq skk-auto-paren-string-alist
                  (delq cell skk-auto-paren-string-alist) ))
        (setq strlist (cdr strlist)) )
      (setq alist skk-auto-paren-string-alist)
      (while alist
        (setq str (car (car alist)))
        (if (and (eq (string-width str) 1)
                 ;; 既にユーザーが指定している場合は、どんな文字であっても (キー
                 ;; となる文字とは違っていても)、何もしない。
                 (not (aref skk-input-vector (string-to-char str))) )
            (aset skk-input-vector (string-to-char str) str) )
        (setq alist (cdr alist)) ))))

(defun skk-adjust-user-option ()
  ;; 両立できないオプションの調整を行なう。
  (if skk-process-okuri-early
      ;; skk-process-okuri-early の値が non-nil であるときに下記の値が non-nil
      ;; であれば正常に動かないのでこの変数の優先順位を高くした。
      (setq skk-kakutei-early nil
            skk-auto-okuri-process nil
            skk-henkan-okuri-strictly nil
	    skk-henkan-strict-okuri-precedence nil)))

(defun skk-try-completion (arg)
  "▽モードで見出し語の補完を行う。
それ以外のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "P")
  (if (and skk-henkan-on (not skk-henkan-active))
      (progn
        (setq this-command 'skk-completion)
        (skk-completion (not (eq last-command 'skk-completion))) )
    (skk-emulate-original-map arg) ))

(defun skk-ascii-mode ()
  "SKK のモードを ascii モードに変更する。"
  (interactive)
  (skk-kakutei)
  (skk-ascii-mode-on) )

(defun skk-zenkaku-mode ()
  "SKK のモードを全角英字入力モードに変更する。"
  (interactive)
  (skk-kakutei)
  (skk-zenkaku-mode-on) )

(defun skk-abbrev-mode ()
  "ascii 文字をキーにした変換を行うための入力モード。"
  (interactive "*")
  (if (and skk-henkan-on (not skk-henkan-active))
      (skk-error "既に▽モードに入っています" "Already in ▽ mode") )
  (skk-kakutei)
  (skk-set-henkan-point-subr)
  (skk-abbrev-mode-on) )

(defun skk-toggle-kana (arg)
  "ひらがなモードとカタカナモードをトグルで切り替える。
カタカナモードで変換を行なうときに、送り仮名をカタカナに変換したくないときは、
skk-convert-okurigana-into-katakana の値を non-nil にする。

▽モードでは、skk-henkan-start-point (▽の直後) とカーソルの間の文字列を

    ひらがな <=> カタカナ
    全角英数字 <=> ascii

のように変換する。"
  (interactive "P")
  (cond ((and skk-henkan-on (not skk-henkan-active))
         (let (char)
           (skk-save-point
             (goto-char skk-henkan-start-point)
             ;; "ー" では文字種別が判別できないので、ポイントを進める。
             (while (looking-at "ー")
               (skk-forward-char 1) )
             (setq char (skk-what-char-type)) )
           (skk-set-marker skk-henkan-end-point (point))
           (cond ((eq char 'hirakana)
                  (skk-katakana-henkan arg) )
                 ((eq char 'katakana)
                  (skk-hiragana-henkan arg) )
                 ((eq char 'ascii)
                  (skk-zenkaku-henkan arg) )
                 ((eq char 'zenkaku)
                  (skk-ascii-henkan arg) ))))
        ((and (skk-in-minibuffer-p) (not skk-j-mode))
         ;; ミニバッファへの初突入時。
         (skk-j-mode-on) )
        (t (setq skk-katakana (not skk-katakana))) )
  (skk-kakutei)
  (if skk-katakana
      (progn
        (setq skk-input-mode-string skk-katakana-mode-string)
        (skk-set-cursor-color skk-katakana-cursor-color) )
    (setq skk-input-mode-string skk-hirakana-mode-string)
    (skk-set-cursor-color skk-hirakana-cursor-color) )
  (force-mode-line-update) )

(defun skk-misc-for-picture ()
  ;; picture-mode へ入ったときに SKK を正常に動かすために必要な処理を行なう。
  ;; edit-picture-hook に add-hook して使用する。
  ;;
  ;; picture-mode で SKK を使用し漢字入力をした場合に、BS で全角文字が消せない
  ;; のは、SKK の不具合ではなく、picture.el の問題 (move-to-column-force 関数
  ;; の中で使用している move-to-column で全角文字を無視したカラム数が与えられ
  ;; たときにカーソル移動ができないから) である。消したい文字にポイントを合わ
  ;; せ、C-c C-d で一文字づつ消すしか方法はない。
  (if skk-mode
      ;; SKK 起動前の状態に戻す。
      (skk-kill-local-variables) ))

(defun skk-kill-local-variables ()
  ;; SKK 関連のバッファローカル変数を無効にする。
  ;; 既存のバッファを picture mode にしたとき、picture-mode 関数は
  ;; kill-all-local-variables 関数を呼ばないので、SKK 関連のバッファローカル
  ;; 変数が元のバッファの値のままになってしまう。そこで、picture mode に入った
  ;; ときにフックを利用してこれらのバッファローカル変数を kill する。
  ;; RMS は picture-mode で kill-all-local-variables 関数を呼ばないのは、バグ
  ;; ではない、と言っていた。
  (if (eq (nth 1 mode-line-format) 'skk-mode-line)
      (setq mode-line-format (delq 'skk-mode-line mode-line-format) ))
  (let ((lv (buffer-local-variables))
        v vstr )
    (while lv
      (setq v (car (car lv))
            lv (cdr lv)
            vstr (prin1-to-string v) )
      (if (and
           (> (length vstr) 3) (string= "skk-" (substring vstr 0 4)) )
          (kill-local-variable v) ))))

;;;; kana inputting functions

(defun skk-insert (table)
  ;; skk-prefix をキーとして、連想リスト TABLE から文字列を探して入力する。
  (let ((char (assoc skk-prefix table)))
    (if (null char)
        (progn
          ;; skk-prefix not found in the table
          (setq skk-prefix "")
          (skk-unread-event (skk-character-to-event last-command-char)) )
      (if (and skk-henkan-active skk-kakutei-early
               (not skk-process-okuri-early) )
          (skk-kakutei) )
      (skk-insert-str (if skk-katakana (nthcdr 2 char) (nth 1 char)))
      (if skk-okurigana
          (skk-set-okurigana)
        (setq skk-prefix "") )
      (if (not skk-henkan-on) (skk-do-auto-fill)) )))

(defun skk-insert-str (str)
  ;; skk-insert のサブルーチン。STR を挿入する。必要であれば
  ;; self-insert-after-hook をコールする。overwrite-mode であれば、適切に上書き
  ;; を行う。この関数は、skk-vip.el で上書きされる 
  (skk-cancel-undo-boundary)
  (skk-insert-and-inherit str)
  (if (and skk-henkan-on (not skk-henkan-active))
      (if (and skk-auto-start-henkan (not skk-okurigana))
          (skk-auto-start-henkan str) )
    (if (and (boundp 'self-insert-after-hook) self-insert-after-hook)
        (funcall self-insert-after-hook (- (point) (length str)) (point)) )
    (if overwrite-mode
        (skk-del-char-with-pad (skk-ovwrt-len (string-width str))) )))

(defun skk-auto-start-henkan (str)
  ;; skk-auto-start-henkan-keyword-list の要素の文字列を挿入したときに自動的に 
  ;; (スペースを打鍵しなくとも) 変換を開始する。エー×イソフト社の MSDOS 用 の 
  ;; FEP、WX2+ 風。
  (if (member str skk-auto-start-henkan-keyword-list)
      (skk-save-point
        (skk-backward-char 1)
        (if (> (point) skk-henkan-start-point)
            (let ((skk-prefix ""))
              (skk-start-henkan (prefix-numeric-value current-prefix-arg)) )))))

(defun skk-ovwrt-len (len)
  ;; 上書きして良い長さを返す。
  ;; この関数は、skk-vip.el で上書きされる (<(skk-vip.el/skk-ovwrt-len)>)。
  (min (string-width
        (skk-buffer-substring (point) (skk-save-point (end-of-line) (point))) )
       len ))

(defun skk-del-char-with-pad (length)
  ;; 長さ LENGTH の文字を消去する。調整のため、必要であれば、末尾にスペースを
  ;; 挿入する。
  (let ((p (point)) (len 0))
    (while (< len length)
      (forward-char 1)
      (setq len (string-width (skk-buffer-substring (point) p))))
    (delete-region p (point))
    (or (eq length len)
        (progn
          (insert " ")
          (backward-char 1)))))

(defun skk-cancel-undo-boundary ()
  ;; skk-insert-[aiue], skk-insert-comma, skk-insert-period, skk-kana-input,
  ;; skk-self-insert, skk-set-henkan-point, skk-zenkaku-insert で連続して入力
  ;; された 20 文字を 1 回のアンドゥの対象とする。`20' は keyboard.c に定めら
  ;; れたマジックナンバー。Mule-2.3 添付の egg.el を参考にした。
  (if (and (< skk-self-insert-non-undo-count 20)
           (memq last-command
                 '(
                   ;; SKK abbrev モードでは、アスキー文字入力が Emacs オリジナ
                   ;; ルの self-insert-command により行なわれているので、
                   ;; skk-self-insert-non-undo-count をインクリメントすること
                   ;; ができないので、アンドゥをエミュレートできない。
                   ;; しかも、カンマやピリオドを挿入した時点で、
                   ;; skk-abbrev-comma や skk-abbrev-period を使うことになるの
                   ;; で、オリジナルのアンドゥの機能も損なってしまう。現実問題
                   ;; としては、SKK abbrev モードは省略形としての見出し語を挿
                   ;; 入するためのモードであるので、長い見出し語を挿入すること
                   ;; はあまりなく、問題も小さいと考えられる。
                   ;;skk-abbrev-comma
                   ;;skk-abbrev-period
                   skk-insert-comma
                   skk-insert-period
                   skk-kana-input
                   skk-self-insert
                   ;;skk-set-henkan-point
                   skk-zenkaku-insert )))
      (progn
        (cancel-undo-boundary)
        (setq skk-self-insert-non-undo-count
              (1+ skk-self-insert-non-undo-count) ))
    (setq skk-self-insert-non-undo-count 1) ))

(defun skk-get-next-rule (prefix)
  (or (if (and (boundp 'skk-rom-kana-rule-tree)
	       skk-rom-kana-rule-tree )
	  (skk-assoc-tree prefix
			  skk-rom-kana-rule-tree )
	(cdr (assoc prefix skk-rom-kana-rule-list)) )
      (if (and (boundp 'skk-standard-rom-kana-rule-tree)
	       skk-standard-rom-kana-rule-tree)
	  (skk-assoc-tree prefix
			  skk-standard-rom-kana-rule-tree )
	(cdr (assoc prefix skk-standard-rom-kana-rule-list)) )))

(defun skk-get-fallback-rule (prefix)
  (cdr (assoc prefix skk-fallback-rule-alist)) )

(defun skk-check-postfix-rule (last)
  (let ((l skk-postfix-rule-alist)
	ret)
    (while l
      (if (eq (string-to-char last) (string-to-char (car (car l))))
	  (setq ret (cons (car l) ret)) )
      (setq l (cdr l)) )
    ret ))

(defun skk-get-postfix-rule (prefix &optional alist)
  (let ((alist (or alist skk-postfix-rule-alist)))
    (cdr (assoc prefix alist)) ))

(defun skk-kana-input ()
  "かな文字の入力を行うルーチン。"
  (interactive "*")
  (combine-after-change-calls
    (if (and skk-henkan-active
             skk-kakutei-early (not skk-process-okuri-early) )
        (skk-kakutei) )
    (let ((echo-keystrokes 0)
          ;; don't echo key strokes in the minibuffer.
	  last-input
	  last-kana )
      (if skk-isearch-message (skk-isearch-message))
      (setq skk-prefix "")
      (skk-set-marker skk-kana-start-point (point))
      (skk-unread-event (skk-character-to-event last-command-char))
      (condition-case nil
	  (let ((cont t)
		prev )
	    (while cont
	      (let* ((raw-event (skk-read-event))
		     ;; ascii equivallence of raw-event or nil.
		     (r-char (skk-event-to-character raw-event))
		     input
		     prefix
		     next
		     low )
		(if skk-debug (message "%S" r-char))
		(if r-char
		    (progn
		      (if (and
			   (or (and
				skk-henkan-on (not skk-henkan-active)
				(= skk-henkan-start-point
				   skk-kana-start-point ))
			       (and
				skk-okurigana
				(= (1+ skk-okurigana-start-point)
				   ;; "*"
				   skk-kana-start-point )))
			   (not (eq r-char (skk-downcase r-char))) )
			  ;; this case takes care of the rare case where
			  ;; one types two characters in upper case
			  ;; consequtively.  For example, one sometimes
			  ;; types "TE" when one should type "Te"
			  (setq r-char (skk-downcase r-char)
				raw-event (skk-character-to-event r-char) ))
		      (setq input (skk-char-to-string r-char)
			    last-input input
			    prefix (concat skk-prefix input)
			    next (skk-get-next-rule prefix) )))
		(if skk-debug (message "%S" next))
		(if skk-isearch-message (skk-isearch-message))
		(if next
		    (let ((newprefix (car next))
			  (output (nth 1 next)) )
		      (setq low (nth 2 next))
		      (skk-erase-prefix)
		      (if output
			  (progn
			    (setq last-kana 
				  (if skk-katakana (car output) (cdr output)))
			    ;; XXX for isearch
			    (skk-insert-str last-kana)
			    (if skk-okurigana (skk-set-okurigana))))
		      (if newprefix
			  (progn
			    (skk-set-marker skk-kana-start-point (point))
			    (skk-insert-prefix newprefix)
			    (setq skk-prefix newprefix))
			(setq cont nil
			      skk-prefix "" )))
		  (let ((type (skk-kana-input-char-type
			       (or r-char (skk-event-to-character raw-event)))))
		    (cond ((eq type 5)    ; delete prefix
			   (setq cont nil)
			   (if skk-okurigana
			       (progn
				 (skk-delete-okuri-mark)
				 (skk-set-marker skk-kana-start-point
						 skk-okurigana-start-point )))
			   (or (string= skk-prefix "")
			       (if skk-echo
				   (skk-erase-prefix)
				 (skk-message "プレフィックス \"%s\" を消しました"
					      "Deleted prefix \"%s\""
					      skk-prefix )))
			   (setq skk-prefix "") )
			  (t
			   (if (string= skk-prefix "")
			       (progn
				 (skk-message
				  "適切なローマ字かな変換ルールがありません。"
				   "No suitable rule." )
				 (skk-set-marker skk-kana-start-point nil)
				 (setq cont nil) )
			     (skk-erase-prefix)
			     (let ((output (skk-get-fallback-rule skk-prefix)))
			       (if output
				   (progn
				     (setq last-kana 
					   (if skk-katakana
					       (car output) (cdr output)))
				     ;; XXX for isearch
				     (skk-insert-str last-kana)
				     (if skk-okurigana (skk-set-okurigana)) ))
			       (skk-unread-event raw-event)
			       (skk-set-marker skk-kana-start-point nil)
			       (setq skk-prefix "")
			       (setq cont nil) ))

			   )))
		  )
		)))
        (quit
	 (setq skk-prefix "")
	 (skk-erase-prefix)
	 (skk-set-marker skk-kana-start-point nil)
	 (keyboard-quit) ))
      (let ((postfix-rules (skk-check-postfix-rule last-input)))
	(if postfix-rules
	    (progn
	      (let ((prefix last-kana))
		(if skk-isearch-message (skk-isearch-message)))
	      (let* ((raw-event (skk-read-event))
		     ;; ascii equivallence of raw-event or nil.
		     (r-char (skk-event-to-character raw-event)) )
		(if r-char
		    (let ((new-char
			   (skk-get-postfix-rule
			    (concat last-input (skk-char-to-string r-char))
			    postfix-rules ))
			  prefix )
		      (if new-char
			  (progn
			    (setq skk-prefix (skk-char-to-string r-char))
			    (skk-set-marker skk-kana-start-point (point))
			    (skk-insert-prefix skk-prefix)
			  ;; XXX for isearch
			    (setq prefix (concat last-kana skk-prefix))
			    (if skk-isearch-message (skk-isearch-message))
			    (condition-case nil
				(let* ((raw-event2 (skk-read-event))
				       (r-char2 (skk-event-to-character raw-event2))
				       (type (skk-kana-input-char-type r-char2)) 
				       (prefix (concat skk-prefix
						       (skk-char-to-string r-char2)))
				       (next (skk-get-next-rule prefix) ))
				  (cond  (next
					  ;; rule がある。
					  (setq skk-prefix "")
					  (skk-erase-prefix)
					  (skk-set-marker skk-kana-start-point nil)
					  (skk-unread-event raw-event2)
					  (skk-unread-event raw-event) )
					 ((eq type 5)
					  ;; delete
					  (setq skk-prefix "")
					  (skk-erase-prefix)
					  (skk-set-marker skk-kana-start-point nil) )
					 (t
					  (skk-erase-prefix)
					  (skk-insert-str
					   (if skk-katakana
					       (car new-char)
					     (cdr new-char)))
					  (skk-unread-event raw-event2) )))
			      (quit
			       (setq skk-prefix "")
			       (skk-erase-prefix)
			       (skk-set-marker skk-kana-start-point nil)
			       (skk-unread-event raw-event)
			       (keyboard-quit) )))
			(skk-unread-event raw-event) ))
		  (skk-unread-event raw-event) )))))
      )))

(defun skk-translate-okuri-char (okurigana)
  (if skk-okuri-char-alist
      (cdr (assoc (skk-substring-head-character okurigana) skk-okuri-char-alist)) ))

(defun skk-set-okurigana ()
  ;; 見出し語から skk-henkan-okurigana, skk-henkan-key の各値をセットする。
  (if skk-katakana
      (skk-hiragana-region skk-henkan-start-point (point)) )
  (skk-set-marker skk-henkan-end-point skk-okurigana-start-point)
  ;; just in case
  (skk-save-point
    (goto-char skk-okurigana-start-point)
    (if (not (eq (following-char) ?*)) ;?*
        (insert "*") ))
  (setq skk-henkan-okurigana (skk-buffer-substring
                              (1+ skk-okurigana-start-point)
                              (point) ))
  (setq skk-henkan-key (concat (skk-buffer-substring skk-henkan-start-point
                                                     skk-henkan-end-point )
			       (or (skk-translate-okuri-char
				    skk-henkan-okurigana)
				   skk-okuri-char ))
        skk-prefix "" )
  (if skk-debug
      (message "%S %S %S" skk-henkan-okurigana skk-henkan-key skk-okuri-char) )
  (delete-region skk-okurigana-start-point (1+ skk-okurigana-start-point))
  (setq skk-henkan-count 0)
  (skk-henkan)
  (setq skk-okurigana nil)
  (cancel-undo-boundary) )

;;;; other inputting functions

(defun skk-insert-period (count)
  "見出しの補完を行っている最中であれば、次の候補を表示する。
補完の直後でなければ、\".\" を挿入する。
SKK abbrev モードでは、skk-abbrev-period 関数を使用すること。"
  (interactive "*P")
  (if (and (eq last-command 'skk-completion) (not skk-henkan-active))
      (progn
        (setq this-command 'skk-completion)
        (skk-completion nil) )
    (skk-self-insert count)
    (setq skk-last-henkan-result nil)
    (skk-set-marker skk-last-henkan-point nil) ))

(defun skk-insert-comma (count)
  "見出しの補完を行っている最中であれば、直前の候補を表示する。
補完の直後でなければ、\",\" を挿入する。
SKK abbrev モードでは、skk-abbrev-comma 関数を使用すること。"
  (interactive "*P")
  (if (and (eq last-command 'skk-completion) (not skk-henkan-active))
      (skk-previous-completion)
    (skk-self-insert count) ))

(defun skk-abbrev-period (arg)
  "SKK abbrev モードで見出しの補完を行っている最中であれば、次の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-period 関数を使用すること。"
  (interactive "*P")
  (if (eq last-command 'skk-completion)
      (progn
        (setq this-command 'skk-completion)
        (skk-completion nil) )
    (skk-emulate-original-map arg) ))

(defun skk-abbrev-comma (arg)
  "SKK abbrev モードで見出しの補完を行っている最中であれば、直前の候補を表示する。
補完の直後でなければ、オリジナルのキー割り付けのコマンドをエミュレートする。
SKK abbrev モード以外では、skk-insert-comma 関数を使用すること。"
  (interactive "*P")
  (if (eq last-command 'skk-completion)
      (skk-previous-completion)
    (skk-emulate-original-map arg) ))

(defun skk-self-insert (arg)
  "ひらがな、カタカナ、もしくは ascii 文字をカレントバッファに挿入する。
ひらがなモードもしくはカタカナモードでは、skk-input-vector をテーブルとして、
最後に入力されたキーに対応する文字を挿入する。
ascii モードでは、キー入力をそのまま挿入する。
skk-auto-insert-paren の値が non-nil の場合で、skk-auto-paren-string-alist に
対応する文字列があるときは、その対応する文字列 (かっこ類) を自動的に挿入する。"
  (interactive "*P")
  (let ((str (aref skk-input-vector last-command-char)))
    ;; Overlay を消すために先に確定する。
    (if skk-henkan-active (skk-kakutei))
    (if (not str)
        (skk-emulate-original-map arg)
      (let* ((count (prefix-numeric-value arg))
             (count2 count)
             (pair-str
              (and skk-auto-insert-paren
                   (cdr (assoc str skk-auto-paren-string-alist)) ))
             (pair-str-inserted 0) )
        (while (> count 0)
          (skk-insert-str str)
          (setq count (1- count)) )
        (if (not pair-str)
            nil
          (while (> count2 0)
            (if (not (string= pair-str (char-to-string (following-char))))
                (progn
                  (setq pair-str-inserted (1+ pair-str-inserted))
                  (skk-insert-str pair-str) ))
            (setq count2 (1- count2)) )
          (if (not (eq pair-str-inserted 0))
              (backward-char pair-str-inserted) ))))))

(defun skk-zenkaku-insert (arg)
  "全英文字をカレントバッファに挿入する。
skk-zenkaku-vector をテーブルとして、最後に入力されたキーに対応する文字を挿入
する。
skk-auto-insert-paren の値が non-nil の場合で、skk-auto-paren-string-alist に
対応する文字列があるときは、その対応する文字列 (かっこ類) を自動的に挿入する。"
  (interactive "*p")
  (let* ((str (aref skk-zenkaku-vector last-command-char))
         (arg2 arg)
         (pair-str
          (and skk-auto-insert-paren
               (cdr (assoc str skk-auto-paren-string-alist)) ))
         (pair-str-inserted 0) )
    (while (> arg 0)
      (skk-insert-str str)
      (setq arg (1- arg)) )
    (if (not pair-str)
        nil
      (while (> arg2 0)
        (if (not (string= pair-str (char-to-string (following-char))))
            (progn
              (setq pair-str-inserted (1+ pair-str-inserted))
              (skk-insert-str pair-str) ))
        (setq arg2 (1- arg2)) )
      (if (not (eq pair-str-inserted 0))
          (backward-char pair-str-inserted) ))))

;;;; henkan routines
(defun skk-henkan ()
  ;; カナを漢字変換するメインルーチン。
  (let (mark new-word kakutei-henkan)
    (if (string= skk-henkan-key "")
        (skk-kakutei)
      (if (not (eobp))
          ;; we use mark to go back to the correct position after henkan
          (setq mark (skk-save-point (forward-char 1) (point-marker))) )
      (if (not skk-henkan-active)
          (progn
            (skk-change-marker)
            (setq skk-current-search-prog-list skk-search-prog-list) ))
      ;; skk-henkan-1 の中からコールされる skk-henkan-show-candidate から throw
      ;; される。ここでキャッチした場合は、?x がストリームに戻されているので、
      ;; この関数を出て、skk-previous-candidates へゆく。
      (catch 'unread
        (setq new-word (or (skk-henkan-1) (skk-henkan-in-minibuff))
              kakutei-henkan skk-kakutei-flag )
        (if new-word
            (skk-insert-new-word new-word) ))
      (if mark
          (progn
            (goto-char mark)
            ;; 参照されていないマーカーは、Garbage Collection がコールされたと
            ;; きに回収されるが、それまでの間、テキストのどこかを指していると、
            ;; テキストのアップデートの際にそのマーカー値を更新する必要がある
            ;; ので、どこも指さないようにする。
            (skk-set-marker mark nil)
	    (backward-char 1) )
        (goto-char (point-max)) )
      (if kakutei-henkan
	  ;; 確定しても良い (確定辞書に候補を見つけた場合、辞
	  ;; 書登録を行った場合、あるいはミニバッファから候補
	  ;; を選択した場合) のなら、Overlay による表示変更せ
	  ;; ずにそのまま確定。
	  (skk-kakutei (if (skk-numeric-p)
			   (skk-get-simply-current-candidate 'noconv)
			 new-word )))
      )))

(defun skk-henkan-1 ()
  ;; skk-henkan のサブルーチン。
  (let (new-word)
    (if (eq skk-henkan-count 0)
        (progn
          (if (and (eq last-command 'skk-undo-kakutei-henkan)
                   (eq (car (car skk-current-search-prog-list))
                       'skk-search-kakutei-jisyo-file ))
              ;; in this case, we should not search kakutei jisyo.
              (setq skk-current-search-prog-list
                    (cdr skk-current-search-prog-list) ))
          (setq skk-henkan-list (skk-search))
          (if (null skk-henkan-list)
              nil
            (setq new-word (skk-get-current-candidate))
            (if skk-kakutei-flag
                ;; found the unique candidate in kakutei jisyo
                (setq this-command 'skk-kakutei-henkan
                      skk-last-kakutei-henkan-key skk-henkan-key ))))
      ;; 変換回数が 1 以上のとき。
      (setq new-word (skk-get-current-candidate))
      (if (not new-word)
          ;; 新しい候補を見つけるか、skk-current-search-prog-list が空にな
          ;; るまで skk-search を連続してコールする。
          (while (and skk-current-search-prog-list (not new-word))
            (setq skk-henkan-list (skk-nunion skk-henkan-list (skk-search))
                  new-word (skk-get-current-candidate) )))
      (if (and new-word (> skk-henkan-count 3))
          ;; show candidates in minibuffer
          (setq new-word (skk-henkan-show-candidates) )))
    new-word ))

;;;###skk-autoload
(defun skk-get-current-candidate ()
  (if (skk-numeric-p)
      (let (val)
        (skk-uniq-numerals)
        (setq val (skk-numeric-convert (skk-get-simply-current-candidate)))
        (if (not skk-recompute-numerals-key)
            val
          (skk-uniq-numerals)
          (skk-numeric-convert (skk-get-simply-current-candidate)) ))
    (skk-get-simply-current-candidate) ))

(defun skk-henkan-show-candidates ()
  ;; ミニバッファで変換した候補群を表示する。
  (skk-save-point
   (let* ((candidate-keys               ; 表示用のキーリスト
           (mapcar (function (lambda (c) (char-to-string (upcase c))))
                   skk-henkan-show-candidates-keys ))
          key-num-alist                 ; 候補選択用の連想リスト
          (key-num-alist1               ; key-num-alist を組み立てるための作業用連想リスト。
           (let ((count 6))
             (mapcar (function (lambda (key) (prog1 (cons key count)
                                               (setq count (1- count)) )))
                     ;; 逆さまにしておいて、表示する候補の数が少なかったら先
                     ;; 頭から幾つか削る。
                     (reverse skk-henkan-show-candidates-keys) )))
          (loop 0)
          inhibit-quit
          henkan-list new-one str reverse n )
     ;; 念のため。skk-previous-candidate を参照。
     (if skk-use-face (skk-henkan-face-off))
     (delete-region skk-henkan-start-point skk-henkan-end-point)
     (while loop
       (if str
           (let (message-log-max)
             (message str) )
         (cond (reverse
                (setq loop (1- loop)
                      henkan-list (nthcdr (+ 4 (* loop 7)) skk-henkan-list)
                      reverse nil ))
               (skk-exit-show-candidates
                ;; 候補が尽きてしまって、skk-henkan-show-candidates ->
                ;; skk-henkan-in-minibuff -> skk-henkan
                ;; -> skk-henkan-show-candidates の順で、再びこの関数が呼ばれ
                ;; たときは、ここで henkan-list と loop を計算する。
                (setq henkan-list (nthcdr skk-henkan-count skk-henkan-list)
                      loop (car skk-exit-show-candidates)
                      skk-exit-show-candidates nil ))
               (t
                ;; skk-henkan-show-candidates-keys の最終のキーに対応する候補
                ;; が出てくるまでサーチを続ける。
                (if (skk-numeric-p) (skk-uniq-numerals))
                (while (and skk-current-search-prog-list
                            (null (nthcdr (+ 11 (* loop 7)) skk-henkan-list)) )
                  (setq skk-henkan-list
                        (skk-nunion skk-henkan-list (skk-search)) )
                  (if (skk-numeric-p)
                      (skk-uniq-numerals) ))
                (if (skk-numeric-p)
                    (skk-numeric-convert*7) )
                (setq henkan-list (nthcdr (+ 4 (* loop 7))
                                          skk-henkan-list ))))
         (setq n (skk-henkan-show-candidate-subr candidate-keys henkan-list)) )
       (if (> n 0)
           (condition-case nil
               (let* ((event (skk-read-event))
                      (char (skk-event-to-character event))
                      num )
                 (if (null char)
                     (skk-unread-event event)
                   (setq key-num-alist (nthcdr (- 7 n) key-num-alist1))
                   (if (null key-num-alist)
                       nil
                     (setq num (cdr (or (assq char key-num-alist)
                                        (if (skk-lower-case-p char)
                                            (assq (upcase char) key-num-alist)
                                          (assq (downcase char) key-num-alist) )))))
                   (cond (num
                          (setq new-one (nth num henkan-list)
                                skk-henkan-count (+ 4 (* loop 7) num)
                                skk-kakutei-flag t
                                loop nil
                                str nil ))
                         ((eq char (skk-int-char 32)) ; space
                          (if (or skk-current-search-prog-list
                                  (nthcdr 7 henkan-list) )
                              (setq loop (1+ loop)
                                    str nil )
                            ;; 候補が尽きた。この関数から抜ける。
                            (let ((last-showed-index (+ 4 (* loop 7))))
                              (setq skk-exit-show-candidates
                                    ;; cdr 部は、辞書登録に入る前に最後に表示し
                                    ;; た候補群の中で最初の候補を指すインデクス
                                    (cons loop last-showed-index) )
                              ;; 辞書登録に入る。skk-henkan-count は
                              ;; skk-henkan-list の最後の候補の次 (存在しない
                              ;; --- nil )を指す。
                              (setq skk-henkan-count (+ last-showed-index n)
                                    loop nil
                                    str nil ))))
                         ((eq char skk-previous-candidate-char)  ; ?x
                          (if (eq loop 0)
                              ;; skk-henkan-show-candidates を呼ぶ前の状態に戻
                              ;; す。
                              (progn
                                (setq skk-henkan-count 4)
                                (skk-unread-event (skk-character-to-event
						   skk-previous-candidate-char))
                                ;; skk-henkan まで一気に throw する。
                                (throw 'unread nil) )
                            ;; 一つ前の候補群をエコーエリアに表示する。
                            (setq reverse t
                                  str nil )))
                         (t (skk-message "\"%c\" は有効なキーではありません！"
                                         "\"%c\" is not valid here!"
                                         char )
                            (sit-for 1) ))))
             (quit
              ;; skk-previous-candidate へ
              (setq skk-henkan-count 0)
              (skk-unread-event (skk-character-to-event
				 skk-previous-candidate-char))
              ;; skk-henkan まで一気に throw する。
              (throw 'unread nil) ))))  ; end of while loop
     (if (consp new-one)
         (cdr new-one)
       new-one ))))

(defun skk-henkan-show-candidate-subr (keys candidates)
  ;; key と candidates を組み合わせて 7 つ以下の候補群 (候補数が足りなかったら
  ;; そこで打ち切る) の文字列を作り、ミニバッファに表示する。
  (let ((n 0) str cand
        message-log-max )
    (if (not (car candidates))
        nil
      (setq n 1
            ;; 最初の候補の前に空白をくっつけないように最初の候補だけ先に取り
            ;; 出す。
            str (concat (car keys) ":" (skk-%-to-%%
                                        (if (consp (car candidates))
                                            (cdr (car candidates))
                                          (car candidates) ))))
      ;; 残りの 6 つを取り出す。候補と候補の間を空白でつなぐ。
      (while (and (< n 7) (setq cand (nth n candidates)))
        (setq cand (skk-%-to-%% (if (consp cand) (cdr cand) cand))
              str (concat str "  " (nth n keys) ":" cand)
              n (1+ n) ))
      (message "%s  [残り %d%s]"
               str (length (nthcdr n candidates))
               (make-string (length skk-current-search-prog-list) ?+) ))
    ;; 表示する候補数を返す。
    n ))

(defun skk-%-to-%% (str)
  ;; STR 中に % を含む文字があったら、%% にして message でエラーにならないよう
  ;; にする。
  (let ((tail str)
        temp beg end )
    (save-match-data
      (while (string-match "%+" tail)
        (setq beg (match-beginning 0)
              end (match-end 0)
              temp (concat temp (substring tail 0 beg)
                           (make-string (* 2 (- end beg)) ?%) )
              tail (substring tail end) ))
      (concat temp tail) )))

(defun skk-henkan-in-minibuff ()
  ;; ミニバッファで辞書登録をし、登録したエントリの文字列を返す。
  (save-match-data
    (let ((enable-recursive-minibuffers t)
          ;; 変換中に isearch message が出ないようにする。
          skk-isearch-message new-one )
      (add-hook 'minibuffer-setup-hook 'skk-setup-minibuffer)
      (condition-case nil
          (setq new-one
                (read-from-minibuffer
                 (concat (or (if (skk-numeric-p)
                                 (skk-numeric-midasi-word) )
                             (if skk-okuri-char
                                 (skk-compute-henkan-key2)
                               skk-henkan-key ))
                         " " )))
        (quit
         (setq new-one "") ))
      (if (string= new-one "")
          (if skk-exit-show-candidates
              ;; ミニバッファに表示した候補が尽きて辞書登録に入ったが、空文字
              ;; 列が登録された場合。最後にミニバッファに表示した候補群を再表
              ;; 示する。
              (progn
                (setq skk-henkan-count (cdr skk-exit-show-candidates))
                (skk-henkan) )
            ;; skk-henkan-show-candidates に入る前に候補が尽きた場合
            (setq skk-henkan-count (1- skk-henkan-count))
            (if (eq skk-henkan-count -1)
                (progn
                  ;; 送りありの変換で辞書登録に入り、空文字を登録した後、その
                  ;; まま再度送りなしとして変換した場合は 
                  ;; skk-henkan-okurigana, skk-okuri-char の値を nil にしなけ
                  ;; れば、それぞれの値に古い送り仮名が入ったままで検索に失敗
                  ;; する。
                  (setq skk-henkan-okurigana nil
                        skk-okurigana nil
                        skk-okuri-char nil )
                  (skk-change-marker-to-white) )
              ;; skk-henkan-count が -1 でなければ、カレントバッファでは最後の
              ;; 候補を表示したままなので (表示関連では何もしなくても、もう既
              ;; に望みの状態になっている) 何もしない。
              ))
        ;; ミニバッファで変換した文字列がある (空文字列でない) とき。
        ;; 末尾の空白を取り除く。
        (if (string-match "[ 　]+$" new-one)
            (setq new-one (substring new-one 0 (match-beginning 0))) )
        (if (skk-numeric-p)
            (setq new-one (skk-adjust-numeric-henkan-data new-one))
          ;; すごくたくさんの候補がある場合に、その最後に新しい候補を加えるのは
          ;; けっこう骨だが。
          (setq skk-henkan-list (nconc skk-henkan-list (list new-one))
                ;; フラグをオンにする。
                skk-kakutei-flag t ))
        (setq skk-henkan-in-minibuff-flag t
              skk-touroku-count (1+ skk-touroku-count) ))
      ;; (nth skk-henkan-count skk-henkan-list) が nil だから辞書登録に
      ;; 入っている。skk-henkan-count をインクリメントする必要はない。
      ;; (setq skk-henkan-count (1+ skk-henkan-count))
      ;; new-one が空文字列だったら nil を返す。
      (if (not (string= new-one "")) new-one) )))

(defun skk-compute-henkan-key2 ()
  ;; skk-henkan-okurigana が non-nil なら skk-henkan-key から、かつて 
  ;; skk-henkan-key2 と呼ばれていたものを作る。
  ;; skk-henkan-key2 とは、「漢字部分の読み + "*" + 送り仮名」の形式の文字列を
  ;; 言う。
  (if skk-henkan-okurigana
      (save-match-data
        (if (string-match "[a-z]+$" skk-henkan-key)
            (concat (substring skk-henkan-key 0 (match-beginning 0))
                    "*" skk-henkan-okurigana )))))
              
(defun skk-setup-minibuffer ()
  ;; カレントバッファの入力モードに従いミニバッファの入力モードを設定する。
  (let ((mode (skk-spy-origin-buffer-mode)))
    (if (not mode)
        nil
      (cond ((eq mode 'hirakana) (skk-j-mode-on))
            ((eq mode 'katakana) (skk-j-mode-on t))
            ((eq mode 'abbrev) (skk-abbrev-mode-on))
            ((eq mode 'ascii) (skk-ascii-mode-on))
            ((eq mode 'zenkaku) (skk-zenkaku-mode-on))))))

(defun skk-spy-origin-buffer-mode ()
  ;; ミニバッファに居るときにオリジナルのカレントバッファの入力モードを偵察する。
  (with-current-buffer (skk-minibuffer-origin)
    (if skk-mode
        (cond (skk-abbrev-mode 'abbrev)
              (skk-ascii-mode 'ascii)
              (skk-zenkaku-mode 'zenkaku)
              (skk-katakana 'katakana)
              (t 'hirakana) ))))

;;;###skk-autoload
(defun skk-previous-candidate ()
  "▼モードであれば、一つ前の候補を表示する。
▼モード以外ではカレントバッファに \"x\" を挿入する。
確定辞書による確定の直後に呼ぶと確定がアンドゥされて、確定前の状態で
skk-last-kakutei-henkan-key がカレントバッファに挿入される。"
  (interactive)
  (if (not skk-henkan-active)
      (if (not (eq last-command 'skk-kakutei-henkan))
          (skk-kana-input)
        ;; restore the state just before the last kakutei henkan.
        (delete-region skk-henkan-start-point (point))
        (skk-set-henkan-point-subr)
        (insert skk-last-kakutei-henkan-key)
        (setq this-command 'skk-undo-kakutei-henkan) )
    (if (string= skk-henkan-key "")
        nil
      ;; 空白を挿入しておいて、後で delete-backward-char を使って消す方法では、
      ;; overwrite-mode のときにその空白を消せない。従い skk-henkan で行なって
      ;; いる方法と同じものを使う。
      ;;(insert " ")
      (let ((mark
             (if (not (eobp))
                 (skk-save-point (forward-char 1) (point-marker)) )))
        (skk-save-point
          (if (eq skk-henkan-count 0)
              (progn
                (if skk-okuri-char
                    ;; roman prefix for okurigana should be removed.
                    (setq skk-henkan-key (substring skk-henkan-key 0 -1)) )
                (setq skk-henkan-count -1
                      skk-henkan-list nil
                      skk-henkan-okurigana nil
                      skk-okuri-char nil
                      skk-okurigana nil
                      skk-prefix "" )
                (if skk-auto-okuri-process
                    (skk-init-auto-okuri-variables) )
                (if (skk-numeric-p)
                    (skk-init-numeric-conversion-variables) )
                ;; Emacs 19.28 だと何故か Overlay を消しておかないと、次に
                ;; insert される skk-henkan-key Overlay がかかってしまう。
                (if skk-use-face (skk-henkan-face-off))
                (delete-region skk-henkan-start-point skk-henkan-end-point)
                (goto-char skk-henkan-end-point)
                (insert skk-henkan-key)
                (skk-change-marker-to-white) )
            (setq skk-henkan-count (1- skk-henkan-count))
            (skk-insert-new-word (skk-get-simply-current-candidate)) ))
        ;;(if (and (> (point) 1) (eq (char-after (1- (point))) 32))
        ;; delete-backward-char では、overwrite-mode のときに直前の空白を消せ
        ;; ない。
        ;;    (delete-backward-char 1) )
        (if mark
            (progn
              (goto-char mark)
              (skk-set-marker mark nil)
              (backward-char 1) )
          (goto-char (point-max)) )
        (if (and skk-abbrev-mode (eq skk-henkan-count -1))
            (skk-abbrev-mode-on) )))))

(defun skk-insert-new-word (word)
  ;; 見出し語を消し、その場所へ変換結果の文字列を挿入する。
  (let (func)
    ;; 念のため。何故これを入れるのかについては、skk-previous-candidate を参照
    (if skk-use-face (skk-henkan-face-off))
    (delete-region skk-henkan-start-point skk-henkan-end-point)
    (goto-char skk-henkan-start-point)
    ;; (^_^;) のような見出し語に対し、read-from-string を呼ぶとエラーになるの
    ;; で、condition-case でそのエラーを捕まえる。
    (condition-case nil
        (setq func (car (read-from-string word)))
      (error (setq func word)))
    ;; symbolp で nil を返すような単語を、symbolp でチェックすること無くいき
    ;; なり fboundp でチェックすると、エラーになる。
    ;; e.x. "(#0)"
    (condition-case nil
        (if (and (listp func) (symbolp (car func)) (fboundp (car func)))
            (insert (eval func))
          (insert word) )
      ;; 文字列を返さない Lisp プログラムを評価してもエラーにならない方が便利？
      (error nil) )
    (skk-set-marker skk-henkan-end-point (point))
    (if skk-use-face (skk-henkan-face-on))
    (if skk-insert-new-word-function
        (funcall skk-insert-new-word-function) )))

;;;###skk-autoload
(defun skk-kakutei (&optional word)
  "現在表示されている語で確定し、辞書の更新を行う。
オプショナル引数の WORD を渡すと、現在表示されている候補とは無関係に WORD で確
定する。"
  ;; read only でエラーになるようにすると read only バッファで SKK が起動でき
  ;; なくなる。
  (interactive)
  (let ((inhibit-quit t)
        converted kakutei-word )
    (if skk-mode
        (skk-j-mode-on skk-katakana)
      ;; カレントバッファでまだ skk-mode がコールされていなかったら、コールす
      ;; る。
      (skk-mode 1) )
    (if (not skk-henkan-on)
        nil
      (if (not skk-henkan-active)
          nil
        (setq kakutei-word
              ;; 確定辞書の語で確定したときは、辞書にその語を書き込む必要もな
              ;; いし、更新する必要もないと思っていたが、補完を行なうときは、
              ;; 個人辞書を参照する (確定辞書は参照しない) ので、多少資源と時
              ;; 間を無駄にしても、個人辞書に確定辞書のエントリを書き込んで更
              ;; 新もしておく。
              (or word (skk-get-simply-current-candidate (skk-numeric-p))) )
        (if (or
             (and (not skk-search-excluding-word-pattern-function) kakutei-word)
             (and
              kakutei-word
              skk-search-excluding-word-pattern-function
              (not
               (funcall skk-search-excluding-word-pattern-function kakutei-word) )))
            (progn
            (skk-update-jisyo kakutei-word)
	    ;; keep this order
	    (setq skk-last-henkan-result kakutei-word)
	    (skk-set-marker skk-last-henkan-point (point))
            (if (skk-numeric-p)
                (progn
                  (setq converted (skk-get-simply-current-candidate))
                  (skk-update-jisyo-for-numerals kakutei-word converted) )))))
      (skk-kakutei-cleanup-henkan-buffer) )
    (skk-kakutei-save-and-init-variables
     (if (skk-numeric-p)
         (cons kakutei-word converted)
       kakutei-word ))
    (skk-do-auto-fill)
    (skk-set-cursor-color (if skk-katakana
                              skk-katakana-cursor-color
                            skk-hirakana-cursor-color ))))

(defun skk-kakutei-cleanup-henkan-buffer ()
  ;; 確定直後のバッファの整形を行なう。
  ;; この関数は、skk-vip.el で上書きされる。
  (if skk-okurigana
      (progn
        (skk-delete-okuri-mark)
        (if (and skk-katakana skk-convert-okurigana-into-katakana)
            (skk-katakana-region skk-henkan-end-point (point)) )))
  (skk-delete-henkan-markers)
  (if (and (boundp 'self-insert-after-hook) self-insert-after-hook)
      (funcall self-insert-after-hook skk-henkan-start-point (point)) )
  (if overwrite-mode
      (skk-del-char-with-pad
       (skk-ovwrt-len
        (string-width
         (skk-buffer-substring skk-henkan-start-point (point)) )))))

(defun skk-kakutei-save-and-init-variables (&optional kakutei-word)
  ;; 確定時に変数の初期化とアンドゥのための変数の保存を行なう。
  (if (and kakutei-word (or (consp kakutei-word)
                            (not (string= kakutei-word "")) ))
      (progn
        ;; skk-undo-kakutei のために最後の変換のデーターを保存する。
        (setq skk-last-henkan-key skk-henkan-key
              ;; 確定した語を先頭にして skk-henkan-list の値を保存する。
              skk-last-henkan-list (cons kakutei-word
                                         (delete kakutei-word skk-henkan-list) )
              skk-last-henkan-okurigana skk-henkan-okurigana
              skk-last-okuri-char skk-okuri-char
              skk-kakutei-count (1+ skk-kakutei-count) )
        ;;(if (boundp 'disable-undo)
        ;;    (setq disable-undo nil)
        ))
  (setq skk-abbrev-mode nil
        skk-exit-show-candidates nil
        skk-henkan-active nil
        skk-henkan-count -1
        skk-henkan-key nil
        skk-henkan-list nil
        skk-henkan-okurigana nil
        skk-henkan-on nil
        skk-kakutei-flag nil
        skk-okuri-char nil
        skk-prefix "" )
  (if skk-auto-okuri-process
      (skk-init-auto-okuri-variables) )
  (if (skk-numeric-p)
      (skk-init-numeric-conversion-variables) ))

(defun skk-undo-kakutei ()
  "一番最後の確定をアンドゥし、見出しに対する候補を表示する。
最後に確定したときの候補はスキップされる。
候補が他にないときは、ミニバッファでの辞書登録に入る。"
  (interactive)
  (cond ((eq last-command 'skk-undo-kakutei)
         (skk-error "確定アンドゥは連続使用できません"
                    "Cannot undo kakutei repeatedly" ))
        (skk-henkan-active
         (skk-error "▼モードでは確定アンドゥできません"
                    "Cannot undo kakutei in ▼ mode" ))
        ((or (not skk-last-henkan-key) (string= skk-last-henkan-key ""))
         ;; skk-last-henkan-key may be nil or "".
         (skk-error "アンドゥデーターがありません" "Lost undo data") ))
  (condition-case nil
      (let ((end (if skk-last-henkan-okurigana
                     (+ (length skk-last-henkan-okurigana)
                        skk-henkan-end-point )
                   skk-henkan-end-point )))
        (setq skk-henkan-active t
              skk-henkan-key skk-last-henkan-key
              skk-henkan-list skk-last-henkan-list
              skk-henkan-on t
              skk-henkan-okurigana skk-last-henkan-okurigana
              skk-okuri-char skk-last-okuri-char
              skk-current-search-prog-list
              (if (eq (car (car skk-search-prog-list))
                      'skk-search-kakutei-jisyo-file )
                  ;; 確定辞書は探しても無意味。
                  (cdr skk-search-prog-list)
                skk-search-prog-list ))
        (if (>= (point-max) end)
            ;; 最後の変換部分のテキストを消す。送り仮名を把握しているのなら 
            ;; (skk-process-okuri-early が non-nil なら送り仮名を把握できない)、
            ;; 送り仮名を含めた部分までを消す。
            (delete-region skk-henkan-start-point end) )
        (goto-char skk-henkan-start-point)
        (cancel-undo-boundary)
        (insert "▼")
        (undo-boundary)
        (skk-set-marker skk-henkan-start-point (point))
        (if skk-okuri-char
            ;; 送りあり
            (progn
              (insert (substring skk-henkan-key 0
                                 (1- (length skk-henkan-key)) ))
              (skk-set-marker skk-henkan-end-point (point))
              (if skk-henkan-okurigana (insert skk-henkan-okurigana)) )
          (insert skk-henkan-key)
          (skk-set-marker skk-henkan-end-point (point)) )
        ;; さぁ、準備が整いました！
        (skk-message "確定アンドゥ！" "Undo kakutei!")
        (setq skk-henkan-count 1)
        (skk-henkan) )
    ;; skk-kakutei-undo から途中で抜けた場合は、各種フラグを初期化しておかない
    ;; と次の動作をしようとしたときにエラーになる。
    (error (skk-kakutei))
    (quit (skk-kakutei)) ))
     
(defun skk-downcase (char)
  (let ((d (cdr (assq char skk-downcase-alist))))
    (if d
	d
      (downcase char) )))

(defun skk-set-henkan-point (&optional arg)
  "変換を開始するポイントをマークし、対応する skk-prefix か、母音を入力する。"
  (interactive "*P")
  (combine-after-change-calls
    (let* ((last-char (skk-downcase last-command-char))
	   (normal (not (eq last-char last-command-char)))
           (sokuon (and (string= skk-prefix (char-to-string last-char))
                        (/= last-char ?o)))
           (henkan-active skk-henkan-active))
      (cancel-undo-boundary)
      (if (or (not skk-henkan-on) skk-henkan-active)
          (if normal
              (skk-set-henkan-point-subr)
            (if skk-henkan-on
                (skk-set-henkan-point-subr) )
            (if henkan-active
                (skk-emulate-original-map arg)
              (skk-self-insert arg) ))
        (if (not normal)
            ;; process special char
            (progn
              (insert last-char)
              (skk-set-marker skk-henkan-end-point (point))
              (setq skk-henkan-count 0
                    skk-henkan-key (skk-buffer-substring
                                    skk-henkan-start-point (point) )
                    skk-prefix "" )
              (skk-henkan) )
          ;; prepare for the processing of okurigana if not skk-okurigana
          ;; and the preceding character is not a numeric character.
          ;; if the previous char is a special midashi char or a
          ;; numeric character, we assume that the user intended to type the
          ;; last-command-char in lower case.
          (if (and (not skk-okurigana)
                   (or (= skk-henkan-start-point (point))
                       (let ((p (char-after (1- (point)))))
                         (not
                          (or
                           ;; previous char is a special midashi char
                           (memq p skk-special-midashi-char-list)
                           ;; previous char is an ascii numeric char
                           (and (<= 48 p) ; ?0
                                (<= p 57) ) ; ?9
                           ;; previous char is a jis numeric char
                           (and (eq (char-after (- (point) 2)) 163)
                                (<= 176 p) (<= p 185) ))))))
              (if skk-process-okuri-early
                  (progn
                    (skk-set-marker skk-henkan-end-point (point))
                    (setq skk-okuri-char (char-to-string last-char))
                    (if sokuon
                        (progn
                          (setq skk-henkan-key
                                (concat (skk-buffer-substring
                                         skk-henkan-start-point
                                         skk-kana-start-point )
                                        (if skk-katakana "ッ" "っ")
                                        skk-henkan-okurigana ))
                          (skk-erase-prefix)
                          (insert (if skk-katakana "ッ " "っ "))
                          (setq skk-prefix ""
                                skk-henkan-count 0 )
                          (skk-henkan)
                          ;;(if skk-use-face (skk-henkan-face-off))
                          (delete-backward-char 2)
                          ;;(if skk-use-face (skk-henkan-face-on))
                          )
                      (setq skk-henkan-key (concat
                                            (skk-buffer-substring
                                             skk-henkan-start-point
                                             (point) )
					    skk-okuri-char ))
                      (insert " ")
                      (setq skk-prefix ""
                            skk-henkan-count 0 )
                      (skk-henkan)
                      ;;(if skk-use-face (skk-henkan-face-off))
                      (delete-backward-char 1)
                      ;;(if skk-use-face (skk-henkan-face-on))
                      )
                    ;; we set skk-kana-start-point here, since the marker may no
                    ;; longer point at the correct position after skk-henkan.
                    (skk-set-marker skk-kana-start-point (point)) )
                (if (= skk-henkan-start-point (point))
                    nil
                  (if sokuon
                      (progn
                        (skk-erase-prefix)
                        (insert (if skk-katakana "ッ" "っ"))
                        (setq skk-prefix "") ))
                  (skk-set-marker skk-okurigana-start-point (point))
                  (insert "*")
                  (skk-set-marker skk-kana-start-point (point))
                  (setq skk-okuri-char (char-to-string last-char)
                        skk-okurigana t ))))))
      (if normal
          (skk-unread-event
           (skk-character-to-event last-char)) ))))

;;;###skk-autoload
(defun skk-start-henkan (arg)
  "▽モードでは変換を開始する。▼モードでは次の候補を表示する。
  その他のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。"
  (interactive "*p")
  (combine-after-change-calls
    (save-match-data
      (if (not skk-henkan-on)
          (skk-self-insert arg)
        (if skk-henkan-active
            (progn (setq skk-henkan-count (1+ skk-henkan-count))
                   (skk-henkan) )
          (let ((pos (point)))
            (or (string= skk-prefix "")
                (skk-error "フィックスされていない skk-prefix があります"
                           "Have unfixed skk-prefix" ))
            (if (< pos skk-henkan-start-point)
                (skk-error
                 "カーソルが変換開始地点より前にあります"
                 "Henkan end point must be after henkan start point" ))
            ;; 見出し語がカタカナであればひらがなに変換する。もし見出し語の変換
            ;; せずにそのまま skk-henkan に渡したければ、C-u SPC (arg が 4 にな
            ;; る) とタイプすればよい。
            (if (and skk-katakana (eq arg 1))
                (skk-hiragana-region skk-henkan-start-point pos) )
            (setq skk-henkan-key (skk-buffer-substring
                                  skk-henkan-start-point pos ))
            (if skk-allow-spaces-newlines-and-tabs
                ;; skk-henkan-key の中の "[ \n\t]+" を完全に取り除く。
                (while (string-match "[ \n\t]+" skk-henkan-key)
                  (setq skk-henkan-key
                        (concat (substring skk-henkan-key 0 (match-beginning 0))
                                (substring skk-henkan-key (match-end 0)) )))
              (skk-save-point
               (beginning-of-line)
               (if (> (point) skk-henkan-start-point)
                   (skk-error
                    "変換キーに改行が含まれています"
                    "Henkan key may not contain a new line character" )))
              ;; 最初のスペースで skk-henkan-key をちょん切るだけ。
              (setq skk-henkan-key (substring skk-henkan-key 0
                                              (string-match " "
                                                            skk-henkan-key ))))
            (skk-set-marker skk-henkan-end-point pos)
            (setq skk-henkan-count 0)
            (skk-henkan)
            (if (and skk-abbrev-mode skk-henkan-active)
		(progn
		  (skk-j-mode-on)
		  (setq skk-abbrev-mode t) )))) ;; XXX
        (cancel-undo-boundary) ))))

(defun skk-backward-and-set-henkan-point (arg)
  "ポイントの直前にある文字列の先頭に変換開始ポイントを示す \"▽\" を付ける。
カーソルの直前にある文字 \(スペース文字、タブ文字、長音を表わす「ー」 は無条件
にスキップされる\) を skk-what-char-type にて判別し、同種の文字列をひとかたま
りとして後方へスキップする。
但し、ひらかなの場合は「を」の直前で、カタカナの場合は「ヲ」の直前で止まる。
C-u ARG で ARG を与えると、その文字分だけ戻って同じ動作を行なう。"
  (interactive "*P")
  (if (not skk-mode)
      (skk-emulate-original-map arg)
    (catch 'exit1
      (skk-save-point
        ;; とりあえず最初の SPC, TAB, 全角 SPC だけジャンプする。
        (skip-chars-backward " \t　")
        ;; 引数あり。
        (if arg
            (if (not skk-allow-spaces-newlines-and-tabs)
                (skk-backward-char (prefix-numeric-value arg))
              (setq arg (prefix-numeric-value arg))
              (while (> arg 0)
                (skip-chars-backward " \t　")
                (if (bolp)
                    ;; 行頭だったら一行前の行末まで戻るが、arg は減らさない。
                    (skk-backward-char 1)
                  (skk-backward-char 1)
                  (setq arg (1- arg)) )))
          ;; 引数なし。
          (let ((limit
                 (if (not skk-allow-spaces-newlines-and-tabs)
                     (skk-save-point (beginning-of-line) (point))
                   (point-min) ))
                ;; ＿￣＾¨｀´゜゛！？；：・．，。
                (unknown-chars-regexp
                 (if skk-allow-spaces-newlines-and-tabs
                     "[ 　\n\tー〃ゞゝヾヽ]"
                   "[　ー〃ゞゝヾヽ]" ))
                char p )
            (save-match-data
              (skk-save-point
                (skk-backward-char 1)
                (while (and (> (point) limit)
                            ;; unknown-chars-regexp では文字種別が判別できないの
                            ;; で、その文字列が続く限りポイントをバッファの先頭
                            ;; 方向へ戻す。
                            (looking-at unknown-chars-regexp) )
                  (skk-backward-char 1) )
                (setq char (skk-what-char-type))
                (if (eq char 'unknown)
                    (throw 'exit1 nil)
                  (skk-backward-and-set-henkan-point-1 char)
                  (setq p (point))
                  (if skk-allow-spaces-newlines-and-tabs
                      (while (and (> (point) limit) (bolp))
                        ;; 1 行上の行末へ。
                        (skk-backward-char 1)
                        ;; ポイントが判別できない文字種別の上にある間は 
                        ;; backward 方向へポイントを戻す。
                        ;;(while (and (> (point) limit)
                        ;;            (looking-at unknown-chars-regexp) )
                        ;;  (skk-backward-char 1) )
                        (if ;;(or
                            (> 0 (skk-backward-and-set-henkan-point-1 char))
                            ;;(eq (skk-what-char-type) char))
                            (setq p (point)) ))))))
            (goto-char p)
            (skip-chars-forward unknown-chars-regexp) ))
        (skk-set-henkan-point-subr) ))))

(defun skk-backward-and-set-henkan-point-1 (char)
  ;; skk-backward-and-set-henkan-point のサブルーチン。CHAR の種類に応じた文字
  ;; をスキップしてバッファの先頭方向へ戻る。
  (cond ((eq char 'hirakana)
         ;; "を" の前で止まった方が便利？
         (skip-chars-backward "ヽヾゝゞ〃ーんぁ-ゑ") )
        ((eq char 'katakana)
         ;; "ヲ" の前で止まった方が便利？
         (skip-chars-backward "ヽヾゝゞ〃ーンァ-ヱ") )
        ((eq char 'zenkaku)
         (skip-chars-backward "　-ｚ") )
        ((eq char 'ascii)
         (skip-chars-backward " -~") )))

(defun skk-what-char-type ()
  ;; 現在のポイントにある文字がどんな種類かを判別する。
  (save-match-data
    (cond ((looking-at "[ぁ-ん]") 'hirakana)
          ((looking-at "[ァ-ン]") 'katakana)
          ;; "ー" を除外している ("ー" は "〇" と "―" の間に入っている)。
          ((looking-at "[　-〇―-ｚ]") 'zenkaku)
          ((looking-at "[ -~]") 'ascii)
          (t 'unknown) )))

(defun skk-set-henkan-point-subr ()
  "かなを入力した後で、ポイントに変換開始のマーク \(▽\) を付ける。
元々はこの関数は skk-set-henkan-point の内部関数である。"
  (interactive "*")
  (if skk-henkan-on (skk-kakutei))
  ;;(if (boundp 'disable-undo)
  ;;    (setq disable-undo t)
  (cancel-undo-boundary)
  ;;  )
  (if (string= skk-prefix "")
      (insert "▽")
    (skk-erase-prefix)
    (insert "▽")
    (skk-set-marker skk-kana-start-point (point))
    (skk-insert-prefix) )
  ;;(or (boundp 'disable-undo)
  (undo-boundary)
  ;;    )
  (setq skk-henkan-on t)
  (skk-set-marker skk-henkan-start-point (point)) )

(defun skk-change-marker ()
  ;; "▽"を"▼"に変える。skk-henkan-active フラグを t にする。
  (combine-after-change-calls
    (skk-save-point
     (goto-char (- skk-henkan-start-point skk-kanji-len))
     (if (looking-at "▽")
         (progn
           (cancel-undo-boundary)
           (let ((buffer-undo-list t))
             (insert "▼")
             (delete-char 1)
             (setq skk-henkan-active t) )
           (undo-boundary)
           )
       (skk-kakutei)
       (skk-error "▽がありません" "It seems that you have deleted ▽") ))))

(defun skk-change-marker-to-white ()
  ;; "▼"を"▽"に変える。skk-henkan-active フラグを nil にする。
  (combine-after-change-calls
    (skk-save-point
     (goto-char (- skk-henkan-start-point skk-kanji-len))
     (cancel-undo-boundary)
     (if (looking-at "▼")
         (let ((buffer-undo-list t))
           (insert "▽")
           (delete-char 1) )
       (goto-char skk-henkan-start-point)
       (insert "▽")
       ;;(or (boundp 'disable-undo)
       ;;(undo-boundary)
       ;;    )
       (skk-set-marker skk-henkan-start-point (point))
       (skk-message "▼がありません" "It seems that you have deleted ▼") )
     (setq skk-henkan-active nil) )))

(defun skk-delete-henkan-markers (&optional nomesg)
  ;; 変換時にカレントバッファに表われる `▽', `▼' マークを消す。
  (if (not (marker-position skk-henkan-start-point))
      nil
    (combine-after-change-calls
      (save-match-data
        (skk-save-point
         (goto-char (- skk-henkan-start-point skk-kanji-len))
         (if skk-henkan-active
             (progn
               (if skk-use-face (skk-henkan-face-off))
               (if (looking-at "▼")
                   (delete-char 1)
                 (or nomesg
                     (skk-message "▼がありません"
                                  "It seems that you have deleted ▼" ))))
           (if (looking-at "▽")
               (delete-char 1)
             (or nomesg
                 (skk-message "▽がありません"
                              "It seems that you have deleted ▽" )))))))))

(defun skk-delete-okuri-mark ()
  ;; 送り仮名入力中にカレントバッファに表われる `*' マークを消し、送り仮名関連
  ;; フラグを nil にセットする。
  (if (not (marker-position skk-okurigana-start-point))
      nil
    (skk-save-point
      (if (eq (char-after skk-okurigana-start-point) ?*) ; ?*
          (delete-region skk-okurigana-start-point
                         (1+ skk-okurigana-start-point) ))
      (setq skk-okurigana nil
            skk-okuri-char nil
            skk-henkan-okurigana nil ))))
            
;;;; jisyo related functions
(defun skk-purge-from-jisyo ()
  "▼モードで現在の候補を辞書バッファから消去する。"
  (interactive "*")
  (if (and skk-henkan-active (not (string= skk-henkan-key "")))
      (if (not
           (yes-or-no-p (format
                         (if skk-japanese-message-and-error
                             "%s /%s/%sを辞書から削除します。良いですか？"
                           "Really purge \"%s /%s/%s\"?" )
                         skk-henkan-key (skk-get-simply-current-candidate)
                         (if (and skk-henkan-okurigana
                                  (or skk-henkan-okuri-strictly
				      skk-henkan-strict-okuri-precedence ))
                             (concat
                              (if skk-japanese-message-and-error
                                  " (送り仮名: "
                                "(okurigana: " )
                              skk-henkan-okurigana
                              ") " )
                           " " ))))
          nil
        ;; skk-henkan-start-point から point まで削除してしまっても、変換直後
        ;; に (カーソルを動かすことなく) skk-purge-from-jisyo を呼べば問題ない
        ;; が、カーソルが違う場所へ移動していた場合は、削除すべきでないものま
        ;; で削除してしまう可能性がある。そこで、送り仮名があればその長さを含
        ;; めた end を求め、今回の変換に関連した個所だけを正確に切り取るように
        ;; する。
        (let ((end (if skk-henkan-okurigana
                       (+ (length skk-henkan-okurigana)
                          skk-henkan-end-point )
                     skk-henkan-end-point ))
              (word (skk-get-simply-current-candidate (skk-numeric-p))) )
          ;;(if skk-use-numeric-conversion
          ;;    (skk-update-jisyo-for-numerals purge-word 'purge) )
          (skk-update-jisyo word 'purge)
          ;; 念のため。skk-previous-candidate を参照。 
          (if skk-use-face (skk-henkan-face-off))
          (delete-region skk-henkan-start-point end)
          (skk-change-marker-to-white)
          (skk-kakutei)
	  (if skk-use-relation
	      (skk-update-relation
               (if skk-use-numeric-conversion
                   (skk-compute-numeric-henkan-key skk-henkan-key)
		 skk-henkan-key )
               (or skk-henkan-okurigana skk-okuri-char)
	       word nil 'purge))
          ;;(if (boundp 'skk-attr-alist)
          ;;    (skk-attr-purge
          ;;     (if skk-use-numeric-conversion
          ;;         (skk-compute-numeric-henkan-key skk-henkan-key)
          ;;       skk-henkan-key )
          ;;     (or skk-henkan-okurigana skk-okuri-char)
          ;;     word ))
	  ))))

;;;###skk-autoload
(defun skk-save-jisyo (&optional quiet)
  "SKK の辞書バッファをセーブする。
オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを出さな
い。"
  (interactive "P")
  (let* ((skk-jisyo (expand-file-name skk-jisyo))
         (jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg)) )
    (if (or (not jisyo-buffer) (not (buffer-modified-p jisyo-buffer)))
        (if (not quiet) 
            (progn
              (skk-message "SKK 辞書を保存する必要はありません"
                           "No need to save SKK jisyo" )
              (sit-for 1) ))
      (with-current-buffer jisyo-buffer
        (let ((inhibit-quit t)
              (tempo-file (skk-make-temp-jisyo)) )
          (if (not quiet)
              (skk-message "SKK 辞書を保存しています..."
                           "Saving SKK jisyo..." ))
          (skk-save-jisyo-1 tempo-file)
          (skk-check-jisyo-size tempo-file)
          ;; 辞書のセーブに成功して初めて modified フラッグを nil にする。
          (set-buffer-modified-p nil)
          (if (not quiet)
              (progn
                (skk-message "SKK 辞書を保存しています...完了！"
                             "Saving SKK jisyo...done" )
                (sit-for 1) ))
          (if (eq this-command 'save-buffers-kill-emacs)
              (skk-record-jisyo-data jisyo-buffer) )))
      (skk-set-cursor-properly) )))

(defun skk-save-jisyo-1 (file)
  (save-match-data
    (let (mc-flag enable-multibyte-characters buffer-read-only)
      (goto-char (point-min))
      (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
          nil
        (skk-error
         "送りありエントリのヘッダーがありません！ SKK 辞書のセーブを中止します"
         "Header line for okuri-ari entries is missing!  Stop saving SKK jisyo" ))
      ;; おっ、コメントフェイスが $" で終わらないぞ > hilit19.el
      (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
          nil
        (skk-error
         "送りなしエントリのヘッダーがありません ！ SKK 辞書のセーブを中止します"
         "Header line for okuri-nasi entries is missing!  Stop saving SKK jisyo" )))
    (write-region 1 (point-max) file nil 'nomsg) ))

(defun skk-check-jisyo-size (new-file)
  (let ((new-size (nth 7 (file-attributes new-file)))
        old-size )
    (if (eq new-size 0)
        (progn
          (delete-file new-file)
          (skk-error "SKK 辞書が空になっています！ 辞書のセーブを中止します"
                     "Null SKK jisyo!  Stop saving jisyo" )))
    (if (or (not skk-compare-jisyo-size-when-saving)
            ;; 旧辞書とのサイズ比較を行なわない。
            (progn
              ;; (1)skk-jisyo がないか、
              ;; (2)new-file と skk-jisyo が同一のサイズか
              ;;    (skk-(aux-)large-jisyo から新規の単語を読み込まなかったり、
              ;;    新規単語の登録を行なわなかった場合はサイズが同じ)、
              ;; (3)new-file の方が大きい
              ;; 場合 (上記の 3 通りであればいずれも正常)。
              (setq old-size (nth 7 (file-attributes skk-jisyo)))
              (or (not old-size)
                  (>= new-size old-size) )))
        (skk-make-new-jisyo new-file)
      ;; yes-or-no-p に回答し、newline すると、this-command が変ってしまう。
      (let (this-command this-command-char last-command last-command-char)
        (if (skk-yes-or-no-p
             (format
              "skk-jisyo が %dbytes 小さくなりますが、セーブして良いですか？"
              (- old-size new-size) )
             (format
              "New %s will be %dbytes smaller.  Save anyway?"
              skk-jisyo (- old-size new-size) ))
            ;; とにかくセーブ。
            (skk-make-new-jisyo new-file)
          ;; セーブとり止め。
          (delete-file new-file)
          (with-output-to-temp-buffer "*SKK warning*"
            (if skk-japanese-message-and-error
                (progn
                  (princ "セーブしようとする辞書のサイズが元のものよりも小さなってしまうので、")
                  (terpri)
                  (princ "セーブを途中で中止しました。辞書のサイズが小さくなった原因には例え")
                  (terpri)
                  (princ "ば、")
                  (terpri)
                  (terpri)
                  (princ "    ・M-x skk-purge-from-jisyo を実行した。")
                  (terpri)
                  (terpri)
                  (princ "    ・.skk-jisyo の漢字コードと、\" *.skk-jisyo*\" バッファの漢字コード")
                  (terpri)
                  (princ "      が異なっている。")
                  (terpri)
                  (terpri)
                  (princ "    ・\" *.skk-jisyo*\" バッファを自分で編集した。")
                  (terpri)
                  (terpri)
                  (princ "などが考えられます (最初の 2 つが原因であれば、異常ではありません。")
                  (terpri)
                  (princ "最後の場合は、あなたがどのような編集をしたかによります)。原因を確認")
                  (terpri)
                  (princ "後、慎重に辞書のセーブを行なうことをお勧めします。")
                  (terpri)
                  (terpri)
                  (princ "元の辞書を再度読み込むには、")
                  (terpri)
                  (terpri)
                  (princ "    M-x skk-reread-private-jisyo")
                  (terpri)
                  (terpri)
                  (princ "を実行して下さい。") )
              (princ "As size of your private JISYO to be saved is smaller than the")
              (terpri)
              (princ "original, we have stopped saving JISYO.  For example, the following")
              (terpri)
              (princ "condition makes a smaller private JISYO;")
              (terpri)
              (terpri)
              (princ "    (a)You executed M-x skk-purge-from-jisyo,")
              (terpri)
              (terpri)
              (princ "    (b)Kanji code of .skk-jisyo is different from the one of")
              (terpri)
              (princ "       \" *.skk-jisyo*\" buffer, or")
              (terpri)
              (terpri)
              (princ "    (c)You edited \" *.skk-jisyo*\" buffer manually.")
              (terpri)
              (terpri)
              (princ "The first two condition is not strange, but the last one depends on")
              (terpri)
              (princ "how you edited JISYO.  We strongly recommend to save JISYO")
              (terpri)
              (princ "carefully after checking what causes this.")
              (terpri)
              (princ "If you want to reread your original private JISYO, type")
              (terpri)
              (terpri)
              (princ "    M-x skk-reread-private-jisyo")
              (terpri) ))
          (skk-error "SKK 辞書のセーブを中止しました！"
                     "Stop saving SKK jisyo!" ))))))

(defun skk-make-temp-jisyo ()
  ;; SKK 個人辞書保存のための作業用のファイルを作り、ファイルのモードを
  ;; skk-jisyo のものと同じに設定する。作った作業用ファイルの名前を返す。
  (let ((tempo-name (skk-make-temp-file "skkdic")))
    (skk-create-file tempo-name)
    (set-file-modes tempo-name  (file-modes skk-jisyo))
    tempo-name ))

(defun skk-make-temp-file (prefix)
  ;; from call-process-region of mule.el.  Welcome!
  (make-temp-name
   (if (null (memq system-type '(ms-dos windows-nt)))
       (concat "/tmp/" prefix)
     (let ((tem (or (getenv "TMP") (getenv "TEMP") "/")))
       (concat tem
               (if (memq (aref tem (1- (length tem))) '(47 92)) ;?/, ?\\
                   "" "/" )
               prefix )))))

(defun skk-make-new-jisyo (tempo-file)
  ;; TEMPO-FILE を新規の skk-jisyo にする。skk-backup-jisyo が non-nil だった
  ;; らバックアップ辞書を作る。
  (if skk-backup-jisyo
      (progn
        (if (file-exists-p skk-backup-jisyo)
            (delete-file skk-backup-jisyo) )
        (rename-file skk-jisyo skk-backup-jisyo) )
    (delete-file skk-jisyo) )
  (rename-file tempo-file skk-jisyo 'ok-if-already-exists) )

(defun skk-reread-private-jisyo ()
  "バッファに読み込んだ個人辞書を破棄し、ファイルからバッファへ再読み込みする。"
  (interactive)
  (let ((buf (skk-get-jisyo-buffer skk-jisyo 'nomsg)))
    (if (and buf
             (skk-yes-or-no-p "編集中の個人辞書を破棄しますか？"
                              "Discard your editing private JISYO?" ))
        (progn
          (save-excursion
            (set-buffer buf)
            (set-buffer-modified-p nil)
            (kill-buffer buf) )
          (or
           (skk-get-jisyo-buffer skk-jisyo 'nomsg)
           (skk-error "個人辞書を再読み込みすることができません！"
                      "Cannot reread private JISYO!" ))))))

(defun skk-record-jisyo-data (jisyo-buffer)
  ;; 辞書データーを取り、Emacs の終了の際であれば、そのデーターを 
  ;; skk-record-file に保存し、それ以外であれば、それをエコーする。
  (if (or (not skk-keep-record) (> 1 skk-kakutei-count))
      nil
    (with-temp-file skk-record-file
      (insert-file-contents skk-record-file)
      (goto-char (point-min))
      (insert
       (format
        "%s  登録: %3d  確定: %4d  確定率: %3d%%  語数:%6d\n"
        (current-time-string)
        skk-touroku-count skk-kakutei-count
        (/ (* 100 (- skk-kakutei-count skk-touroku-count))
           skk-kakutei-count )
        (if skk-count-private-jisyo-candidates-exactly
            (skk-count-jisyo-candidates (expand-file-name skk-jisyo))
          ;; 1 行 1 候補とみなす。
          (with-current-buffer jisyo-buffer
            (- (count-lines (point-min) (point-max)) 2) )))))
    (setq skk-touroku-count 0 skk-kakutei-count 0) ))

(defun skk-count-jisyo-candidates (file)
  "SKK 辞書の候補数を数える。
`[' と `]' に囲まれた送り仮名毎のブロック内は数えない。"
  (interactive
   (list (read-file-name
          (format "Jisyo File: (default: %s) " skk-jisyo)
          "~/" skk-jisyo 'confirm )))
  ;; mule@emacs19.31 だと下記のようにすると (`ァ' が原因のよう) 何故か 
  ;; default-directory の末尾に改行が付く。
  ;; 通常は気が付かないが、rsz-mini.el を使って resize-minibuffer-mode を 
  ;; non-nil にしていると不要な 2 行目が出現する。
  ;; (interactive "f辞書ファイル: ")
  (with-current-buffer (find-file-noselect file)
    (save-match-data
      (let ((count 0)
            (min (point-min))
            (max (if (interactive-p) (point-max)))
            (interactive-p (interactive-p))
            mc-flag enable-multibyte-characters )
        (goto-char min)
        (if (or
             ;; こちらは skk-save-point を使わず、ポイントを移動させる。
             (not (re-search-forward "^;; okuri-ari entries.$" nil t nil))
             (not
              (skk-save-point
                (re-search-forward "^;; okuri-nasi entries.$" nil t nil) )))
            (skk-error "このファイルは SKK 辞書ではありません"
                       "This file is not a SKK dictionary") )
        (while (search-forward "/" nil t)
          (cond ((looking-at "\\[")
                 (forward-line 1)
                 (beginning-of-line) )
                ((not (eolp))
                 (setq count (1+ count)) ))
          (if interactive-p
              (message "Counting jisyo candidates...%3d%% done"
                       (/ (* 100 (- (point) min)) max) )))
        (if (interactive-p)
            (message "%d entries" count)
          count )))))

(defun skk-create-file (file &optional message)
  ;; FILE がなければ、FILE という名前の空ファイルを作る。
  ;; オプショナル引数の MESSAGE を指定すると、ファイル作成後そのメッセージを
  ;; ミニバッファに表示する。
  (let ((file (expand-file-name file)))
    (or (file-exists-p file)
        (progn
          (write-region 1 1 file nil 0)
          (if message
              (progn
                (message message)
                (sit-for 3) ))))))

(defun skk-get-jisyo-buffer (file &optional nomsg)
  ;; FILE を開いて SKK 辞書バッファを作り、バッファを返す。
  ;; オプショナル引数の NOMSG を指定するとファイル読み込みの際のメッセージを
  ;; 表示しない。
  (if file
      (let ((inhibit-quit t)
            ;; expand-file-name を呼んでおかないと、使っている OS 上ありえない
            ;; ファイルネームでも、そのまま渡されてしまう。
            ;; (例) MSDOS の場合 ~/_skk-jisyo の実際のファイルネームは、OS 上
            ;; の制限から ~/_skk-jis となる。expand-file-name を呼ばないと、
            ;; " *_skk-jisyo*" というバッファができてしまい、skk-save-jisyo な
            ;; ど他の関数に影響が出る。
            (file (expand-file-name file))
            (jisyo-buf (concat " *" (file-name-nondirectory file)
                               "*" )))
        ;; 辞書バッファとしてオープンされているなら、何もしない。
        (or (get-buffer jisyo-buf)
            (with-current-buffer (setq jisyo-buf (get-buffer-create jisyo-buf))
              (buffer-disable-undo jisyo-buf)
              (auto-save-mode -1)
              ;; ワーキングバッファのモードラインはアップデートされない？
              ;;(make-local-variable 'line-number-mode)
              ;;(make-local-variable 'column-number-mode)
              (setq buffer-read-only nil
                    case-fold-search nil
                    ;; buffer-file-name を nil にすると、辞書バッファに入り込
                    ;; んで C-x C-s したときにファイルネームを尋ねてくるように
                    ;; なるが、M-x compile など内部で save-some-buffers をコー
                    ;; ルしているコマンドを使ったときでもセーブするかどうかを
                    ;; 尋ねてこなくなる。
                    ;;buffer-file-name file
                    ;;cache-long-line-scans nil
                    ;;column-number-mode nil
                    ;;line-number-mode nil
                    ;; dabbrev のサーチとなるバッファにならないように存在しな
                    ;; いモード名にしておく。実害のある副作用はないはず。
                    major-mode 'skk-jisyo-mode
                    mode-name "SKK辞書" )
              (or nomsg
                  (skk-message "SKK 辞書 %s をバッファに読み込んでいます..."
                               "Inserting contents of %s ..."
                               (file-name-nondirectory file) ))
	      (let ((coding-system-for-read 'euc-japan); XXX
		    enable-character-unification)
                 (insert-file-contents file) )
              (skk-set-jisyo-code)
              (or nomsg
                  (skk-message
                   "SKK 辞書 %s をバッファに読み込んでいます...完了！"
                   "Inserting contents of %s ...done"
                   (file-name-nondirectory file) ))
              (skk-setup-jisyo-buffer)
              (set-buffer-modified-p nil)
              jisyo-buf )))))

(defun skk-set-jisyo-code ()
  ;; 文字コードを CODE にセットする。
  (if (not skk-jisyo-code)
      nil
    (if (stringp skk-jisyo-code)
        (setq skk-jisyo-code (cdr
                              (assoc skk-jisyo-code skk-coding-system-alist) )))
    (if (fboundp 'set-buffer-file-coding-system)
        (set-buffer-file-coding-system skk-jisyo-code)
      (set-file-coding-system skk-jisyo-code) )))

(defun skk-setup-jisyo-buffer ()
  ;; skk-jisyo の辞書バッファで、
  ;; (1)空バッファであれば、新しくヘッダーを作り、
  ;; (2)辞書エントリがある既存の辞書バッファならば、ヘッダーが正しいかどうかを
  ;;    チェックする。
  ;;
  ;; skk-okuri-ari-min と skk-okuri-nasi-min の位置を変更した。
  ;;                       ↓ 新しい skk-okuri-ari-min
  ;;   ;; okuri-ari entries.
  ;;   ← 以前の skk-okuri-ari-min
  ;;
  ;;   ↓ skk-okuri-ari-max ↓ 新しい skk-okuri-nasi-min
  ;;   ;; okuri-nasi entries.
  ;;   ← 以前の skk-okuri-nasi-min
  ;;
  ;;
  ;; 変更前の位置であれば、下記のような空辞書の場合、
  ;;
  ;;   ;; okuri-ari entries.
  ;;   ;; okuri-nasi entries.
  ;;
  ;; skk-okuri-ari-min と skk-okuri-ari-max のマーカーが重なってしまい、
  ;; skk-okuri-ari-min の位置に挿入したエントリが skk-okuri-ari-max のマーカー
  ;; を後方に押しやらない。
  ;;
  ;; この関数のオリジナルの名称は、j-check-jisyo だったが、skk-check-jisyo と
  ;; いう名前にすると skk-tools.el 内の関数名と重複する。
  ;; case-fold-search は、辞書バッファでは常に nil。
  (let (mc-flag enable-multibyte-characters)
    (save-match-data
      (if (eq (buffer-size) 0)
          ;; 空バッファだったら、ヘッダーのみ挿入。
          (insert ";; okuri-ari entries.\n" ";; okuri-nasi entries.\n") )
      (goto-char (point-min))
      (if (re-search-forward "^;; okuri-ari entries.$" nil 'noerror)
          ;; 固定ポイントなので、(point) で十分。
          (setq skk-okuri-ari-min (point))
        (skk-error "送りありエントリのヘッダーがありません！"
                   "Header line for okuri-ari entries is missing!" ))
      (if (re-search-forward "^;; okuri-nasi entries.$" nil 'noerror)
          (progn
            (beginning-of-line)
            ;; 共有辞書なら固定ポイントでも良いのだが、辞書バッファで編集を行
            ;; なったときのことを配慮してマーカーにしておく。
            (setq skk-okuri-ari-max (point-marker))
            (forward-line 1)
            (backward-char 1)
            (setq skk-okuri-nasi-min (point-marker)) )
        (skk-error "送りなしエントリのヘッダーがありません！"
                   "Header line for okuri-nasi entries is missing!" )))))

;;;###skk-autoload
(defun skk-search ()
  ;; skk-current-search-prog-list の要素になっているプログラムを評価して、
  ;; skk-henkan-keyをキーにして検索を行う。
  (let (l)
    (while (and (null l) skk-current-search-prog-list)
      (setq l (eval (car skk-current-search-prog-list))
            skk-current-search-prog-list (cdr skk-current-search-prog-list) ))
    l ))

(defun skk-search-relation ()
  (let ((last 
	 (if (not (and
		   skk-last-henkan-result
		   (markerp skk-last-henkan-point)
		   (< skk-last-henkan-point (point))
		   (< (- (point) skk-last-henkan-point) skk-relation-length) ))
	     nil
	   skk-last-henkan-result )))
    (skk-attr-search-relation
     skk-henkan-key
     (or skk-henkan-okurigana skk-okuri-char)
     last )))

(defun skk-attr-search-relation (midasi okuri last)
  (if skk-debug (message "%S" midasi okuri last))
  (or skk-attr-alist (skk-attr-read))
  (let ((entries (cdr (skk-attr-get-table-for-midasi midasi okuri)))
	ret rest )
    (if skk-debug (message "%S" entries))
    (while entries
      (let* ((entry (car entries))
	     (rel (assq 'relation (cdr entry))))
	(if (or (null last) (member last (cdr rel)))
	    (setq ret (nconc ret (list (car entry))))
	  (setq rest (nconc rest (list (car entry)))) ) )
      (setq entries (cdr entries)) )
    (nconc ret rest) ))
	     
(defun skk-update-relation (midasi okuri word last &optional purge)
  (skk-attr-update-relation midasi okuri word last purge) )

(defun skk-attr-update-relation (midasi okuri word last &optional purge)
  (or skk-attr-alist (skk-attr-read))
  (if skk-debug (message "update %S %S %S %S" midasi okuri word last))
  (if purge
      (skk-attr-purge midasi okuri word)
    (let* ((table (skk-attr-get-table-for-midasi midasi okuri))
	   (entry (assoc word (cdr table)))
	   (oldattr (assq 'relation (cdr entry)))
	   (listlast (if last (list last) nil)) )
      (cond (oldattr
	     (if  last
		 (progn
		   (setcdr oldattr (cons last
					 (delete last (cdr oldattr)) ))
		   (let ((tail (nthcdr skk-relation-record-num oldattr)))
		     (if tail
			 (setcdr tail nil) )))))
	    (entry
	     (setcdr entry (cons (cons 'relation listlast)
				 (cdr entry))))
	    (table
	     (setcdr table (cons (list word
				       (list 'okurigana okuri)
				       (cons 'relation listlast) )
				 (cdr table) )))
	    ;; new entry
	    (t (skk-attr-put-1 midasi okuri word 'relation listlast)) ))))

(defun skk-search-jisyo-file (file limit &optional nomsg)
  ;; SKK 辞書フォーマットの FILE で skk-henkan-key をキーにして検索を行う。
  ;; 検索リージョンが LIMIT 以下になるまでバイナリサーチを行い、その後リニア
  ;; サーチを行う。
  ;; LIMIT が 0 であれば、リニアサーチのみを行う。
  ;; 辞書がソートされていないのであれば、LIMIT を 0 する必要がある。
  ;; オプショナル引数の NOMSG が non-nil であれば skk-get-jisyo-buffer のメッ
  ;; セージを出力しないようにする。
  (let ((jisyo-buffer (skk-get-jisyo-buffer file nomsg)))
    (if jisyo-buffer
        ;; skk-henkan-key と skk-henkan-okurigana はカレントバッファのローカル
        ;; 値。
        (let ((okurigana (or skk-henkan-okurigana skk-okuri-char))
              (midasi 
               (if skk-use-numeric-conversion
		   ;; skk-henkan-key が nil のことがある。何故?
                   (skk-compute-numeric-henkan-key skk-henkan-key)
                 skk-henkan-key ))
              entry-list entry )
          (with-current-buffer jisyo-buffer
            (setq skk-henkan-key midasi
                  entry-list (skk-search-jisyo-file-1 okurigana limit) )
            (if entry-list
                (progn
                  (setq entry
                        (cond ((and okurigana skk-henkan-okuri-strictly)
                               ;; 送り仮名が同一のエントリのみを返す。
                               (nth 2 entry-list) )
                              ((and okurigana skk-henkan-strict-okuri-precedence)
                               ;; 送り仮名が同一のエントリのうしろに、
                               ;; その他のエントリをつけてかえす。
                               (skk-nunion (nth 2 entry-list) (car entry-list)))
                              (t (car entry-list)) ))
                  (if (and (boundp 'skk-attr-search-function)
                           skk-attr-search-function )
                      (funcall skk-attr-search-function midasi okurigana entry)
                    entry ))))))))

(defun skk-search-jisyo-file-1 (okurigana limit &optional delete)
  ;; skk-search-jisyo-file のサブルーチン。skk-compute-henkan-lists を使用し、
  ;; 見出し語についてのエントリの情報を返す。
  ;; DELETE が non-nil であれば、MIDASI にマッチするエントリを削除する。
  (let ((key (concat "\n" skk-henkan-key " /"))
        min max size p )
    (save-match-data
      ;; skk-okuri-ari-min と skk-okuri-ari-max は辞書バッファのローカル値。
      (if okurigana
          (setq min skk-okuri-ari-min
                max skk-okuri-ari-max )
        (setq min skk-okuri-nasi-min
              max (point-max) ))
      (if (> limit 0)
          (while (progn (setq size (- max min)) (> size limit))
            (goto-char (+ min (/ size 2)))
            (beginning-of-line)
            (setq p (point))
            ;; 送りありなら逆順に比較を行なう。
            (if
                (if okurigana
                    (string< (skk-buffer-substring p (1- (search-forward  " ")))
                             skk-henkan-key )
                  (string< skk-henkan-key
                           (skk-buffer-substring p (1- (search-forward " "))) ))
                (setq max p)
              (setq min p) )))
      (goto-char min)
      ;; key が検索開始地点にあった場合でも検索可能なように一文字戻る。key が
      ;; その先頭部分に "\n" を含んでいることに注意。
      (or (bobp) (backward-char 1))
      ;; 効率が良いように kanji-flag, mc-flag, enable-multibyte-characters を
      ;; nil にしておく。
      ;; case-fold-search は、辞書バッファでは常に nil。
      (let (mc-flag)
            ;; enable-multibyte-characters)
        (if (search-forward key max 'noerror)
            (prog1
                (skk-compute-henkan-lists okurigana)
              (if delete
                  (progn
                    (beginning-of-line)
                    (delete-region (point)
                                   (progn (forward-line 1) (point)) )))))))))


(defun skk-compute-henkan-lists (okurigana)
  ;; 辞書エントリを 4 つのリストに分解する。
  ;;
  ;; 送りなし (例えば、辞書エントリ "てんさい /転載/天災/天才/" の処理)
  ;; entry1 := ("転載" "天災" "天才") == 全エントリ
  ;; entry2 := nil
  ;; entry3 := nil
  ;; entry4 := nil
  ;;
  ;; 送りあり (例えば、「泣く」の変換を行った場合の、辞書エントリ
  ;;           "なk /亡/無/鳴/泣/[く/無/鳴/泣/]/[き/亡/]/" の処理)
  ;; entry1 := ("亡" "無" "鳴" "泣")  == 漢字部分の全エントリ
  ;; entry2 := ("[く")                == 他の送り仮名を使う漢字エントリ (あれ
  ;;                                     ば) + 今回の変換の送り仮名部分
  ;; entry3 := ("無" "鳴" "泣")       == 今回の変換の送り仮名を使う可能性の
  ;;                                     ある全漢字エントリ
  ;; entry4 := ("]" "[き" "亡" "]")   == 他の送り仮名を使う漢字エントリ (残
  ;;                                     り。あれば)
  ;;
  ;;   * "[" は直後に続くひらがなを送り仮名に持つ漢字のエントリの初まりを表し、
  ;;     "]" は、該当の送り仮名グループの終りを示す。
  ;;
  ;; この関数は、変換時と、確定直後の辞書のアップデート時の 2 度呼ばれる
  ;; (変換時に検索を行った辞書が、skk-jisyo とは限らないので、2 度計算せざる
  ;; を得ない)。
  ;;
  ;; 変換時は、skk-henkan-okuri-strictly が non-nil であれば、
  ;; 計算結果の entry3を、skk-henkan-okuri-strictly が nil であって
  ;; かつ skk-henkan-strict-okuri-precedence が non-nil あれば
  ;; (skk-uniion entry3 entry1) を取り出す。
  ;; ふたつの変数がともに nil の場合は entry1 を取り出す。
  (if (not okurigana)
      (list (string-split
             "/"
             (skk-buffer-substring (point)
                                   (progn (end-of-line) (1- (point))) ))
            nil nil nil )
    (save-match-data
      (let ((stage 1) (q1 (queue-create)) (q2 (queue-create))
            (q3 (queue-create)) (q4 (queue-create))
            (okuri-key (concat "\[" okurigana)) item headchar )
        (catch 'exit
          (while (not (eolp))
            (setq item (skk-buffer-substring (point) (1- (search-forward "/")))
                  headchar (if (string= item "") (skk-int-char 0) (aref item 0)) )
            (cond ((and (eq headchar ?\[) ; ?\[
                        (<= stage 2) )
                   (if (string= item okuri-key)
                       (progn (queue-enqueue q2 item)
                              (setq stage 3) )
                     (setq stage 2)
                     (queue-enqueue q2 item) ))
                  ((eq stage 1)
                   (queue-enqueue q1 item) )
                  ((eq stage 2)
                   (queue-enqueue q2 item) )
                  ((eq stage 3)
                   (if (eq headchar ?\]) ; ?\]
                       (progn (setq stage 4)
                              (queue-enqueue q4 item) )
                     (queue-enqueue q3 item) ))
                  ((eq stage 4)
                   (queue-enqueue q4 item) ))))
        ;;        entry1          entry2        entry3          entry4
        (list (queue-all q1) (queue-all q2) (queue-all q3) (queue-all q4)) ))))

;; キュー関連の関数の使用に際しては、「プログラムの構造と実行」(H. エーベルソ
;; ン、G.J.サスマン、J.サスマン著、元吉文男訳。マグロウヒル出版) と Elib (the
;; GNU emacs lisp library version 1.0) を参考にした。上記の文献で解説されてい
;; るキューの表現は、Elib の queue-m.el において実現されているものとほぼ同じ実
;; 装となっている。
;;
;; リストでのキューの表現は、具体的には例えば ((A B C D E F) F) のような形になっ
;; ており、car のリスト (A B C D E F) がキューの全体を表わし、キューの nth 1 
;; を取ったときの F がキューの最後尾を表わす。キューの cdr を取ったときの (F) 
;; というリストは、キューの car に対し nthcdr 5 を取ったときのリスト (F) と同
;; じものである。従い、cdr のリストの後に新しい要素を追加することで、car で表
;; わされるキューの末尾に新しい要素を追加することができる。
;; 一方、nconc や append でつなぐには、それらの関数の第１引数のリストの全ての
;; 要素を走査せねばならず、O(n) の時間がかかるので、長いリストをつなぐときは比
;; 較的コストがかかる。
;;
;; さて、空の queue == (cons nil nil) に対し、新しい要素 A を追加する方法を説
;; 明する。まず、新しい要素 A のみを含んだ長さ 1 のリスト (A) を作る (仮に 
;; new-pair という変数に取る)。次に、(setcar queue new-pair) を行なうことによ
;; り、queue が ((A)) となる (setcar, setcdr の返り値は、new-pair であることに
;; 注意)。次に (setcdr queue new-pair) して ((A) A) となったところを図示する。
;; front, rear の両方のポインタが (A) を指すようにする (キューの要素が A しか
;; ないので、front, rear ポインタともに同じものを指している)。
;;         queue
;;   +-------+-------+
;;   | Front |  Rear |
;;   +---|---+---|---+
;;       |       +---> +---------------+
;;       +------------>|   o   |  nil  |
;;                     +---|---+-------+
;;                         |      +-------+
;;                         +----> |   A   |
;;                                +-------+
;;
;; 上記の queue, ((A) A) に対し、更に新しい要素 B を追加する。例により B のみ
;; を含む長さ 1 のリスト (B) を作り、変数 new-pair に取る。ここで
;; (setcdr (cdr queue) new-pair) を評価すると (注1)、* の個所のポインタ操作が
;; 行なわれる。キューの最後方に新しい要素である B が追加されることになる。
;; queue は ((A B) A B) となる。
;;         queue
;;   +-------+-------+
;;   | Front |  Rear |
;;   +---|---+---|---+
;;       |       +---> +---------------+   *    +---------------+
;;       +------------>|   o   |   o --|------->|   o   |  nil  |
;;                     +---|---+-------+        +-------+-------+
;;                         |      +-------+         |      +-------+
;;                         +----> |   A   |         +----> |   B   |
;;                                +-------+                +-------+
;;
;;   注1; 追加前のキューの要素が 1 つのときは、front も rear も同じものを指し
;;        ているので (setcdr (car queue) new-pair) でも等価だが、キューの要素
;;        が 2 つ以上のときは (setcdr (cdr queue) new-pair) でないとまずい。
;;
;; 最後に (setcdr queue new-pair) を評価することにより、rear ポインタを張り変
;; える (* の個所のポインタ操作が行なわれる)。rear ポインタがキューの最後方の
;; 要素を指すようにする。front ポインタが指すリストはキューの全ての要素を表わ
;; す。
;;         queue
;;   +-------+-------+           *
;;   | Front |  Rear |---------------------+
;;   +---|---+-------+                     |
;;       |             +---------------+   +--> +---------------+
;;       +------------>|   o   |   o --|------->|   o   |  nil  |
;;                     +---|---+-------+        +-------+-------+
;;                         |      +-------+         |      +-------+
;;                         +----> |   A   |         +----> |   B   |
;;                                +-------+                +-------+
;;
;; このようにキューの最後方に新しい要素を追加すること (リストの最後方に長さ 1
;; の新しいリストをつなげること) が 2 回のポインタ操作で可能となるので、どのよ
;; うな長いリストであっても連結にかかるコストは一定 (O(1) の関数である) である。
;; なお、現状では、平均して安価にリストの最後方に要素をつなげる、という目的に
;; だけキューを使っている。キュー本来の目的では使用しておらないので、例えば、
;; 下記のような関数は使用していない。
;; queue-last, queue-first, queue-nth, queue-nthcdr, queue-dequeue

;;;###skk-autoload
(defun skk-nunion (x y)
  ;; X と Y の和集合を作る。等しいかどうかの比較は、equal で行われる。X に Y
  ;; を破壊的に連接する。
  (cond ((null x) y)
        ((null y) x)
        (t (let ((e x))
             (while e
               (setq y (delete (car e) y)
                     e (cdr e) ))
             (if y
                 ;; 上記の while ループの中の delete と下記の nconc とを合わせ
                 ;; て、全部で X を 2 回、Y を X 回走査したことになる。ソー
                 ;; トされていない集合同士から集合を作る以上 (候補はソートして
                 ;; はならない) 、逐次走査になるのでこれは止むを得ないか...。
                 (nconc x y)
               x )))))

(defun skk-search-kakutei-jisyo-file (file limit &optional nomsg)
  ;; 辞書ファイルを探し、候補をリストで返す。
  ;; 候補を見つけた場合は、大域変数 skk-kakutei-flag に non-nil を代入する。
  ;; 候補が見つからなかった場合は、nil を返す。
  (setq skk-kakutei-flag (skk-search-jisyo-file file limit nomsg)) )

;;;###skk-autoload
(defun skk-update-jisyo (word &optional purge)
  ;; WORD が次の変換時に最初の候補になるように、プライベート辞書を更新する。
  ;; PURGE が non-nil で WORD が共有辞書にあるエントリなら skk-ignore-dic-word
  ;; 関数でクォートしたエントリをプライベート辞書に作り、次の変換から出力しな
  ;; いようにする。
  ;; WORD が共有辞書になければ、プライベート辞書の辞書エントリから削除する。
  ;;
  ;; SKK 9.x より、プライベート辞書のエントリの挿入の方法を変更した (9.3 のみ
  ;; は例外)。
  ;;
  ;; 【変更前】
  ;;         ;; okuri-ari entries.
  ;;  見キ   わるk /悪/[か/悪/]/[く/悪/]/
  ;;  出ー   わるi /悪/[い/悪/]/
  ;;  しに   わたs /渡/[さ/渡/]/[せ/渡/]/
  ;;  語降   わすr /忘/[れ/忘/]/
  ;;  を順   わかt /分/判/[った/分/判/]/[って/分/]/
  ;;   ↓     .....
  ;;         あi /合/[い/合/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; 【変更後】
  ;;         ;; okuri-ari entries.
  ;;  変で   でt /出/[て/出/]/[た/出/]/
  ;;  換昇   つi /付/[い/付/]/
  ;;  順順   けs /消/[す/消/]/[し/消/]/[せ/消/]/[さ/消/]/
  ;;   ↓    かえs /返/[し/返/]/[す/返/]/[さ/返/]/[せ/返/]/
  ;;         ...
  ;;         ...
  ;;         ながs /長/流/[し/流/]/[さ/長/]/[そ/流/]/
  ;;         ;; okuri-nasi entries.
  ;;  変で   じょうたい /状態/
  ;;  換昇   そうにゅう /挿入/
  ;;  順順   かな /仮名/
  ;;   ↓    ...
  ;;         ...
  ;;
  ;; skk-auto-okuri-process が non-nil のときに、(j-okuri-search 改め)
  ;; skk-okuri-search は見出し語の長い順に候補を返す必要がある。
  ;; SKK 8.6 までは、skk-okuri-search が j-okuri-ari-min から j-okuri-ari-max
  ;; までを順に探し、見つけたもの順に候補を返すためにプライベート辞書が見出し
  ;; 語をキーとして降順にソートされている必要があった。
  ;; SKK 9.x では、skk-okuri-search が、見付けた候補を見出し語をキーとして昇順
  ;; にソートして返すため、プライベート辞書のソートは必要でない。よって、最後
  ;; に変換したものを (j-okuri-ari-min 改め) skk-okuri-ari-min の位置に挿入す
  ;; る。
  ;;
  (let ((jisyo-buffer (skk-get-jisyo-buffer skk-jisyo 'nomsg))
	(midasi 
	 (if skk-use-numeric-conversion
	     (skk-compute-numeric-henkan-key skk-henkan-key)
	   skk-henkan-key ))
	(last skk-last-henkan-result)
	(last-point skk-last-henkan-point)
	(here (point)) )
    (if jisyo-buffer
        (let ((inhibit-quit t) buffer-read-only old-entry okurigana)
          (if skk-auto-okuri-process
              (setq word (skk-remove-common word)) )
          (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
                ;; midasi skk-henkan-key )
          (with-current-buffer jisyo-buffer
            ;; 既存エントリを検索後消去する。挿入すべきエントリが entry1 に 1
            ;; つしかなく、word と同じ文字であっても、いったん消してそのエント
            ;; リを min ポイントに移動させなければならない (読みの補完を行うと
            ;; きは、min ポイントから見出しを探すため、新しい見出しほど、min
            ;; ポイントに近いところになければならない)。
            (setq skk-henkan-key midasi
                  old-entry (skk-search-jisyo-file-1 okurigana 0 'delete) )
            (skk-update-jisyo-1 okurigana word old-entry purge)
	    (if skk-use-relation
		(progn
		  (if (not (and
			    last
			    (markerp last-point)
			    (< last-point here)
			    (< (- here last-point) skk-relation-length) ))
		      (setq last nil) )
		  (skk-update-relation
		   midasi okurigana word last purge) ))
	    (if skk-debug (message "%S %S %S" last last-point here))
            ;;(if (featurep 'skk-attr)
            ;;    (progn
            ;;      (and skk-attr-default-update-function
            ;;           (funcall skk-attr-default-update-function midasi
            ;;                    okurigana word purge ))
            ;;      (and skk-attr-update-function
            ;;           (funcall skk-attr-update-function midasi okurigana
            ;;                    word purge ))))
            ;; auto save.
            (if skk-jisyo-save-count
                (if (> skk-jisyo-save-count skk-update-jisyo-count)
                    (setq skk-update-jisyo-count (1+ skk-update-jisyo-count))
                  (setq skk-update-jisyo-count 0)
                  (skk-save-jisyo 'quiet) ))
            ;; 辞書バッファをオープンしたときに auto-save-mode をオフにしてお
            ;; けば毎度下記のような操作をしなくて済む。
            ;;
            ;; こうしておけば、辞書バッファはオートセーブされなくて済む。
            ;;(set-buffer-modified-p nil)
            )))))

(defun skk-update-jisyo-1 (okurigana word old-entry-list purge)
  ;; 既存エントリから計算した entry[1-4] の値と、今回の変換の結果 word とをマー
  ;; ジして、新たなエントリを計算し、挿入する。
  (let ((entry1 (car old-entry-list)) (entry2 (nth 1 old-entry-list))
        (entry3 (nth 2 old-entry-list)) (entry4 (nth 3 old-entry-list)) )
    (if (not purge)
        ;; entry1 の先頭のエントリを word にする。
        (setq entry1 (cons word (delete word entry1)))
      ;; 送りなし、もしくは skk-henkan-okuri-strictly と
      ;; skk-henkan-strict-okuri-precedence が nil の場合。
      (if (or (not okurigana) (not (or skk-henkan-okuri-strictly
				       skk-henkan-strict-okuri-precedence )))
          ;; entry1 を purge。共用辞書にあるエントリだったら、
          ;; skk-ignore-dic-word でクォートして次の変換から出力しないようにす
          ;; る。共用辞書にない文字列は word を消す。
          (if (skk-public-jisyo-contains-p okurigana word)
              (setq entry1 (skk-compose-ignore-entry entry1 word))
            (setq entry1 (delete word entry1)) )
        ;; 送りありで、かつ skk-henkan-okuri-strictly か
	;; skk-henkan-strict-okuri-precedence が non-nil の場合で、かつ
        ;; この word とペアになる送り仮名が okurigana しかないとき。
        (if (and okurigana (or skk-henkan-okuri-strictly
			       skk-henkan-strict-okuri-precedence )
                 (null (member word entry2)) (null (member word entry4)) )
            (setq entry1 (delete word entry1))
          ;; その他の場合は何もしない。
          )))
    (if (null entry1)
        ;; entry1 が null であれば、もう何もすることはない。
        nil
      (goto-char (if okurigana skk-okuri-ari-min skk-okuri-nasi-min))
      (insert "\n" skk-henkan-key " /")
      ;; entry1 -- 全エントリ (送りなしの場合) or 漢字部分の全エントリ (送りあ
      ;; りの場合)
      (insert (mapconcat 'skk-quote-char entry1 "/") "/")
      (if (not okurigana)
          nil
        ;; entry2 以降のエントリを処理するのは、送りありの場合のみ。
        ;; 先に挿入すべきエントリを計算、調整する。
        (if entry3
            (if (not purge)
                (setq entry3 (cons word (delete word entry3)))
              (setq entry3 (delete word entry3))
              (if (null entry3)
                  ;; entry3 として挿入するものが全くなければ、"/[く/]/" のよ
                  ;; うな送り仮名のみのエントリを作らないようにする (必要で
                  ;; あれば、entry2 の最後方と) entry4 の先頭のエントリ "]"
                  ;; を削除。
                  (let ((last2 (nthcdr (- (length entry2) 2) entry2)))
                    ;; entry2 の最後方は常に "[送り仮名" とは限らない。
                    (if (string= (nth 1 last2) (concat "[" okurigana))
                        (setcdr last2 nil) )
                    ;; entry4 の先頭は常に "]"。
                    (setq entry4 (cdr entry4)) )))
          ;; entry3 が null であれば
          (if (or skk-process-okuri-early purge)
              ;; skk-process-okuri-early が non-nil なら送り仮名が分らないので
              ;; 何もしない。-- 今回使用した送り仮名がわからないまま変換してい
              ;; るので、全てのエントリが entry2 に入っている -- entry3,
              ;; entry4 は null。
              ;; entry3 として挿入するものが全くなければ、何もしない -- entry3
              ;; が purge 前から null なら、entry2 の末尾は "[" でないし、
              ;; entry4 は null だから entry[234] の操作は不要。
              nil
            (setq entry2 (nconc entry2 (list (concat "[" okurigana)))
                  entry3 (list word)
                  ;; purge 前から entry3 が null だったのだから entry4 も null。
                  entry4 (list "]") ))))
      (if entry2
          ;; entry2 -- 今回使用しなかった送り仮名を使う漢字の候補群 + "[" + 今
          ;; 回使用した送り仮名 (送り仮名のみ。その送り仮名を使用する漢字の候
          ;; 補群は、entry3 に含まれる)。
          (progn
            (insert (mapconcat 'skk-quote-char entry2 "/" ) "/")
            ;; entry2 が null なら entry3 も null。
            (if entry3
                ;; entry3 -- 今回使用した送り仮名を使う全漢字エントリ
                (insert (mapconcat 'skk-quote-char entry3 "/") "/") )
            ;; purge で entry3 が null になった場合は entry4 が残っているとき
            ;; がある。
            (if entry4
                ;; entry4 -- "]" + 他の送り仮名を使う全漢字エントリ (entry2 の
                ;; 残り)。
                (insert (mapconcat 'skk-quote-char entry4 "/") "/") ))))))

(defun skk-quote-char (word)
  ;; 辞書の制限から辞書エントリ内に含めてはならない文字が WORD の中にあれば、
  ;; 評価したときにその文字となるような Lisp コードを返す。
  (save-match-data
    (if (and word
             (string-match "[/\n\r\"]" word)
             ;; we should not quote WORD if it is a symbolic expression
             (not (skk-lisp-prog-p word)) )
        (concat "(concat \""
                (mapconcat (function (lambda (c)
                                       (cond ((eq c ?/) ; ?/
                                              "\\057" )
                                             ((eq c ?\n) ; ?\n
                                              "\\n" )
                                             ((eq c ?\r) ; ?\r
                                              "\\r" )
                                             ((eq c ?\") ; ?\"
                                              "\\\"" )
                                             ((eq c ?\\) ; ?\\
                                              "\\\\" )
                                             (t (char-to-string c)))))
                           ;; 文字列を対応する char のリストに分解する。
                           (append word nil) "")
                "\")")
      word )))

(defun skk-lisp-prog-p (word)
  ;; word が Lisp プログラムであれば、t を返す。
  (let ((l (length word)))
    (and (> l 2)
         (eq (aref word 0) ?\() ; ?\(
         (< (aref word 1) 128)
         (eq (aref word (1- l)) ?\)) ))) ; ?\)

(defun skk-public-jisyo-contains-p (okurigana word)
  ;; 共有辞書が MIDASHI 及びそれに対応する WORDS エントリを持っていれば、
  ;; non-nil を返す。プライベート辞書のバッファでコールされる。
  (let (fn skk-henkan-okuri-strictly skk-henkan-strict-okuri-precedence)
    (if okurigana
        (setq skk-henkan-okurigana okurigana) )
    (if (and (not (featurep 'skk-server))
             (or (and (boundp 'skk-server-host) skk-server-host)
                 (and (boundp 'skk-servers-list) skk-servers-list)
                 (getenv "SKKSERVER")
                 (getenv "SKKSERV") ))
        (require 'skk-server) )
    (if (and (featurep 'skk-server)
             (or (and skk-server-host skk-server-prog)
                 skk-servers-list ))
        (setq fn (assq 'skk-search-server skk-search-prog-list)) )
    ;; skk-search-server から始まるリストがなければ、とにかく大きい辞書を引数
    ;; にしている skk-search-jisyo-file プログラムを探す。
    (if (and (not fn) (or skk-aux-large-jisyo skk-large-jisyo))
        (let ((spl skk-search-prog-list)
              cell )
          (while (setq cell (car spl))
            (if (and (eq (car cell) 'skk-search-jisyo-file)
                     (member (nth 1 cell)
                             '(skk-aux-large-jisyo skk-large-jisyo) ))
                (setq fn cell
                      spl nil )
              (setq spl (cdr spl)) ))))
    (and fn (member word (eval fn))) ))

(defun skk-compose-ignore-entry (entry &optional add)
  ;; ENTRY の中に skk-ignore-dic-word 関数でクォートしたエントリがあれ
  ;; ば、一つのエントリにまとめる。
  ;; オプショナル引数の ADD が指定されていたら、ADD を含めた
  ;; skk-ignore-dic-word エントリを作る。
  ;; 新しい skk-ignore-dic-word エントリを car に、それ以外のエントリ cdr にし
  ;; たリストを返す。
  (let (l arg e)
    (if add (setq entry (delete add entry)))
    (setq l entry)
    (save-match-data
      (while l
        (setq e (car l)
              l (cdr l) )
        (if (string-match "(skk-ignore-dic-word +\\([^\)]+\\))" e)
            (setq arg (concat arg
                              (substring e (1+ (match-beginning 1))
                                         (1- (match-end 1)) )
                              "\" \"" )
                  entry (delq e entry) )))
      (if add
          (setq arg (if arg (concat arg add) add))
        ;; 末尾の " \"" を切り落とす。
        (setq arg (substring arg 0 -2)) )
      (cons (concat "(skk-ignore-dic-word \"" arg "\")") entry) )))

(defun skk-katakana-region (start end &optional vcontract)
  "リージョンのひらがなをカタカナに変換する。
オプショナル引数の VCONTRACT が non-nil であれば、\"う゛\" を \"ヴ\" に変換す
る。
引数の START と END は数字でもマーカーでも良い。"
  (interactive "*r\nP")
  (let ((diff (if skk-mule (- ?ア ?あ)))
        ch )
    (skk-save-point
      (save-match-data
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-off))
        (goto-char start)
        (while (re-search-forward  "[ぁ-ん]" end 'noerror)
          (setq ch (preceding-char))
          ;; firstly insert a new char, secondly delete an old char to save
          ;; the cursor position.
          (if skk-mule
              (progn
                (backward-char 1)
                (cancel-undo-boundary)
                (insert (+ ch diff)) )
            (backward-char 2)
            (cancel-undo-boundary)
            (insert ?\245 ch) )
          (cancel-undo-boundary)
          (delete-region (+ (match-beginning 0) skk-kanji-len)
                         (+ (match-end 0) skk-kanji-len) ))
        (if vcontract
            (progn
              (goto-char start)
              (while (re-search-forward  "ウ゛" end 'noerror)
                (if skk-mule
                    (backward-char 2)
                  (backward-char 3) )
                (cancel-undo-boundary)
                (insert "ヴ")
                (cancel-undo-boundary)
                (delete-region (+ (match-beginning 0) skk-kanji-len)
                               (+ (match-end 0) skk-kanji-len) ))))
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-on))
        (skk-set-cursor-properly)
        ))))

(defun skk-hiragana-region (start end &optional vexpand)
  "リージョンのカタカナをひらがなに変換する。
オプショナル引数の VEXPAND が non-nil であれば、\"ヴ\" を \"う゛\" に変換する。
引数の START と END は数字でもマーカーでも良い。
\"ヵ\" と \"ヶ\" は変更されない。この 2 つの文字は対応するひらがながないので、カ
タカナとしては扱われない。"
  (interactive "*r\nP")
  (let ((diff (if skk-mule (- ?ア ?あ)))
        ch )
    (skk-save-point
      (save-match-data
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-off))
        (goto-char start)
        (while (re-search-forward  "[ァ-ン]" end 'noerror)
          (setq ch (preceding-char))
          ;; firstly insert a new char, secondly delete an old char to save
          ;; the cursor position.
          (if skk-mule
              (progn
                (backward-char 1)
                (cancel-undo-boundary)
                (insert (- ch diff)) )
            (backward-char 2)
            (cancel-undo-boundary)
            (insert ?\244 ch) )
          (cancel-undo-boundary)
          (delete-region (+ (match-beginning 0) skk-kanji-len)
                         (+ (match-end 0) skk-kanji-len) ))
        (if vexpand
            (progn
              (goto-char start)
              (while (re-search-forward "ヴ" end 'noerror)
                (backward-char 1)
                (cancel-undo-boundary)
                (insert "う゛")
                (cancel-undo-boundary)
                (delete-region (+ (match-beginning 0) (* skk-kanji-len 2))
                               (+ (match-end 0) (* skk-kanji-len 2)) ))))
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-on))
        (skk-set-cursor-properly)
        ))))

(defun skk-zenkaku-region (start end)
  "リージョンの ascii 文字を対応する全角文字に変換する。"
  (interactive "*r")
  (skk-save-point
    (save-match-data
      ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-off))
      (goto-char end)
      (while (re-search-backward "[ -~]" start 'noerror)
        (cancel-undo-boundary)
        ;; firstly insert a new char, secondly delete an old char to save
        ;; the cursor position.
        (insert (aref skk-default-zenkaku-vector (following-char)))
        (cancel-undo-boundary)
        (delete-region (+ (match-beginning 0) skk-kanji-len)
                       (+ (match-end 0) skk-kanji-len) ))
      ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-on))
      (skk-set-cursor-properly)
      )))

(defun skk-ascii-region (start end)
  ;; リージョンの全角英数字を対応する ascii 文字に変換する。
  ;; egg.el 3.09 の hankaku-region を参考にした。
  (interactive "*r")
  (skk-save-point
    (save-match-data
      (let (val)
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-off))
        (goto-char end)
        (while (re-search-backward "\\cS\\|\\cA" start 'noerror)
          (setq val (skk-jisx0208-to-ascii (char-to-string (following-char))))
          (if val
              (progn
                (insert val)
                (delete-region (+ (match-beginning 0) 1)
                               (+ (match-end 0) 1) ))))
        ;;(if (and skk-henkan-active skk-use-face) (skk-henkan-face-on))
        (skk-set-cursor-properly)
        ))))

(defun skk-katakana-henkan (arg)
  "▽モードであれば、リージョンのひらがなをカタカナに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (if skk-henkan-on
      (if (not skk-henkan-active)
          (skk-*-henkan-1 'skk-katakana-region skk-henkan-start-point
                          skk-henkan-end-point 'vcontract ))
    (skk-emulate-original-map arg) ))

(defun skk-hiragana-henkan (arg)
  "▽モードであれば、リージョンのカタカナをひらがなに変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (if skk-henkan-on
      (if (not skk-henkan-active)
          (skk-*-henkan-1 'skk-hiragana-region skk-henkan-start-point
                          skk-henkan-end-point 'vexpand ))
    (skk-emulate-original-map arg) ))

(defun skk-zenkaku-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (if skk-henkan-on
      (if (not skk-henkan-active)
          (skk-*-henkan-1 'skk-zenkaku-region skk-henkan-start-point
                          skk-henkan-end-point ))
    (skk-emulate-original-map arg) ))

(defun skk-ascii-henkan (arg)
  "▽モードであれば、ascii 文字を対応する全角文字に変換する。
▼モードでは何もしない。
その他のモードでは、オリジナルのキー割り付けでバインドされているコマンドを実行
する。"
  (interactive "*P")
  (if skk-henkan-on
      (if (not skk-henkan-active)
          (skk-*-henkan-1 'skk-ascii-region skk-henkan-start-point
                          skk-henkan-end-point ))
    (skk-emulate-original-map arg) ))

(defun skk-*-henkan-1 (func &rest args)
  ;; 変換可能かどうかのチェックをした後に ARGS を引数として FUNC を適用し、
  ;; skk-henkan-start-point と skk-henkan-end-point の間の文字列を変換する。
  (let ((pos (point)))
    (cond ((not (string= skk-prefix ""))
           (skk-error "フィックスされていない skk-prefix があります"
                      "Have unfixed skk-prefix" ))
          ((< pos skk-henkan-start-point)
           (skk-error "カーソルが変換開始地点より前にあります"
                      "Henkan end point must be after henkan start point" ))
          ((and (not skk-allow-spaces-newlines-and-tabs)
                (skk-save-point (beginning-of-line)
                                (> (point) skk-henkan-start-point) ))
           (skk-error "変換キーに改行が含まれています"
                      "Henkan key may not contain a new line character" )))
    (skk-set-marker skk-henkan-end-point pos)
    (apply func args)
    (skk-kakutei) ))

;;;###skk-autoload
(defun skk-jisx0208-to-ascii (string)
  (let ((char
         (cond (skk-mule3
                (require 'language/japan-util)
                (get-char-code-property (string-to-char string) 'ascii) )
               (skk-mule
                (let* ((ch (string-to-char string))
                       (ch1 (char-component ch 1)) )
                  (cond ((eq 161 ch1)   ; ?\241
                         (cdr (assq (char-component ch 2) skk-hankaku-alist)) )
                        ((eq 163 ch1)   ; ?\243
                         (- (char-component ch 2) 128) ; ?\200
                         ))))
               (t (- (aref string (1- skk-kanji-len)) 128)) )))
    (if char (char-to-string char)) ))
                             
;;;###skk-autoload
(defun skk-middle-list (org offset inserted)
  ;; ORG := '(A B C), INSERTED := '(X Y), OFFSET := 1
  ;; -> '(A B X Y C)
  (let (tmp tail)
    (if (>= 0 offset)
        (error "Cannot insert!") )
    (setq tmp (nthcdr (1- offset) org)
          tail (cdr tmp) )
    (setcdr tmp nil) ;cut off
    (setcdr tmp (if tail (nconc inserted tail) inserted))
    org ))

;;(defun skk-chomp (nth list)
;;  (let ((l (nthcdr (1- nth) list)))
;;    (setcdr l nil)
;;    list ))

;;(defun skk-cutoff-list (list offset)
;;  ;; LIST := '(A B C), OFFSET := 1
;;  ;; -> '(A B)
;;  (if (< 0 offset)
;;      (setcdr (nthcdr (1- offset) list) nil) )
;;  list )
  
(defun skk-henkan-face-on ()
  ;; skk-use-face が non-nil の場合、skk-henkan-start-point と
  ;; skk-henkan-end-point の間の face 属性を skk-henkan-face の値に変更する。
  ;;
  ;; SKK 9.4 より Text Properties を使用するのを止めて、Overlays を使用するよ
  ;; うにした (egg.el, canna.el, wnn-egg.el を参考にした)。
  ;; Overlays は、テキストの一部ではないので、バッファから文字を切り出してもコ
  ;; ピーの対象にならないし、アンドゥ時も無視されるので、変換された候補の表示
  ;; を一時的に変更するには Text Properties よりも好都合である。
  ;;
  ;; 但し、Overlays は Text Properties より扱いに注意すべき点がある。
  ;; オーバレイの中のマーカを直接変更してしまうと、ほかの重要なデータ構造の更
  ;; 新が行われず、失われるオーバレイが出ることになりかねない (Overlay の
  ;; Bufferの範囲を変更するときは、必ず move-overlay を使用しなければならない)
  ;; という点である。skk-henkan-face-on で変換結果の候補に関する
  ;; skk-henkan-overlay を新規に作ってから (あるいは既存の Overlay を該当個所
  ;; に移動してから) skk-henkan-face-off で消去するまでの間に
  ;; skk-henkan-start-point と skk-henkan-end-point 中のテキストを削除すると、
  ;; 結果的に move-overlay を使用せずそれらのマーカー値を更新することになって
  ;; しまう。従い、skk-henkan-start-point と skk-henkan-end-point の間にあるテ
  ;; キストに変更を加えるときは、先に skk-henkan-face-off で一旦
  ;; skk-henkan-overlay を消す必要がある (<(skk-e19.el/kill-region)>)。
  ;;
  ;;From: enami tsugutomo <enami@ba2.so-net.or.jp>
  ;;Subject: overlay (was Re: SKK-9.4.15)
  ;;Date: 23 Oct 1996 16:35:53 +0900
  ;;Message-ID: <87n2xe5e06.fsf@plants-doll.enami.ba2.so-net.or.jp>
  ;;
  ;;enami> 関数 skk-henkan-face-on の comment の
  ;;enami> 
  ;;enami>   ;; 但し、Overlays は Text Properties より扱いに注意すべき点がある。
  ;;enami>   ;; オーバレイの中のマーカを直接変更してしまうと、ほかの重要なデータ構造の更
  ;;enami>   ;; 新が行われず、失われるオーバレイが出ることになりかねない
  ;;enami> 
  ;;enami>  (及びそれ以降) の部分に関してですが, make-overlay で位置の指定に使っ
  ;;enami> た marker がそのまま overlay の一部として使われると, 誤解していません
  ;;enami> でしょうか?
  ;;enami> 
  ;;enami> 実際には overlay の一部を為す marker を直接移動させることはできません
  ;;enami> し, text の削除追加による移動には対処していますから問題ないと思います.
  ;;enami> 
  ;;enami> # そうでなかったら overlay って無茶苦茶使いづらいものになってしまいま
  ;;enami> # す.
  ;;enami> えなみ
  (let ((inhibit-quit t)
        cbuf )
    (if (and skk-henkan-face
             (setq cbuf (current-buffer))
             (eq (marker-buffer skk-henkan-start-point) cbuf)
             (eq (marker-buffer skk-henkan-end-point) cbuf)
             (marker-position skk-henkan-start-point)
             (marker-position skk-henkan-end-point) )
        (progn
          (or skk-henkan-overlay
              (setq skk-henkan-overlay (skk-make-overlay skk-henkan-start-point
							 skk-henkan-end-point
							 cbuf )))
          (skk-move-overlay skk-henkan-overlay skk-henkan-start-point
			    skk-henkan-end-point cbuf )
          ;; evaporate 属性を付けるべきか...。でも変換を繰り返すときは、再利用
          ;; するのだから、むしろ、既に作ってある方が良いかも。
          (skk-overlay-put skk-henkan-overlay 'face skk-henkan-face) ))))

(defun skk-henkan-face-off ()
  ;; skk-henkan-start-point と skk-henkan-end-point の間の表示を変更している
  ;; skk-henkan-overlay を消す。
  (and skk-henkan-face
       ;; リカーシブミニバッファに入ったときは、overlayp による検査が必要？
       (skk-overlayp skk-henkan-overlay)
       (skk-delete-overlay skk-henkan-overlay) ))

(defun skk-set-cursor-color (color)
  ;; カーソルの色を COLOR に変更する。
  (if skk-use-color-cursor
      (condition-case nil
	  (set-cursor-color color)
	(error
	 (set-cursor-color skk-default-cursor-color)
	 (if skk-report-set-cursor-error
	     (skk-message
              "カラーマップ切れです。ディフォルトのカラーを使います。"
              "Color map is exhausting, use default cursor color" ))))))

;;;###skk-autoload
(defun skk-set-cursor-properly ()
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  (if skk-use-color-cursor
      (if (not skk-mode)
	  (skk-set-cursor-color skk-default-cursor-color)
	(skk-set-cursor-color (cond (skk-zenkaku-mode skk-zenkaku-cursor-color)
				    (skk-katakana skk-katakana-cursor-color)
				    (skk-j-mode skk-hirakana-cursor-color)
				    (t skk-ascii-cursor-color) ))))
  (if skk-use-cursor-change
      (skk-change-cursor-when-ovwrt) ))

;;;###skk-autoload
(defun skk-change-cursor-when-ovwrt ()
  (if skk-xemacs
      (setq bar-cursor overwrite-mode)
    (if overwrite-mode
        (modify-frame-parameters (selected-frame) '((cursor-type bar . 3)))
      (modify-frame-parameters (selected-frame) '((cursor-type . box))) )))

;;;###skk-autoload
(defun skk-make-face (face)
  ;; hilit-lookup-face-create のサブセット。tutorial で色付けを行なう場合でも
  ;; hilit19 に依存せずとりあえず face を自前で作ることができるように、という
  ;; 目的で作ったもので、簡単な色付けしなできない。あまり賢くはない。複雑な
  ;; face を作りたい人は hilit-lookup-face-create 等を使って下さい。
  (or (car (memq face (face-list)))
      (let ((face-name (symbol-name face))
            fore back )
        (setq face (make-face face))
        (save-match-data
          (if (not (string-match "/" face-name))
              (set-face-foreground face face-name)
            (set-face-foreground
             face
             (substring face-name 0 (match-beginning 0)) )
            (set-face-background
             face
             (substring face-name (1+ (match-beginning 0))) ))
          face ))))
                        
;;(defun skk-reinvoke ()
;;  (let ((original-status
;;         (cond (skk-katakana 'katakana)
;;               (skk-zenkaku-mode 'zenkaku)
;;               (skk-j-mode 'hirakana)
;;               (skk-mode 'ascii)
;;               (t 'unkown) )))
;;    (skk-mode 1)
;;    (cond ((eq original-status 'katakana)
;;           (setq skk-katakana t) )
;;          ((eq original-status 'zenkaku)
;;           (setq skk-zenkaku-mode t) )
;;          ((eq original-status 'ascii)
;;           (setq skk-j-mode nil) )
;;          ((eq original-status 'hirakana)) )
;;    (skk-kakutei) ))

(add-hook 'edit-picture-hook 'skk-misc-for-picture 'append)
(add-hook 'skk-before-kill-emacs-hook
          (function (lambda ()
                      (if skk-menu-modified-user-option
                          (skk-menu-save-modified-user-option) ))))
(add-hook 'after-make-frame-hook 'skk-set-cursor-properly)
(add-hook 'minibuffer-setup-hook
          (function (lambda () (skk-set-cursor-properly))) )

(add-hook 'minibuffer-exit-hook
          (function
           (lambda ()
             (remove-hook 'minibuffer-setup-hook 'skk-setup-minibuffer)
             (skk-set-cursor-properly) )))

(run-hooks 'skk-load-hook)

(provide 'skk)
;;; skk.el ends here
