(require 'skk-foreword)

;;;### (autoloads (skk-make-face skk-change-cursor-when-ovwrt skk-set-cursor-properly skk-middle-list skk-jisx0208-to-ascii skk-update-jisyo skk-nunion skk-search skk-save-jisyo skk-start-henkan skk-kakutei skk-previous-candidate skk-get-current-candidate skk-emulate-original-map skk-auto-fill-mode skk-mode skk-set-marker skk-y-or-n-p skk-yes-or-no-p skk-error skk-message skk-save-point skk-version) "skk" "skk/skk.el")

(defconst skk-month-alist '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4") ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8") ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")) "\
英語の月名と算用数字の連想リスト。

算用数字から英語の月名のみを出力するのであれば、ベクターを使った方が高速だが、
英語の月名から算用数字を出力するのであれば連想リストでなければ無理なので、多
目的に使用できるよう連想リストの形態を取る。

Alist of English month abbreviations and numerical values.

Although it is faster to use a vector if we only want to output
month abbreviations given the ordinal, without the alist it's
unreasonable [sic] to output the ordinal given the abbreviation,
so for multi-purpose utility we use the alist form.")

(autoload 'skk-version "skk" nil t nil)

(defvar skk-init-file (if (eq system-type 'ms-dos) "~/_skk" "~/.skk") "\
*SKK の初期設定ファイル名。
skk.el 9.x より ~/.emacs でのカスタマイズが可能となった。

Name of the SKK initialization file.
From skk.el 9.x on all customization may be done in ~/.emacs.")

(defvar skk-special-midashi-char-list '(?\> ?\< ?\?) "\
*接頭辞、接尾辞の入力のためのプレフィックスキー、サフィックスキーのリスト。

List of prefix and suffix keys for entering `setsutoji' and `setsuoji'.")

(defvar skk-mode-hook nil "\
*SKK を起動したときのフック。
他に、skk-auto-fill-mode-hook、skk-load-hook, skk-init-file でもカスタ
マイズが可能。

Hook run at SKK startup.

`skk-auto-fill-mode-hook', `skk-load-hook', and skk-init-file may also
be used for customization.")

(defvar skk-auto-fill-mode-hook nil "\
*skk-auto-fill-mode を起動したときのフック。
他に、skk-mode-hook, skk-load-hook, skk-init-file でもカスタマイズが可
能。

Hook run at startup of skk-auto-fill-mode.

`skk-mode-hook', `skk-load-hook', and `skk-init-file' may also be
used for customization.")

(defvar skk-load-hook nil "\
*skk.el をロードしたときのフック。
他に、skk-mode-hook, skk-auto-fill-mode-hook, skk-init-file でもカスタ
マイズが可能。

Hook run when SKK is loaded.

`skk-auto-fill-mode-hook', `skk-mode-hook', and `skk-init-file' may
also be used for customization.")

(defvar skk-kakutei-jisyo nil "\
*最初に検索する辞書。
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
searched and the order of search can be changed.")

(defvar skk-initial-search-jisyo nil "\
*ユーザー辞書の検索の前に検索する辞書。
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
searched and the order of search can be changed.")

(defvar skk-large-jisyo nil "\
*ユーザー辞書の検索の後に検索する辞書。
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
searched and the order of search can be changed.")

(defvar skk-aux-large-jisyo nil "\
*SKK サーバーで最後に検索する辞書。
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
autoloaded.")

(defvar skk-search-prog-list '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t) (skk-search-jisyo-file skk-initial-search-jisyo 10000 t) (skk-search-jisyo-file skk-jisyo 0 t) (skk-search-jisyo-file skk-large-jisyo 10000)) "\
*検索関数、検索対象の辞書を決定するためのリスト。
変換した候補を返す S 式をリストの形に表記したもの。
skk-search 関数が skk-search-prog-list の car から後方向へ順番に S 式の評価を
行い変換を行なう。

This list determines the search functions used and the dictionaries
searched.
A list of S-expressions returning conversion candidates.
The function `skk-search' performs conversions by evaluating each S-
expression in order, starting with the car of `skk-search-prog-list'.")

(defvar skk-jisyo (if (eq system-type 'ms-dos) "~/_skk-jisyo" "~/.skk-jisyo") "\
*SKK のユーザー辞書。

SKK's dictionary of user-specified conversions.")

(defvar skk-backup-jisyo (if (eq system-type 'ms-dos) "~/_skk-jisyo.BAK" "~/.skk-jisyo.BAK") "\
*SKK のユーザー辞書のバックアップファイル。

Name of user dictionary backup (a file name as a string).")

(defvar skk-jisyo-code nil "\
*Non-nil であれば、その値で辞書バッファの漢字コードを設定する。
Mule では、*euc-japan*, *sjis*, *junet*。
また、\"euc\", \"ujis\", \"sjis\", \"jis\" などの文字列によっても指定が
可能。

If non-nil, the value sets the kanji code used in dictionary buffers.
In Mule, the symbols *euc-japan*, *sjis*, or *junet*.  Can also be
specified as a string such as \"euc\", \"ujis\", \"sjis\", or \"jis\".")

(defvar skk-keep-record t "\
*Non-nil であれば、変換に関する記録を skk-record-file に取る。

If non-nil, a record of conversions is kept in `skk-record-file'.")

(defvar skk-record-file (if (eq system-type 'ms-dos) "~/_skk-record" "~/.skk-record") "\
*ユーザー辞書の統計を取るファイル。
辞書セーブの時刻、単語の登録数、確定を行った回数、確定率、全体の語数の
情報を収める。

File containing statistics about the user dictionary.

At the time the dictionary is saved, the number of words registered,
number of conversions accepted, rate of acceptance, and the total
number of words are collected.")

(defvar skk-kakutei-key "\n" "\
*確定動作 (\"skk-kakutei\") を行うキー。

The key that executes conversion confirmation (\"skk-kakutei\").")

(defvar skk-use-vip nil "\
*Non-nil であれば、VIP に対応する。

If non-nil, VIP compatibility mode.")

(defvar skk-use-viper nil "\
*Non-nil であれば、VIPER に対応する。。

If non-nil, VIPER compatibility mode.")

(defvar skk-henkan-okuri-strictly nil "\
*Non-nil であれば、見出し語と送り仮名が一致したときだけ候補として出力する。
例えば、下記のような辞書エントリが、skk-jisyo (プライベート辞書) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、\"多く\" のみを出力し、\"大く\" を出力しない。

SKK-JISYO.[SML] の送り仮名エントリは上記の形式になっていないので、skk-jisyo の
送りありの辞書エントリがこの形式のものをあまり含んでいない場合は、このオプショ
ンを on にすることで、すぐに単語登録に入ってしまうので注意すること。

skk-process-okuri-early の値が nil ならば上記の形式で skk-jisyo が作られる。

Emacs 19 ベースの Mule ならば、下記のフォームを評価することで、単語登録に入っ
たときだけ一時的にこのオプションを nil にすることができる。

    (add-hook 'minibuffer-setup-hook
              (function
               (lambda ()
                 (if (and (boundp 'skk-henkan-okuri-strictly)
                          skk-henkan-okuri-strictly
                          (not (eq last-command 'skk-purge-from-jisyo)) )
                     (progn
                       (setq skk-henkan-okuri-strictly nil)
                       (put 'skk-henkan-okuri-strictly 'temporary-nil t) )))))

    (add-hook 'minibuffer-exit-hook
              (function
               (lambda ()
                 (if (get 'skk-henkan-okuri-strictly 'temporary-nil)
                     (progn
                       (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                       (setq skk-henkan-okuri-strictly t) )))))

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される)。

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

    (add-hook 'minibuffer-setup-hook
              (function
               (lambda ()
                 (if (and (boundp 'skk-henkan-okuri-strictly)
                          skk-henkan-okuri-strictly
                          (not (eq last-command 'skk-purge-from-jisyo)) )
                     (progn
                       (setq skk-henkan-okuri-strictly nil)
                       (put 'skk-henkan-okuri-strictly 'temporary-nil t) )))))

    (add-hook 'minibuffer-exit-hook
              (function
               (lambda ()
                 (if (get 'skk-henkan-okuri-strictly 'temporary-nil)
                     (progn
                       (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                       (setq skk-henkan-okuri-strictly t) )))))

When using this option, `skk-process-okuri-early' must be `nil'.
\(When using customize from the menubar this will automatically
temporarily be set to `nil'.)")

(defvar skk-henkan-strict-okuri-precedence nil "\
*Non-nil であれば、見出し語と送り仮名が一致した候補を優先して表示する。
例えば、下記のような辞書エントリが、skk-jisyo (プライベート辞書) にあった場合に

  \"おおk /大/多/[く/多/]/[き/大/]/\"

\"▽おお*く\" を変換したとき、まず\"多く\" を出力し、
次に \"大く\" を出力する。

\"大く\"などの候補はうっとうしいが、すぐに単語登録にはいってしまうのも
嫌なひとにおすすめ。

このオプション利用時は、skk-process-okuri-early の値は nil でならない。
また skk-henkan-okuri-strictly が non-nil のときは、この変数は無視される。
\(メニューバー を利用してカスタマイズした場合は自動的に調整される)。")

(defvar skk-auto-okuri-process nil "\
*Non-nil であれば、送り仮名部分を自動認識して変換を行う。
例えば、

    \"Uresii (\"UreSii\" ではなく) -> 嬉しい\"

のように変換される。但し、skk-jisyo 辞書 (プライベート辞書) が、

    \"うれs /嬉/[し/嬉/]/\"

のような形式になっていることが必要である (SKK-JISYO.[SML] はこの形式に対応し
ていないので、skk-jisyo にこのエントリがなければならない)。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される)。")

(defvar skk-process-okuri-early nil "\
*Non-nil であれば、送り仮名のローマ字プレフィックスの入力時点で変換を開始する。
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
セットされる。")

(defvar skk-egg-like-newline nil "\
*Non-nil であれば、▼モードで改行をタイプしても確定するのみで改行しない。")

(defvar skk-kakutei-early t "\
*Non-nil であれば skk-kana-input が呼ばれたときに現在の候補を確定する。
例えば、

    \"▽かくてい -> ▼確定 -> 確定s -> 確定す\"

のように変換後、「す」の prefix である \"s\" を入力した時点で確定する。
nil であれば、例えば

    \"▽かくてい -> ▼確定 -> ▼確定s -> ▼確定する -> 確定する。\"

のように skk-kakutei を直接、間接にコールするまで (句読点を入力したり、新たな
▽モードに入ったりすると間接的に skk-kakutei をコールする) は、確定しないので、
その間は、変換候補を選びなおすことなどが可能。

このオプション利用時は、skk-process-okuri-early の値は nil でなければならない
\(メニューバー を利用してカスタマイズした場合は自動的に調整される)。")

(defvar skk-delete-implies-kakutei t "\
*Non-nil であれば、▼モードで BS を押すと、前の一文字を削除し確定する。
nil であれば、一つ前の候補を表示する。")

(defvar skk-allow-spaces-newlines-and-tabs t "\
*Non-nil であれば、skk-henkan-key にスペース、タブ、改行があっても変換可能。
例えば、下記のように skk-henkan-key の中に改行が入っていても変換が可能である。

     \"▽か
  な\"
   -> \"仮名\"

この値が nil であれば、最初のスペースで skk-henkan-key を切り詰めてしまい、
以降のスペース、タブ、改行は無視される。
この値は、skk-start-henkan, skk-ascii-henkan, skk-katakana-henkan,
skk-hiragana-henkan, skk-zenkaku-henkan 及び skk-backward-and-set-henkan-point
の動作に影響する。")

(defvar skk-convert-okurigana-into-katakana nil "\
*Non-nil であれば、カタカナモードで変換したときに送り仮名もカタカナに変換する。")

(defvar skk-delete-okuri-when-quit nil "\
*Non-nil であれば、送りありの変換中に \"C-g\" を押すと送り仮名を消し▽モードに入る。
例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" ->▽な\"

nil であれば、送り仮名を含めた見出し語をそのまま残し、■モードに入る。例えば、

    \"▽な*く -> ▼泣く -> \"C-g\" -> なく\"")

(defvar skk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l) "\
*メニュー形式で候補を選択するときの選択キーのリスト。
\"x\", \" \" 及び \"C-g\" 以外の 7 つのキー (char type) を含む必要があ
る。\"x\", \" \" 及び \"C-g\" は候補選択時にそれぞれ特別な仕事に割り当
てられているので、このリストの中には含めないこと。")

(defvar skk-ascii-mode-string " SKK" "\
*SKK が ascii モードであるときにモードラインに表示される文字列。")

(defvar skk-hirakana-mode-string " かな" "\
*ひらがなモードであるときにモードラインに表示される文字列。")

(defvar skk-katakana-mode-string " カナ" "\
*カタカナモードであるときにモードラインに表示される文字列。")

(defvar skk-zenkaku-mode-string " 全英" "\
*全英モードであるときにモードラインに表示される文字列。")

(defvar skk-abbrev-mode-string " aあ" "\
*SKK abbrev モードであるときにモードラインに表示される文字列。")

(defvar skk-echo t "\
*Non-nil であれば、仮名文字のプレフィックスを表示する。")

(defvar skk-use-numeric-conversion t "\
*Non-nil であれば、数値変換を行う。")

(defvar skk-char-type-vector [0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 0 4 4 4 4 0 4 4 4 4 4 4 0 4 4 0 0 0 0 0 0 3 1 1 1 3 1 1 1 3 1 1 0 1 2 3 1 0 1 1 1 3 1 1 2 1 1 0 0 0 0 5] "\
*skk-kana-input で参照するかな文字変換のための char type ベクター。
各要素の数字の意味は下記の通り。

0 ローマ文字よりかな文字への変換を中止する (現在のところ使用していない)。
1 促音の一部分となり得る子音。
2 上記 1 以外の子音 (n, x)
3 母音
4 skk-mode で、skk-set-henkan-point に割り付けられている文字。
5 プレフィックスを消去する")

(defvar skk-standard-rom-kana-rule-list '(("b" "b" nil) ("by" "by" nil) ("c" "c" nil) ("ch" "ch" nil) ("cy" "cy" nil) ("d" "d" nil) ("dh" "dh" nil) ("dy" "dy" nil) ("f" "f" nil) ("fy" "fy" nil) ("g" "g" nil) ("gy" "gy" nil) ("h" "h" nil) ("hy" "hy" nil) ("j" "j" nil) ("jy" "jy" nil) ("k" "k" nil) ("ky" "ky" nil) ("m" "m" nil) ("my" "my" nil) ("n" "n" nil) ("ny" "ny" nil) ("p" "p" nil) ("py" "py" nil) ("r" "r" nil) ("ry" "ry" nil) ("s" "s" nil) ("sh" "sh" nil) ("sy" "sy" nil) ("t" "t" nil) ("th" "th" nil) ("ts" "ts" nil) ("ty" "ty" nil) ("v" "v" nil) ("w" "w" nil) ("x" "x" nil) ("xk" "xk" nil) ("xt" "xt" nil) ("xw" "xw" nil) ("xy" "xy" nil) ("y" "y" nil) ("z" "z" nil) ("zy" "zy" nil) ("bb" "b" ("ッ" . "っ")) ("cc" "c" ("ッ" . "っ")) ("dd" "d" ("ッ" . "っ")) ("ff" "f" ("ッ" . "っ")) ("gg" "g" ("ッ" . "っ")) ("hh" "h" ("ッ" . "っ")) ("jj" "j" ("ッ" . "っ")) ("kk" "k" ("ッ" . "っ")) ("mm" "m" ("ッ" . "っ")) ("pp" "p" ("ッ" . "っ")) ("rr" "r" ("ッ" . "っ")) ("ss" "s" ("ッ" . "っ")) ("tt" "t" ("ッ" . "っ")) ("vv" "v" ("ッ" . "っ")) ("ww" "w" ("ッ" . "っ")) ("xx" "x" ("ッ" . "っ")) ("yy" "y" ("ッ" . "っ")) ("zz" "z" ("ッ" . "っ")) ("a" nil ("ア" . "あ")) ("ba" nil ("バ" . "ば")) ("bya" nil ("ビャ" . "びゃ")) ("cha" nil ("チャ" . "ちゃ")) ("cya" nil ("チャ" . "ちゃ")) ("da" nil ("ダ" . "だ")) ("dha" nil ("デャ" . "でゃ")) ("dya" nil ("ヂャ" . "ぢゃ")) ("fa" nil ("ファ" . "ふぁ")) ("fya" nil ("フャ" . "ふゃ")) ("ga" nil ("ガ" . "が")) ("gya" nil ("ギャ" . "ぎゃ")) ("ha" nil ("ハ" . "は")) ("hya" nil ("ヒャ" . "ひゃ")) ("ja" nil ("ジャ" . "じゃ")) ("jya" nil ("ジャ" . "じゃ")) ("ka" nil ("カ" . "か")) ("kya" nil ("キャ" . "きゃ")) ("ma" nil ("マ" . "ま")) ("mya" nil ("ミャ" . "みゃ")) ("na" nil ("ナ" . "な")) ("nya" nil ("ニャ" . "にゃ")) ("pa" nil ("パ" . "ぱ")) ("pya" nil ("ピャ" . "ぴゃ")) ("ra" nil ("ラ" . "ら")) ("rya" nil ("リャ" . "りゃ")) ("sa" nil ("サ" . "さ")) ("sha" nil ("シャ" . "しゃ")) ("sya" nil ("シャ" . "しゃ")) ("ta" nil ("タ" . "た")) ("tha" nil ("テァ" . "てぁ")) ("tya" nil ("チャ" . "ちゃ")) ("va" nil ("ヴァ" . "う゛ぁ")) ("wa" nil ("ワ" . "わ")) ("xa" nil ("ァ" . "ぁ")) ("xka" nil ("ヵ" . "か")) ("xwa" nil ("ヮ" . "ゎ")) ("xya" nil ("ャ" . "ゃ")) ("ya" nil ("ヤ" . "や")) ("za" nil ("ザ" . "ざ")) ("zya" nil ("ジャ" . "じゃ")) ("i" nil ("イ" . "い")) ("bi" nil ("ビ" . "び")) ("byi" nil ("ビィ" . "びぃ")) ("chi" nil ("チ" . "ち")) ("cyi" nil ("チィ" . "ちぃ")) ("di" nil ("ヂ" . "ぢ")) ("dhi" nil ("ディ" . "でぃ")) ("dyi" nil ("ヂィ" . "ぢぃ")) ("fi" nil ("フィ" . "ふぃ")) ("fyi" nil ("フィ" . "ふぃ")) ("gi" nil ("ギ" . "ぎ")) ("gyi" nil ("ギィ" . "ぎぃ")) ("hi" nil ("ヒ" . "ひ")) ("hyi" nil ("ヒィ" . "ひぃ")) ("ji" nil ("ジ" . "じ")) ("jyi" nil ("ジィ" . "じぃ")) ("ki" nil ("キ" . "き")) ("kyi" nil ("キィ" . "きぃ")) ("mi" nil ("ミ" . "み")) ("myi" nil ("ミィ" . "みぃ")) ("ni" nil ("ニ" . "に")) ("nyi" nil ("ニィ" . "にぃ")) ("pi" nil ("ピ" . "ぴ")) ("pyi" nil ("ピィ" . "ぴぃ")) ("ri" nil ("リ" . "り")) ("ryi" nil ("リィ" . "りぃ")) ("si" nil ("シ" . "し")) ("shi" nil ("シ" . "し")) ("syi" nil ("シィ" . "しぃ")) ("ti" nil ("チ" . "ち")) ("thi" nil ("ティ" . "てぃ")) ("tyi" nil ("チィ" . "ちぃ")) ("vi" nil ("ヴィ" . "う゛ぃ")) ("wi" nil ("ウィ" . "うぃ")) ("xi" nil ("ィ" . "ぃ")) ("xwi" nil ("ヰ" . "ゐ")) ("zi" nil ("ジ" . "じ")) ("zyi" nil ("ジィ" . "じぃ")) ("u" nil ("ウ" . "う")) ("bu" nil ("ブ" . "ぶ")) ("byu" nil ("ビュ" . "びゅ")) ("chu" nil ("チュ" . "ちゅ")) ("cyu" nil ("チュ" . "ちゅ")) ("du" nil ("ヅ" . "づ")) ("dhu" nil ("デュ" . "でゅ")) ("dyu" nil ("ヂュ" . "ぢゅ")) ("fu" nil ("フ" . "ふ")) ("fyu" nil ("フュ" . "ふゅ")) ("gu" nil ("グ" . "ぐ")) ("gyu" nil ("ギュ" . "ぎゅ")) ("hu" nil ("フ" . "ふ")) ("hyu" nil ("ヒュ" . "ひゅ")) ("ju" nil ("ジュ" . "じゅ")) ("jyu" nil ("ジュ" . "じゅ")) ("ku" nil ("ク" . "く")) ("kyu" nil ("キュ" . "きゅ")) ("mu" nil ("ム" . "む")) ("myu" nil ("ミュ" . "みゅ")) ("nu" nil ("ヌ" . "ぬ")) ("nyu" nil ("ニュ" . "にゅ")) ("pu" nil ("プ" . "ぷ")) ("pyu" nil ("ピュ" . "ぴゅ")) ("ru" nil ("ル" . "る")) ("ryu" nil ("リュ" . "りゅ")) ("su" nil ("ス" . "す")) ("shu" nil ("シュ" . "しゅ")) ("syu" nil ("シュ" . "しゅ")) ("tu" nil ("ツ" . "つ")) ("thu" nil ("テュ" . "てゅ")) ("tsu" nil ("ツ" . "つ")) ("tyu" nil ("チュ" . "ちゅ")) ("vu" nil ("ヴ" . "う゛")) ("wu" nil ("ウ" . "う")) ("xu" nil ("ゥ" . "ぅ")) ("xtu" nil ("ッ" . "っ")) ("xtsu" nil ("ッ" . "っ")) ("xyu" nil ("ュ" . "ゅ")) ("yu" nil ("ユ" . "ゆ")) ("zu" nil ("ズ" . "ず")) ("zyu" nil ("ジュ" . "じゅ")) ("e" nil ("エ" . "え")) ("be" nil ("ベ" . "べ")) ("bye" nil ("ビェ" . "びぇ")) ("che" nil ("チェ" . "ちぇ")) ("cye" nil ("チェ" . "ちぇ")) ("de" nil ("デ" . "で")) ("dhe" nil ("デェ" . "でぇ")) ("dye" nil ("ヂェ" . "ぢぇ")) ("fe" nil ("フェ" . "ふぇ")) ("fye" nil ("フェ" . "ふぇ")) ("ge" nil ("ゲ" . "げ")) ("gye" nil ("ギェ" . "ぎぇ")) ("he" nil ("ヘ" . "へ")) ("hye" nil ("ヒェ" . "ひぇ")) ("je" nil ("ジェ" . "じぇ")) ("jye" nil ("ジェ" . "じぇ")) ("ke" nil ("ケ" . "け")) ("kye" nil ("キェ" . "きぇ")) ("me" nil ("メ" . "め")) ("mye" nil ("ミェ" . "みぇ")) ("ne" nil ("ネ" . "ね")) ("nye" nil ("ニェ" . "にぇ")) ("pe" nil ("ペ" . "ぺ")) ("pye" nil ("ピェ" . "ぴぇ")) ("re" nil ("レ" . "れ")) ("rye" nil ("リェ" . "りぇ")) ("se" nil ("セ" . "せ")) ("she" nil ("シェ" . "しぇ")) ("sye" nil ("シェ" . "しぇ")) ("te" nil ("テ" . "て")) ("the" nil ("テェ" . "てぇ")) ("tye" nil ("チェ" . "ちぇ")) ("ve" nil ("ヴェ" . "う゛ぇ")) ("we" nil ("ウェ" . "うぇ")) ("xe" nil ("ェ" . "ぇ")) ("xke" nil ("ヶ" . "け")) ("xwe" nil ("ヱ" . "ゑ")) ("ye" nil ("イェ" . "いぇ")) ("ze" nil ("ゼ" . "ぜ")) ("zye" nil ("ジェ" . "じぇ")) ("o" nil ("オ" . "お")) ("bo" nil ("ボ" . "ぼ")) ("byo" nil ("ビョ" . "びょ")) ("cho" nil ("チョ" . "ちょ")) ("cyo" nil ("チョ" . "ちょ")) ("do" nil ("ド" . "ど")) ("dho" nil ("デョ" . "でょ")) ("dyo" nil ("ヂョ" . "ぢょ")) ("fo" nil ("フォ" . "ふぉ")) ("fyo" nil ("フョ" . "ふょ")) ("go" nil ("ゴ" . "ご")) ("gyo" nil ("ギョ" . "ぎょ")) ("ho" nil ("ホ" . "ほ")) ("hyo" nil ("ヒョ" . "ひょ")) ("jo" nil ("ジョ" . "じょ")) ("jyo" nil ("ジョ" . "じょ")) ("ko" nil ("コ" . "こ")) ("kyo" nil ("キョ" . "きょ")) ("mo" nil ("モ" . "も")) ("myo" nil ("ミョ" . "みょ")) ("no" nil ("ノ" . "の")) ("nyo" nil ("ニョ" . "にょ")) ("po" nil ("ポ" . "ぽ")) ("pyo" nil ("ピョ" . "ぴょ")) ("ro" nil ("ロ" . "ろ")) ("ryo" nil ("リョ" . "りょ")) ("so" nil ("ソ" . "そ")) ("sho" nil ("ショ" . "しょ")) ("syo" nil ("ショ" . "しょ")) ("to" nil ("ト" . "と")) ("tho" nil ("テョ" . "てょ")) ("tyo" nil ("チョ" . "ちょ")) ("vo" nil ("ヴォ" . "う゛ぉ")) ("wo" nil ("ヲ" . "を")) ("xo" nil ("ォ" . "ぉ")) ("xyo" nil ("ョ" . "ょ")) ("yo" nil ("ヨ" . "よ")) ("zo" nil ("ゾ" . "ぞ")) ("zyo" nil ("ジョ" . "じょ")) ("nn" nil ("ン" . "ん")) ("n'" nil ("ン" . "ん")) ("z/" nil ("・" . "・")) ("z," nil ("‥" . "‥")) ("z." nil ("…" . "…")) ("z-" nil ("〜" . "〜")) ("zh" nil ("←" . "←")) ("zj" nil ("↓" . "↓")) ("zk" nil ("↑" . "↑")) ("zl" nil ("→" . "→")) ("z[" nil ("『" . "『")) ("z]" nil ("』" . "』"))) "\
SKK の標準のローマ字かな変換のオートマトンの状態遷移規則。
リストの各要素は、(現在の状態@入力 次の状態 出力) (但し、\"@\" は連接) を意味
する。
システム用なのでカスタマイズには skk-rom-kana-rule-list を利用してください。")

(defvar skk-rom-kana-rule-list nil "\
*ローマ字かな変換のオートマトンの状態遷移規則。
リストの各要素は、(現在の状態@入力 次の状態 出力) (但し、\"@\" は連接) を意味
する。カスタマイズには skk-standard-rom-kana-rule-list では無く、
こちらを利用してください。")

(defvar skk-fallback-rule-alist '(("n" "ン" . "ん")) "\
*ローマ字かな変換時に、skk-rom-kana-rule-list, skk-standard-rom-kana-rule-list の
あとに参照される規則。
リストの各要素は、(現在の状態 出力) を意味する。
この規則が適用された場合、入力はストリームに返される。")

(defvar skk-postfix-rule-alist '(("oh" "オ" . "お")) "\
*ローマ字かな変換時に、直前のかな文字を作るのに用いられた最後の入力と
現在の入力からかな文字を作りだすための規則。
リストの各要素は、(直前の入力@入力 出力) (但し、\"@\" は連接) を意味する。")

(defvar skk-previous-candidate-char ?x "\
*skk-previous-candidate を割当てたキャラクタ。")

(defvar skk-okuri-char-alist nil "\
*")

(defvar skk-downcase-alist nil "\
*")

(defvar skk-input-vector [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil "！" nil nil nil nil nil nil nil nil nil nil "、" "ー" "。" nil nil nil nil nil nil nil nil nil nil nil "：" "；" nil nil nil "？" nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil "「" nil "」" nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] "\
*skk-self-insert で参照される文字テーブル。
キーに対応する位置に文字列があれば、ひらがなモードもしくはカタカナモードで、該
当のキーを押すことで、対応する文字が挿入される。
例えば、\"~\" キーに対応して、\"〜\" を挿入させるように変更したければ、skk.el 
のロード後 (もしくは skk-load-hook を利用して)、

  (aset skk-input-vector 126 \"〜\")

とするか、もしくは、skk-input-vector の 126 番目 (0 番から数えて) の値を
\"〜\" とするような skk-input-vector を直接書き、setq で代入する (126 は、?
{ を評価したときの値)。")

(defvar skk-zenkaku-vector [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil "　" "！" "”" "＃" "＄" "％" "＆" "’" "（" "）" "＊" "＋" "，" "−" "．" "／" "０" "１" "２" "３" "４" "５" "６" "７" "８" "９" "：" "；" "＜" "＝" "＞" "？" "＠" "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ" "Ｈ" "Ｉ" "Ｊ" "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ" "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ" "Ｕ" "Ｖ" "Ｗ" "Ｘ" "Ｙ" "Ｚ" "［" "＼" "］" "＾" "＿" "‘" "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ" "ｈ" "ｉ" "ｊ" "ｋ" "ｌ" "ｍ" "ｎ" "ｏ" "ｐ" "ｑ" "ｒ" "ｓ" "ｔ" "ｕ" "ｖ" "ｗ" "ｘ" "ｙ" "ｚ" "｛" "｜" "｝" "〜" nil] "\
*skk-zenkaku-insert で参照される文字テーブル。
キーに対応する位置に文字列があれば、全英モードで該当のキーを押すことで、対応す
る文字が挿入される。
値の変更方法については、skk-input-vector を参照のこと。")

(defvar skk-use-face (or window-system (skk-terminal-face-p)) "\
*Non-nil であれば、Emacs 19 の face の機能を使用して変換表示などを行なう。")

(defvar skk-henkan-face (if (and (or window-system (skk-terminal-face-p)) (or (and (fboundp 'frame-face-alist) (assq 'highlight (frame-face-alist (selected-frame)))) (and (fboundp 'face-list) (memq 'highlight (face-list))))) 'highlight) "\
*変換候補の face 属性。skk-use-face が non-nil のときのみ有効。
Emacs 標準フェイスの default, modeline, region, secondary-selection,
highlight, underline, bold, italic, bold-italic の他、新たに face を作り指定す
ることも可能。
新たな face を作り指定するには skk-make-face を利用して、

      (skk-make-face 'DimGray/PeachPuff1)
      (setq skk-henkan-face 'DimGray/PeachPuff1)

のようにするのが手軽。foreground と background の色指定だけでない凝った face
を作る場合は、skk-make-face では対応できないので、Emacs の hilit19.el の
hilit-lookup-face-create などを利用する。色を付ける場合の配色は、canna.el の
canna:attribute-alist が良い例かもしれない。")

(defvar skk-use-color-cursor (and window-system (fboundp 'x-display-color-p) (x-display-color-p)) "\
*Non-nil であれば、SKK モードの入力モードに応じてカーソルに色を付ける。")

(defvar skk-hirakana-cursor-color (if (eq skk-background-mode 'light) "coral4" "pink") "\
*かなモードを示すカーソル色。")

(defvar skk-katakana-cursor-color (if (eq skk-background-mode 'light) "forestgreen" "green") "\
*カタカナモードを示すカーソル色。")

(defvar skk-zenkaku-cursor-color "gold" "\
*全角英字モードを示すカーソル色。")

(defvar skk-ascii-cursor-color (if (eq skk-background-mode 'light) "ivory4" "gray") "\
*アスキーモードを示すカーソル色。")

(defvar skk-abbrev-cursor-color "royalblue" "\
*アスキーモードを示すカーソル色。")

(defvar skk-report-set-cursor-error t "\
*Non-nil であれば、カラーマップ切れが起きた場合、エラーメッセージを表示する。
nil であれば、表示しない。")

(defvar skk-use-cursor-change t "\
*Non-nil であれば、Ovwrt マイナーモード時にカーソルの幅を縮める。")

(defvar skk-auto-insert-paren nil "\
*Non-nil であれば、2 つの文字列をまとめて挿入し、その文字列の間にカーソルを移動する。
例えば、\"「\" を入力したときに \"」\" を自動的に挿入し、両かぎかっこの間に
カーソルを移動する。
挿入する文字列は、skk-auto-paren-string-alist で指定する。")

(defvar skk-auto-paren-string-alist '(("「" . "」") ("『" . "』") ("(" . ")") ("（" . "）") ("{" . "}") ("｛" . "｝") ("〈" . "〉") ("《" . "》") ("[" . "]") ("［" . "］") ("〔" . "〕") ("【" . "】") ("\"" . "\"") ("“" . "”")) "\
*自動的に対になる文字列を入力するための連想リスト。
car の文字列が挿入されたときに cdr の文字列を自動的に挿入する。")

(defvar skk-japanese-message-and-error nil "\
*Non-nil であれば、SKK のメッセージとエラーを日本語で表示する。
nil であれば、英語で表示する。")

(defvar skk-ascii-mode-map nil "\
*ASCII モードのキーマップ。")

(defvar skk-j-mode-map nil "\
*かなモードのキーマップ。")

(defvar skk-zenkaku-mode-map nil "\
*全角モードのキーマップ。")

(defvar skk-abbrev-mode-map nil "\
*SKK abbrev モードのキーマップ。")

(defvar skk-jisyo-save-count 50 "\
*数値であれば、その回数辞書が更新されたときに辞書を自動的にセーブする。
  nil であれば、辞書のオートセーブを行なわない。")

(defvar skk-byte-compile-init-file t "\
*Non-nil であれば、skk-mode 起動時に skk-init-file をバイトコンパイルする。
正確に言うと、

  (1)skk-init-file をバイトコンパイルしたファイルがないか、
  (2)skk-init-file とそのバイトコンパイル済ファイルを比較して、前者の方が新し
     いとき

に skk-init-file をバイトコンパイルする。
nil であれば、skk-init-file とそのバイトコンパイル済みファイルを比較して 
skk-init-file の方が新しいときは、そのバイトコンパイル済ファイルを消す。")

(defvar skk-count-private-jisyo-candidates-exactly nil "\
*Non-nil であれば、Emacs を終了するときに正確に個人辞書の候補数を数える。
nil であれば、1 行に複数の候補があっても 1 候補として数える。
計算結果は、skk-record-file に保存される。")

(defvar skk-compare-jisyo-size-when-saving t "\
*Non-nil であれば、skk-jisyo のセーブ時にファイルサイズのチェックを行なう。
前回セーブした skk-jisyo と今回セーブしようとする辞書とのサイズ比較を行ない、
後者の方が大きいときにユーザーにセーブを続けるかどうかの確認を求める。")

(defvar skk-auto-start-henkan t "\
単語や文節の区切りを示す文字の打鍵により自動的に変換を開始する。
skk-auto-start-henkan-keyword-list により単語や文節の区切りを示す文字を指定する。")

(defvar skk-auto-start-henkan-keyword-list '("を" "、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";" ":" "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}" "]" "?" "." "," "!") "\
自動変換を開始するキーワード。
このリストの要素の文字を挿入すると、SPC を押すことなく自動的に変換を開始する。")

(defvar skk-search-excluding-word-pattern-function nil "\
*個人辞書に取り込まない文字列のパターンを検索する関数を指定する。
確定した文字列を引数に渡して funcall される。

SKK では変換、確定を行なった文字列は全て個人辞書に取り込まれるが、この変数で指
定された関数が non-nil を返すとその文字列は個人辞書に取り込まれない。例えば、
この変数に下記のような指定すると、SKK abbrev mode での変換を除き、カタカナのみ
からなる文字列を変換により得て確定しても、それを個人辞書に取り込まない。

カタカナを変換により求めたいが、個人辞書にはカタカナのみの候補を取り込みたくな
い、など、個人辞書が必要以上に膨れるのを抑える目的に使用できる。

個人辞書に取り込まない文字列については補完が効かないので、注意すること。

  (setq skk-search-excluding-word-pattern-function
        (function
         (lambda (kakutei-word)
         ;; この関数が t を返したときは、その文字列は個人辞書に取り込まれない。
           (save-match-data
             (and
            ;; 送りなし変換で、
              (not skk-okuri-char)
            ;; 確定語がカタカナのみから構成されていて、
              (string-match \"^[ーァ-ン]+$\" kakutei-word)
            ;; SKK abbrev mode 以外での変換か、
              (or (not skk-abbrev-mode)
                ;; 見出し語がカタカナ、ひらがな以外のとき。
                ;; (後で▽マークを付けたときは、見出し語が英文字でも、
                ;; skk-abbrev-modeが t になっていない)。
                  (not (string-match \"^[^ーァ-ンぁ-ん]+$\" skk-henkan-key)) )))))) ")

(defconst skk-kanji-len (length "あ") "\
漢字一文字の長さ。Mule では 3 になる。XEmacs では 1。")

(defvar skk-insert-new-word-function nil "\
候補を挿入したときに funcall される関数を保存する変数。")

(defvar skk-input-mode-string skk-hirakana-mode-string "\
SKK の入力モードを示す文字列。skk-mode 起動時は、skk-hirakana-mode-string。")

(defvar skk-isearch-message nil "\
skk-isearch 関数をコールするためのフラグ。
Non-nil であれば、skk-isearch-message 関数をコールする。")

(defvar skk-mode-invoked nil "\
Non-nil であれば、Emacs を起動後既に skk-mode を起動したことを示す。")

(skk-deflocalvar skk-mode nil "Non-nil であれば、カレントバッファで現在 skk-mode を起動していることを示す。")

(skk-deflocalvar skk-ascii-mode nil "Non-nil であれば、入力モードが ASCII モードであることを示す。")

(skk-deflocalvar skk-j-mode nil "Non-nil であれば、入力モードがかな・カナモードであることを示す。")

(skk-deflocalvar skk-katakana nil "Non-nil であれば、入力モードがカナモードであることを示す。\n\"(and (not skk-katakana) skk-j-mode))\" が t であれば、かなモードであることを\n示す。")

(skk-deflocalvar skk-zenkaku-mode nil "Non-nil であれば、入力モードが全英モードであることを示す。")

(skk-deflocalvar skk-abbrev-mode nil "Non-nil であれば、入力モードが SKK abbrev モードであることを示す。")

(skk-deflocalvar skk-okurigana nil "Non-nil であれば、送り仮名部分が入力中であることを示す。")

(skk-deflocalvar skk-henkan-on nil "Non-nil であれば、▽モード (変換対象の文字列決定のためのモード) であることを示す。")

(skk-deflocalvar skk-henkan-active nil "Non-nil であれば、▼モード (変換中) であることを示す。")

(skk-deflocalvar skk-kakutei-flag nil "Non-nil なら確定して良い候補を見つけた状態であることを指す。\nskk-henkan, skk-search-kakutei-jisyo-file, skk-henkan-show-candidates,\nskk-henkan-in-minibuff と skk-kakutei-save-and-init-variables で変更、参照され\nる。")

(skk-deflocalvar skk-prefix "" "入力するかなを決定するためのプレフィックス。\n後で入力される母音に対応した skk-roma-kana-[aiue] 連想リストで、その\nskk-prefix をキーにして入力すべきかな文字が決定される。\n例えば、\"か\" のように \"k\" から始まる子音を入力しているときは、skk-prefix\nは、\"k\" で、その次に入力された母音 \"a\" に対応する skk-roma-kana-a の中の\n\"k\" をキーに持つ値、\"か\" もしくは \"カ\" が入力すべきかな文字となる。")

(skk-deflocalvar skk-henkan-start-point nil "変換開始ポイントを示すマーカー。")

(skk-deflocalvar skk-kana-start-point nil "かな文字の開始ポイントを示すマーカー。")

(skk-deflocalvar skk-henkan-key nil "変換すべき見出し語。\n例えば、\"▽かな\" を変換すれば、skk-henkan-key には \"かな\" が代入される。\n\"▽わら*う\" のような送りありの変換の場合には、\"わらu\" のように、漢字部分の\n読みがな + 送り仮名の最初の文字のローマ字のプレフィックスが代入される。")

(skk-deflocalvar skk-okuri-char nil "変換すべき語の送り仮名の部分のプレフィックス。\n例えば、\"おく*り\" を変換するときは、skk-okuri-char は \"r\"。\nskk-okuri-char が non-nil であれば、送りありの変換であることを示す。")

(skk-deflocalvar skk-henkan-okurigana nil "現在の変換の送り仮名部分。\n例えば、\"▽うまれ*る\" を変換すれば、skk-henkan-okurigana には \"る\" が代入\nされる。")

(skk-deflocalvar skk-henkan-list nil "変換結果の候補のリスト。\n例えば、\"▽な*く\" という変換すれば、skk-henkan-list は\n(\"鳴\" \"泣\" \"無\" \"亡\") のようになる。")

(skk-deflocalvar skk-henkan-count -1 "skk-henkan-list のリストのインデクスで現在の候補を差すもの。")

(skk-deflocalvar skk-current-search-prog-list nil "skk-search-prog-list の現在の値を保存するリスト。\n最初の変換時は skk-search-prog-list の全ての値を保持し、変換を繰り返すたびに 1\nつづつ短くなってゆく。")

(defvar skk-menu-modified-user-option nil "\
SKK メニューコマンドで変更されたユーザー変数保持するリスト。")

(autoload 'skk-save-point "skk" nil nil 'macro)

(autoload 'skk-message "skk" nil nil 'macro)

(autoload 'skk-error "skk" nil nil 'macro)

(autoload 'skk-yes-or-no-p "skk" nil nil 'macro)

(autoload 'skk-y-or-n-p "skk" nil nil 'macro)

(autoload 'skk-set-marker "skk" nil nil 'macro)

(defsubst skk-j-mode-on (&optional katakana) (setq skk-mode t skk-abbrev-mode nil skk-ascii-mode nil skk-j-mode t skk-zenkaku-mode nil skk-katakana katakana) (if katakana (progn (setq skk-input-mode-string skk-katakana-mode-string) (skk-set-cursor-color skk-katakana-cursor-color)) (setq skk-input-mode-string skk-hirakana-mode-string) (skk-set-cursor-color skk-hirakana-cursor-color)) (force-mode-line-update))

(defsubst skk-ascii-mode-on nil (setq skk-mode t skk-abbrev-mode nil skk-ascii-mode t skk-j-mode nil skk-zenkaku-mode nil skk-katakana nil skk-input-mode-string skk-ascii-mode-string) (skk-set-cursor-color skk-ascii-cursor-color) (force-mode-line-update))

(defsubst skk-zenkaku-mode-on nil (setq skk-mode t skk-abbrev-mode nil skk-ascii-mode nil skk-j-mode nil skk-zenkaku-mode t skk-katakana nil skk-input-mode-string skk-zenkaku-mode-string) (skk-set-cursor-color skk-zenkaku-cursor-color) (force-mode-line-update))

(defsubst skk-abbrev-mode-on nil (setq skk-mode t skk-abbrev-mode t skk-ascii-mode nil skk-j-mode nil skk-zenkaku-mode nil skk-katakana nil skk-input-mode-string skk-abbrev-mode-string) (skk-set-cursor-color skk-abbrev-cursor-color) (force-mode-line-update))

(defsubst skk-in-minibuffer-p nil (window-minibuffer-p (selected-window)))

(defsubst skk-insert-prefix (&optional char) (if skk-echo (let ((buffer-undo-list t)) (insert (or char skk-prefix)))))

(defsubst skk-erase-prefix nil (if skk-echo (let ((buffer-undo-list t)) (delete-region skk-kana-start-point (point)))))

(defsubst skk-numeric-p nil (and skk-use-numeric-conversion (require 'skk-num) skk-num-list))

(autoload 'skk-mode "skk" "\
日本語入力モード。
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
「全英」).  `
' returns to hiragana submode from either ASCII submode.

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

A candidate can be accepted by pressing `
', or by entering a
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
" t nil)

(autoload 'skk-auto-fill-mode "skk" "\
日本語入力モード。自動折り返し機能付き。
マイナーモードの一種で、オリジナルのモードには影響を与えない。
正の引数を与えると、強制的に auto-fill-mode 及び SKK モードに入る。
負の引数を与えると auto-fill-mode 及び SKK モードから抜ける。" t nil)

(autoload 'skk-emulate-original-map "skk" nil nil nil)

(autoload 'skk-get-current-candidate "skk" nil nil nil)

(autoload 'skk-previous-candidate "skk" "\
▼モードであれば、一つ前の候補を表示する。
▼モード以外ではカレントバッファに \"x\" を挿入する。
確定辞書による確定の直後に呼ぶと確定がアンドゥされて、確定前の状態で
skk-last-kakutei-henkan-key がカレントバッファに挿入される。" t nil)

(autoload 'skk-kakutei "skk" "\
現在表示されている語で確定し、辞書の更新を行う。
オプショナル引数の WORD を渡すと、現在表示されている候補とは無関係に WORD で確
定する。" t nil)

(autoload 'skk-start-henkan "skk" "\
▽モードでは変換を開始する。▼モードでは次の候補を表示する。
  その他のモードでは、オリジナルのキー割り付けのコマンドをエミュレートする。" t nil)

(autoload 'skk-save-jisyo "skk" "\
SKK の辞書バッファをセーブする。
オプショナル引数の QUIET が non-nil であれば、辞書セーブ時のメッセージを出さな
い。" t nil)

(autoload 'skk-search "skk" nil nil nil)

(autoload 'skk-nunion "skk" nil nil nil)

(autoload 'skk-update-jisyo "skk" nil nil nil)

(autoload 'skk-jisx0208-to-ascii "skk" nil nil nil)

(autoload 'skk-middle-list "skk" nil nil nil)

(autoload 'skk-set-cursor-properly "skk" nil nil nil)

(autoload 'skk-change-cursor-when-ovwrt "skk" nil nil nil)

(autoload 'skk-make-face "skk" nil nil nil)

;;;***

;;;### (autoloads (skk-adjust-search-prog-list-for-auto-okuri skk-init-auto-okuri-variables skk-remove-common) "skk-auto" "skk/skk-auto.el")

(skk-deflocalvar skk-henkan-in-minibuff-flag nil "ミニバッファで辞書登録を行ったときにこのフラグが立つ。\nskk-remove-common で参照される。")

(autoload 'skk-remove-common "skk-auto" nil nil nil)

(autoload 'skk-init-auto-okuri-variables "skk-auto" nil nil nil)

(autoload 'skk-adjust-search-prog-list-for-auto-okuri "skk-auto" nil nil nil)

;;;***

;;;### (autoloads (skk-previous-completion skk-completion skk-start-henkan-with-completion) "skk-comp" "skk/skk-comp.el")

(autoload 'skk-start-henkan-with-completion "skk-comp" "\
▽モードで読みの補完を行なった後、変換する。
それ以外のモードではオリジナルのキーマップに割り付けられたコマンドをエミュレー
トする。" t nil)

(autoload 'skk-completion "skk-comp" nil nil nil)

(autoload 'skk-previous-completion "skk-comp" nil nil nil)

;;;***

;;;### (autoloads (skk-henkan-face-off-and-remove-itself skk-ignore-dic-word skk-times skk-minus skk-plus skk-calc skk-convert-gengo-to-ad skk-convert-ad-to-gengo skk-clock skk-today skk-date) "skk-gadget" "skk/skk-gadget.el")

(defvar skk-date-ad nil "\
*Non-nil であれば、skk-today, skk-clock で西暦表示する。
nil であれば、元号表示する。")

(defvar skk-number-style 1 "\
*nil もしくは 0 であれば、skk-today, skk-clock の数字を半角で表示する。
t もしくは、1 であれば、全角表示する。
t, 0, 1 以外の non-nil 値であれば、漢数字で表示する。")

(autoload 'skk-date "skk-gadget" nil nil nil)

(autoload 'skk-today "skk-gadget" "\
インタラクティブに起動すると現在の日時を日本語表記でポイントに挿入する。
オプショナル引数の AND-TIME を指定すると、日時に加え、時間も挿入する。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。" t nil)

(autoload 'skk-clock "skk-gadget" "\
デジタル時計をミニバッファに表示する。
quit するとその時点の日時を候補として挿入する。
quit したときに起動してからの経過時間をミニバッファに表示する。
interactive に起動する他、\"clock /(skk-clock)/\" などのエントリを SKK の辞書
に加え、\"/clock\"+ SPC で変換することによっても起動可。C-g で止まる。
実行変換で起動した場合は、C-g した時点の時点の日時を挿入する。
オプショナル引数の KAKUTEI-WHEN-QUIT が non-nil であれば C-g したときに確
定する。
オプショナル引数の TIME-SIGNAL が non-nil であれば、NTT の時報風に ding する。
それぞれ、\"clock /(skk-clock nil t)/\" のようなエントリを辞書に挿入すれば良い。
skk-date-ad と skk-number-style によって表示方法のカスタマイズが可能。" t nil)

(autoload 'skk-convert-ad-to-gengo "skk-gadget" nil nil nil)

(autoload 'skk-convert-gengo-to-ad "skk-gadget" nil nil nil)

(autoload 'skk-calc "skk-gadget" nil nil nil)

(autoload 'skk-plus "skk-gadget" nil nil nil)

(autoload 'skk-minus "skk-gadget" nil nil nil)

(autoload 'skk-times "skk-gadget" nil nil nil)

(autoload 'skk-ignore-dic-word "skk-gadget" nil nil nil)

(autoload 'skk-henkan-face-off-and-remove-itself "skk-gadget" nil nil nil)

;;;***

;;;### (autoloads nil "skk-isearch" "skk/skk-isearch.el")

(defvar skk-isearch-whitespace-regexp "\\(\\s \\|[ 	\n\^L]\\)*")

;;;***

;;;### (autoloads (skk-romaji-message skk-romaji-region skk-hurigana-katakana-message skk-hurigana-katakana-region skk-hurigana-message skk-hurigana-region skk-gyakubiki-katakana-message skk-gyakubiki-katakana-region skk-gyakubiki-message skk-gyakubiki-region) "skk-kakasi" "skk/skk-kakasi.el")

(autoload 'skk-gyakubiki-region "skk-kakasi" "\
リージョンの漢字、送り仮名を全てひらがなに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload 'skk-gyakubiki-message "skk-kakasi" "\
リージョンの漢字、送り仮名を全てひらがなに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload 'skk-gyakubiki-katakana-region "skk-kakasi" "\
リージョンの漢字、送り仮名を全てカタカナに変換する。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload 'skk-gyakubiki-katakana-message "skk-kakasi" "\
リージョンの漢字、送り仮名を全てカタカナに変換後、エコーする。
オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload 'skk-hurigana-region "skk-kakasi" "\
リージョンの漢字に全てふりがなを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload 'skk-hurigana-message "skk-kakasi" "\
リージョンの漢字に全てふりがなを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[へんかんまえ]の漢字[かんじ]の脇[わき]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {なかしま|なかじま}" t nil)

(autoload 'skk-hurigana-katakana-region "skk-kakasi" "\
リージョンの漢字に全てフリガナを付ける。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload 'skk-hurigana-katakana-message "skk-kakasi" "\
リージョンの漢字に全てフリガナを付け、エコーする。
例えば、
   \"変換前の漢字の脇に\" -> \"変換前[ヘンカンマエ]の漢字[カンジ]の脇[ワキ]に\"

オプショナル引数の ALL が non-nil ならば、複数の候補がある場合は、\"{}\" でく
くって表示する。
例えば、
    中島 -> {ナカシマ|ナカジマ}" t nil)

(autoload 'skk-romaji-region "skk-kakasi" "\
リージョンの漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換する。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。" t nil)

(autoload 'skk-romaji-message "skk-kakasi" "\
リージョンの漢字、ひらがな、カタカナ、全英文字を全てローマ字に変換し、エコーする。
変換には、ヘボン式を用いる。
例えば、
   \"漢字かな混じり文をローマ字に変換\"
    -> \"  kan'zi  kana  ma  ziri  bun'  woro-ma  zi ni hen'kan' \"

skk-romaji-*-by-hepburn が nil であれば、ローマ字への変換様式を訓令式に変更す
る。例えば、\"し\" はヘボン式では \"shi\" だが、訓令式では \"si\" となる。" t nil)

;;;***

;;;### (autoloads (skk-display-code-for-char-at-point skk-input-by-code-or-menu) "skk-kcode" "skk/skk-kcode.el")

(autoload 'skk-input-by-code-or-menu "skk-kcode" "\
7bit もしくは 8bit もしくは 区点コードに対応する 2byte 文字を挿入する。" t nil)

(autoload 'skk-display-code-for-char-at-point "skk-kcode" "\
ポイントにある文字の EUC コードと JIS コードを表示する。" t nil)

;;;***

;;;### (autoloads (skk-menu-use-color-cursor skk-menu-compare-jisyo-size-when-saving skk-menu-server-debug skk-menu-report-server-response skk-menu-numeric-conversion-float-num skk-menu-use-kakasi skk-menu-romaji-*-by-hepburn skk-menu-date-ad skk-menu-dabbrev-like-completion skk-menu-auto-henkan skk-menu-count-private-jisyo-entries-exactly skk-menu-japanese-message-and-error skk-menu-auto-insert-paren skk-menu-use-overlay skk-menu-use-numeric-conversion skk-menu-echo skk-menu-delete-okuri-when-quit skk-menu-convert-okurigana-into-katakana skk-menu-allow-spaces-newlines-and-tabs skk-menu-delete-implies-kakutei skk-menu-egg-like-newline skk-menu-kakutei-early skk-menu-auto-okuri-process skk-menu-henkan-strict-okuri-precedence skk-menu-henkan-okuri-strictly skk-menu-process-okuri-early skk-menu-save-modified-user-option) "skk-menu" "skk/skk-menu.el")

(autoload 'skk-menu-save-modified-user-option "skk-menu" nil nil nil)

(autoload 'skk-menu-process-okuri-early "skk-menu" "\
skk-process-okuri-early をスイッチオン/オフする。
両立できないオプションの値を調整する。" t nil)

(autoload 'skk-menu-henkan-okuri-strictly "skk-menu" "\
skk-henkan-okuri-strictly をスイッチオン/オフする。
両立できないオプションの値を調整する。" t nil)

(autoload 'skk-menu-henkan-strict-okuri-precedence "skk-menu" "\
skk-henkan-strict-okuri-precedence をスイッチオン/オフする。
両立できないオプションの値を調整する。" t nil)

(autoload 'skk-menu-auto-okuri-process "skk-menu" "\
skk-auto-okuri-process をスイッチオン/オフする。
両立できないオプションの値を調整する。" t nil)

(autoload 'skk-menu-kakutei-early "skk-menu" "\
skk-kakutei-early をスイッチオン/オフする。
両立できないオプションの値を調整する。" t nil)

(autoload 'skk-menu-egg-like-newline "skk-menu" "\
skk-egg-like-newline をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-delete-implies-kakutei "skk-menu" "\
skk-delete-implies-kakutei をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-allow-spaces-newlines-and-tabs "skk-menu" "\
skk-allow-spaces-newlines-and-tabs をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-convert-okurigana-into-katakana "skk-menu" "\
skk-convert-okurigana-into-katakana をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-delete-okuri-when-quit "skk-menu" "\
skk-delete-okuri-when-quit をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-echo "skk-menu" "\
skk-echo をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-use-numeric-conversion "skk-menu" "\
skk-use-numeric-conversion をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-use-overlay "skk-menu" "\
skk-use-face をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-auto-insert-paren "skk-menu" "\
skk-auto-insert-paren をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-japanese-message-and-error "skk-menu" "\
skk-japanese-message-and-error をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-count-private-jisyo-entries-exactly "skk-menu" "\
skk-count-private-jisyo-candidates-exactly をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-auto-henkan "skk-menu" "\
skk-auto-start-henkan をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-dabbrev-like-completion "skk-menu" "\
skk-dabbrev-like-completion をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-date-ad "skk-menu" "\
skk-date-ad をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-romaji-*-by-hepburn "skk-menu" "\
skk-romaji-*-by-hepburn をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-use-kakasi "skk-menu" "\
skk-use-kakasi をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-numeric-conversion-float-num "skk-menu" "\
skk-numeric-conversion-float-num をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-report-server-response "skk-menu" "\
skk-report-server-response をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-server-debug "skk-menu" "\
skk-server-debug をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-compare-jisyo-size-when-saving "skk-menu" "\
skk-compare-jisyo-size-when-saving をスイッチオン/オフする。" t nil)

(autoload 'skk-menu-use-color-cursor "skk-menu" "\
skk-use-color-cursor をスイッチオン/オフする。" t nil)

;;;***

;;;### (autoloads (skk-num skk-update-jisyo-for-numerals skk-numeric-midasi-word skk-init-numeric-conversion-variables skk-adjust-numeric-henkan-data skk-uniq-numerals skk-numeric-convert*7 skk-numeric-convert skk-compute-numeric-henkan-key) "skk-num" "skk/skk-num.el")

(defvar skk-num-type-list '((?0 . identity) (?1 . skk-zenkaku-num-str) (?2 . skk-kanji-num-str) (?3 . skk-kanji-num-str2) (?4 . skk-recompute-numerals) (?9 . skk-shogi-num-str)) "\
*数字の変換のための、インデクスと変換に使用する関数とのドットペアのリスト。
各要素は、(数字の char-type . 関数名) という構成になっている。
car 部分は、例えば、見出し語が \"平成#1年\" のとき、# 記号の直後に表示される数
字 \"1\" を char-type で表わしたものを代入する。")

(defvar skk-uniq-numerals (or (assq ?4 skk-num-type-list) (and (assq ?2 skk-num-type-list) (assq ?3 skk-num-type-list))) "\
*Non-nil であれば、異なる数値表現でも変換結果が同じ数値を重複して出力しない。")

(skk-deflocalvar skk-num-list nil "skk-henkan-key の中に含まれる数字を表す文字列のリスト。\n例えば、\"▽へいせい7ねん10がつ\" の変換を行うとき、skk-henkan-key は\n\"へいせい7ねん10がつ\" であり、skk-num-list は (\"7\" \"10\") となる。")

(skk-deflocalvar skk-recompute-numerals-key nil "#4 タイプのキーにより数値の再計算を行なったときの検索キー。")

(autoload 'skk-compute-numeric-henkan-key "skk-num" nil nil nil)

(autoload 'skk-numeric-convert "skk-num" nil nil nil)

(autoload 'skk-numeric-convert*7 "skk-num" nil nil nil)

(autoload 'skk-uniq-numerals "skk-num" nil nil nil)

(autoload 'skk-adjust-numeric-henkan-data "skk-num" nil nil nil)

(autoload 'skk-init-numeric-conversion-variables "skk-num" nil nil nil)

(autoload 'skk-numeric-midasi-word "skk-num" nil nil nil)

(autoload 'skk-update-jisyo-for-numerals "skk-num" nil nil nil)

(autoload 'skk-num "skk-num" nil nil nil)

;;;***

;;;### (autoloads (skk-adjust-search-prog-list-for-server-search skk-server-version) "skk-server" "skk/skk-server.el")

(defvar skk-server-host (getenv "SKKSERVER") "\
*SKK 辞書サーバーを走らせているホスト名。")

(defvar skk-server-prog (getenv "SKKSERV") "\
*SKK 辞書サーバープログラム名。フルパスで書く。")

(defvar skk-servers-list nil "\
*辞書サーバー毎の情報リスト。
複数のマシーンで動いているサーバにアクセスできる場合には、以下のように、リスト
の各要素に順にホスト名、フルパスでの SKK サーバー名、SKK サーバーに渡す辞書名、
SKK サーバーが使用するポート番号を書き、設定をすることもできる。

   (setq skk-servers-list
         '((\"mars\" \"/usr/local/soft/nemacs/etc/skkserv\" nil nil)
           (\"venus\" \"/usr/local/nemacs/etc/skkserv\" nil nil) ))

この場合最初に指定したサーバにアクセスできなくなると、自動的に順次リストにある
残りのサーバにアクセスするようになる。なお SKK サーバーに渡す辞書および SKK サー
バーが使用するポート番号で、SKK サーバーをコンパイル時の値を使用する場合は nil 
を指定する。")

(autoload 'skk-server-version "skk-server" nil t nil)

(autoload 'skk-adjust-search-prog-list-for-server-search "skk-server" nil nil nil)

;;;***

;;;### (autoloads (skk-assoc-tree) "skk-tree" "skk/skk-tree.el")

(defvar skk-rom-kana-rule-tree nil "\
*skk-rom-kana-rule-list の要素数が多くなったときに使用するツリー。
.emacs に
        (setq skk-rom-kana-rule-tree
              (skk-compile-rule-list skk-rom-kana-rule-list))
を追加する.

このままでは SKK を起動するときに毎回 \"skk-compile-rule-list\" を計算す
ることになるので, うまくいくことがわかれば,
        (skk-compile-rule-list skk-rom-kana-rule-list)
の値を直接 .emacs に書いておくとよい。")

(defvar skk-standard-rom-kana-rule-tree nil "\
*skk-standard-rom-kana-rule-list の要素数が多くなったときに使用するツリー。
.emacs に
        (setq skk-standard-rom-kana-rule-tree
              (skk-compile-rule-list skk-standard-rom-kana-rule-list))
を追加する.

このままでは SKK を起動するときに毎回 \"skk-compile-rule-list\" を計算す
ることになるので, うまくいくことがわかれば,
        (skk-compile-rule-list skk-standard-rom-kana-rule-list)
の値を直接 .emacs に書いておくとよい。")

(autoload 'skk-assoc-tree "skk-tree" nil nil nil)

;;;***

;;;### (autoloads (skk-vip-mode) "skk-vip" "skk/skk-vip.el")

(autoload 'skk-vip-mode "skk-vip" nil nil nil)

;;;***

;;;### (autoloads nil "skk-viper" "skk/skk-viper.el")

(defvar skk-viper-normalize-map-function nil "\
Viper が minor-mode-map-alist を調整するための関数。")

;;;***
(provide 'skk-vars)
