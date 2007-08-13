;; Copyright (C) 1992,1994 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of GNU Emacs).
;; This file contains Korean symbol characters from KSC5601 code table
;; for use in Korean documents.

;; Mule is free software distributed in the form of patches to GNU Emacs.
;; You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; Mule is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; 94.10.24   Written for Mule Ver.2.0 (K.Un.)
;;;	<zraun01@hpserv.zdv.uni-tuebingen.de>
;;; 94.11.04   Updated for Mule Ver.2.1 (K.Un.)
;;;	<zraun01@hpserv.zdv.uni-tuebingen.de>

;; # Hangul symbol input table for Mule to be used in hangul document.
;; ENCODE:	KSC
;; MULTICHOICE:	YES
;; PROMPT:	::㉿::
;; #
;; COMMENT
;; COMMENT	한글 심볼 글자
;; COMMENT
;; # define keys
;; VALIDINPUTKEY:	(_)abcdefghijklmnopqrstuvwxyzCDEGKMNOPQRSTUW
;; SELECTKEY:	1\040
;; SELECTKEY:	2
;; SELECTKEY:	3
;; SELECTKEY:	4
;; SELECTKEY:	5
;; SELECTKEY:	6
;; SELECTKEY:	7
;; SELECTKEY:	8
;; SELECTKEY:	9
;; SELECTKEY:	0
;; BACKSPACE:	\010\177
;; DELETEALL:	\015\025
;; MOVERIGHT:	.>
;; MOVELEFT:	,<
;; REPEATKEY:	\020\022
;; # the following line must not be removed
;; BEGINDICTIONARY
;; #

(require 'quail)

(quail-define-package
 "hsymbol" "㉿" t
 "한글심볼입력표:
  【(】괄호열기【)】괄호닫기 【won】돈  【pic】상형문자 【xtext】§※¶¡¿
  【music】음악【quot】따옴표【dot】점  【arrow】화살   【math】수학기호
  【index】첨자【unit】단위  【sex】♂♀【accent】악센트【Unit】℃Å￠℉
  【wn】㈜【ks】㉿【No】№【㏇】㏇ 【percent】‰【line】선문자
  【am】㏂【pm】㏘【™】™【Tel】℡【dag】†‡  【frac】분수
  【textline】­―∥＼∼          【Scan】ÐªĦ… 【scan】đðħ…
  【enum】０１２…【Eng】ＡＢＣ… 【eng】ａｂｃ…  【easc】영어ASCII
  【Rom】ⅠⅡⅢ… 【rom】ⅰⅱⅲ… 【Greek】ΑΒΓ…【greek】αβγ…
  【ojaso】㉠∼㉭ 【ogana】㉮∼㉻ 【oeng】ⓐ∼ⓩ   【onum】①∼⑮
  【pjaso】㈀∼㈍ 【pgana】㈎∼㈛ 【peng】⒜∼⒵   【pnum】⑴∼⒂
  【hira】あぃい  【kata】アィイ  【Russ】БВГ… 【russ】абв…
  【자소】2벌식 + ㅥ(S) ㅿ(t_) ㆀ(DD) ㆁ(D) ㆆ(G) ㆅ(GG) ㆍ(uk)"
 '(
   ("," . quail-prev-candidate-block)
   ("<" . quail-prev-candidate-block)
   ("." . quail-next-candidate-block)
   (">" . quail-next-candidate-block)
   (" " . quail-select-current)
   )
 )

(qdv "("	"〔〈《「『【")
(qdv ")"	"〕〉》」』】")
(qdv "math"	"±×÷≠≤≥∞∴∠⊥⌒∂∇≡≒〓≪≫√∽∝∵∫∬∈∋⊆⊇⊂⊃∪∩∧∨￢⇔∀∃∮∑∏")
(qdv "pic"	"☆○★●◎◇◆□■△▲▽▼◁◀▷▶♤♠♡♥♧♣⊙◈▣◐◑▒▤▥▨▧▦▩♨☏☎☜☞¤")
(qdv "arrow"	"→←↑↓↔↕↗↙↖↘")
(qdv "music"	"♩♭♪♬")
(qdv "won"	"￦￥￡")
(qdv "xtext"	"§※¶¡¿")
(qdv "dot"	"·‥…¨ː")
(qdv "quot"	"、。〃‘’“”°′″´˝")
(qdv "textline"	"­―∥＼∼")
(qdv "Unit"	"℃Å￠℉")
(qdv "sex"	"♂♀")
(qdv "accent"	"～ˇ˘˚˙¸˛")
(qdv "percent"	"‰")
(qdv "dag"	"†‡")
(qdv "wn"	"㈜")
(qdv "ks"	"㉿")
(qdv "No"	"№")
(qdv "Co"	"㏇")
(qdv "TM"	"™")
(qdv "am"	"㏂")
(qdv "pm"	"㏘")
(qdv "Tel"	"℡")
(qdv "easc"	"＂＃＄％＆＇（）＊＋，－．／：；＜＝＞？＠［］＾＿｀｛｜｝￣")
(qdv "enum"	"０１２３４５６７８９")
(qdv "Eng"	"ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ")
(qdv "eng"	"ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ")
(qdv "r"	"ㄱ")
(qdv "R"	"ㄲ")
(qdv "rt"	"ㄳ")
(qdv "s"	"ㄴ")
(qdv "sw"	"ㄵ")
(qdv "sg"	"ㄶ")
(qdv "e"	"ㄷ")
(qdv "E"	"ㄸ")
(qdv "f"	"ㄹ")
(qdv "fr"	"ㄺ")
(qdv "fa"	"ㄻ")
(qdv "fq"	"ㄼ")
(qdv "ft"	"ㄽ")
(qdv "fx"	"ㄾ")
(qdv "fv"	"ㄿ")
(qdv "fg"	"ㅀ")
(qdv "a"	"ㅁ")
(qdv "q"	"ㅂ")
(qdv "Q"	"ㅃ")
(qdv "qt"	"ㅄ")
(qdv "t"	"ㅅ")
(qdv "T"	"ㅆ")
(qdv "d"	"ㅇ")
(qdv "w"	"ㅈ")
(qdv "W"	"ㅉ")
(qdv "c"	"ㅊ")
(qdv "z"	"ㅋ")
(qdv "x"	"ㅌ")
(qdv "v"	"ㅍ")
(qdv "g"	"ㅎ")
(qdv "k"	"ㅏ")
(qdv "o"	"ㅐ")
(qdv "i"	"ㅑ")
(qdv "I"	"ㅒ")
(qdv "j"	"ㅓ")
(qdv "p"	"ㅔ")
(qdv "u"	"ㅕ")
(qdv "P"	"ㅖ")
(qdv "h"	"ㅗ")
(qdv "hk"	"ㅘ")
(qdv "ho"	"ㅙ")
(qdv "hl"	"ㅚ")
(qdv "y"	"ㅛ")
(qdv "n"	"ㅜ")
(qdv "nh"	"ㅝ")
(qdv "np"	"ㅞ")
(qdv "nl"	"ㅟ")
(qdv "b"	"ㅠ")
(qdv "m"	"ㅡ")
(qdv "ml"	"ㅢ")
(qdv "l"	"ㅣ")
(qdv "S"	"ㅥ")
(qdv "se"	"ㅦ")
(qdv "st"	"ㅧ")
(qdv "st_"	"ㅨ")
(qdv "frt"	"ㅩ")
(qdv "fqt"	"ㅫ")
(qdv "fe"	"ㅪ")
(qdv "ft_"	"ㅬ")
(qdv "fG"	"ㅭ")
(qdv "aq"	"ㅮ")
(qdv "at"	"ㅯ")
(qdv "at_"	"ㅰ")
(qdv "aD"	"ㅱ")
(qdv "qr"	"ㅲ")
(qdv "qe"	"ㅳ")
(qdv "qtr"	"ㅴ")
(qdv "qte"	"ㅵ")
(qdv "qw"	"ㅶ")
(qdv "qx"	"ㅷ")
(qdv "qD"	"ㅸ")
(qdv "QD"	"ㅹ")
(qdv "tr"	"ㅺ")
(qdv "ts"	"ㅻ")
(qdv "te"	"ㅼ")
(qdv "tq"	"ㅽ")
(qdv "tw"	"ㅾ")
(qdv "t_"	"ㅿ")
(qdv "DD"	"ㆀ")
(qdv "D"	"ㆁ")
(qdv "Dw"	"ㆂ")
(qdv "Dt_"	"ㆃ")
(qdv "vD"	"ㆄ")
(qdv "GG"	"ㆅ")
(qdv "G"	"ㆆ")
(qdv "yi"	"ㆇ")
(qdv "yO"	"ㆈ")
(qdv "yl"	"ㆉ")
(qdv "bu"	"ㆊ")
(qdv "bP"	"ㆋ")
(qdv "bl"	"ㆌ")
(qdv "uk"	"ㆍ")
(qdv "ukl"	"ㆎ")
(qdv "Rom"	"ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ")
(qdv "rom"	"ⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹ")
(qdv "Greek"	"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")
(qdv "greek"	"αβγδεζηθικλμνξοπρστυφχψω")
(qdv "line"	"│┌┐┘└├┬┤┴┼━┃┏┓┛┗┣┳┫┻╋┠┯┨┷┿┝┰┥┸╂┒┑┚┙┖┕┎┍┞┟┡┢┦┧┩┪┭┮┱┲┵┶┹┺┽╀┾╁╃╄╅╆╇╈╉╊")
(qdv "unit"	"㎖㎗ℓ㎘㏄㎣㎤㎥㎦㎙㎚㎛㎜㎝㎞㎟㎠㎡㎢㏊㎍㎎㎏㏏㎈㎉㏈㎧㎨㎰㎱㎲㎳㎴㎵㎶㎷㎸㎹㎀㎁㎂㎃㎄㎺㎻㎼㎽㎾㎿㎐㎑㎒㎓㎔Ω㏁㏀㎊㎋㎌㏖㏅㎭㎮㎯㏛㎩㎪㎫㎬㏝㏐㏓㏃㏉㏜㏆")
(qdv "Scan"	"ÐªĦĲĿŁØŒºÞŦŊ")
(qdv "ojaso"	"㉠㉡㉢㉣㉤㉥㉦㉧㉨㉩㉪㉫㉬㉭")
(qdv "ogana"	"㉮㉯㉰㉱㉲㉳㉴㉵㉶㉷㉸㉹㉺㉻")
(qdv "oeng"	"ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓞⓝⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ")
(qdv "onum"	"①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮")
(qdv "frac"	"½⅓⅔¼¾⅛⅜⅝⅞")
(qdv "scan"	"đðħıĳĸŀłøœßþŧŋŉ")
(qdv "pjaso"	"㈀㈁㈂㈃㈄㈅㈆㈇㈈㈉㈊㈋㈌㈍>")
(qdv "pgana"	"㈎㈏㈐㈑㈒㈓㈔㈕㈖㈗㈘㈙㈚㈛")
(qdv "peng"	"⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒪⒩⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵")
(qdv "pnum"	"⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂")
(qdv "index"	"¹²³⁴ⁿ₁₂₃₄")
(qdv "hira"	"あぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべほぺぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをん")
(qdv "kata"	"アィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベホペボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ")
(qdv "Russ"	"БВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")
(qdv "russ"	"абвгдеёжзйиклмнопрстуфхцчшщъыьэюя")

(quail-setup-current-package)
