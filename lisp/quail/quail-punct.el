(require 'quail)
;; # !Id: Punct.tit,v 1.1 1991/10/27 06:21:16 ygz Exp !
;; # HANZI input table for cxterm
;; # To be used by cxterm, convert me to .cit format first
;; # .cit version 1
;; ENCODE:	GB
;; MULTICHOICE:	YES
;; PROMPT:	$A::WVJdHk!K1j5c7{:E!K# (B
;; #
;; COMMENT	Copyright 1991 by Yongguang Zhang.
;; COMMENT Permission to use/modify/copy for any purpose is hereby granted.
;; COMMENT Absolutely no fee and no warranties.
;; COMMENT
;; COMMENT	use <CTRL-f> to move to the right
;; COMMENT	use <CTRL-b> to move to the left
;; # define keys
;; VALIDINPUTKEY:	"\043$%&'()*+,-./0123456789:;<=>?@[\134]^_`abcdefghijklm
;; VALIDINPUTKEY:	nopqrstuvwxyz|~
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
;; MOVERIGHT:	\006
;; MOVELEFT:	\002
;; REPEATKEY:	\020\022
;; # the following line must not be removed
;; BEGINDICTIONARY
(quail-define-package "punct" "$A1j5c7{:E(B"
 t
 "$A::WVJdHk!K1j5c7{:E!K# (B
	Copyright 1991 by Yongguang Zhang.
 Permission to use/modify/copy for any purpose is hereby granted.
 Absolutely no fee and no warranties.

	use <CTRL-f> to move to the right
	use <CTRL-b> to move to the left"
 '(
  (" " . quail-select-current)
  )
 nil nil)

;; #
(qdv "\""	"$A#"!0!1!e(B")
(qd "\043"	"$A##(B")
(qdv "$"	"$A#$!g!i!j(B")
(qdv "%"	"$A#%!k(B")
(qdv "'"	"$A#'!.!/!d(B")
(qdv "("	"$A#(!8!:(B")
(qdv ")"	"$A#)!9!;(B")
(qdv "*"	"$A#*!A!G!D!I(B")
(qdv "+"	"$A#+!@!F!E!H(B")
(qdv ","	"$A#,!"(B")
(qdv "-"	"$A#-!%!*!+(B")
(qdv "."	"$A#.!#!$!'!-!_!`!c!Q(B")
(qdv "/"	"$A#/!B!L#\(B")
(qdv "0"	"$A#0":"D"N"X"b"n"z(B")
(qdv "1"	"$A#1"1";"E"O"Y"e"q(B")
(qdv "2"	"$A#2"2"<"F"P"Z"f"r(B")
(qdv "3"	"$A#3"3"="G"Q"["g"s(B")
(qdv "4"	"$A#4"4">"H"R"\"h"t(B")
(qdv "5"	"$A#5"5"?"I"S"]"i"u(B")
(qdv "6"	"$A#6"6"@"J"T"^"j"v(B")
(qdv "7"	"$A#7"7"A"K"U"_"k"w(B")
(qdv "8"	"$A#8"8"B"L"V"`"l"x(B")
(qdv "9"	"$A#9"9"C"M"W"a"m"y(B")
(qdv ":"	"$A#:!K(B")
(qdv "<"	"$A#<!4!6!Z!\(B")
(qdv "="	"$A#=!Y!V!T!U(B")
(qdv ">"	"$A#>!5!7![!](B")
(qdv "@"	"$A#@!Q(B")
(qdv "["	"$A#[!2!<!>#{(B")
(qdv "\134"	"$A#\#/(B")
(qdv "]"	"$A#]!3!=!?#}(B")
(qdv "^"	"$A#^!&!P(B")
(qdv "_"	"$A#_!M(B")
(qdv "`"	"$A#`!.!/(B")
(qdv "logo"	"$A!n!o!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~(B")
(qdv "math"	"$A!@!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O!P!Q!R!S!T!U!V!W!X!Y!Z![!\!]!^!_!`(B")
(qdv "punct"	"$A!!!"!#!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>!?(B")
(qdv "symbol"	"$A!a!b!c!d!e!f!g!h!i!j!k!l!m(B")
(qdv "|"	"$A#|!,!N(B")
(qdv "~"	"$A#~!+!W!X!^(B")

(quail-setup-current-package)
