(require 'quail)
;; # HANZI input table for cxterm
;; # Generated from Punct-b5.cit by cit2tit
;; # To be used by cxterm, convert me to .cit format first
;; # .cit version 1
;; ENCODE:	BIG5
;; MULTICHOICE:	YES
;; PROMPT:	$(0&d'GTT&,!JO:X5>KHA!K(B
;; #
;; COMMENT Copyright 1991 by Yongguang Zhang.	(ygz@cs.purdue.edu)
;; COMMENT Permission to use/modify/copy for any purpose is hereby granted.
;; COMMENT Absolutely no fee and no warranties.
;; COMMENT
;; COMMENT use <CTRL-f> to move to the right
;; COMMENT use <CTRL-b> to move to the left
;; COMMENT Modify by Wei-Chung Hwang, OCT 15, 1992.
;; # define keys
;; VALIDINPUTKEY:	!"\043$%&'()*+,-./0123456789:;<=>?@[\134]^_`abcdefghijkl
;; VALIDINPUTKEY:	mnopqrstuvwxyz{|}~
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
(quail-define-package "punct-b5" "$(0O:X5>KHA(B"
 t
 "$(0&d'GTT&,!JO:X5>KHA!K(B
 Copyright 1991 by Yongguang Zhang.	(ygz@cs.purdue.edu)
 Permission to use/modify/copy for any purpose is hereby granted.
 Absolutely no fee and no warranties.

 use <CTRL-f> to move to the right
 use <CTRL-b> to move to the left
 Modify by Wei-Chung Hwang, OCT 15, 1992."
 '(
  (" " . quail-select-current)
  )
 nil nil)

;; #
(qdv "!"	"$(0!*!5(B")
(qdv "\""	"$(0!f!g!h!i!q(B")
(qdv "\043"	"$(0!l"-(B")
(qdv "$"	"$(0"d"l"f"g(B")
(qd "%"	"$(0"h(B")
(qdv "&"	"$(0".!m(B")
(qdv "'"	"$(0!d!e!j!k(B")
(qdv "("	"$(0!>!F!^!@!H!V!Z!Y!](B")
(qdv ")"	"$(0!?!G!_!A!I!W![!X!\(B")
(qdv "*"	"$(0"/"2$T$O"E(B")
(qdv "+"	"$(0"0"?"4$V!U"F(B")
(qdv ","	"$(0!"!.!#!/(B")
(qdv "-"	"$(0"1"@"@!7"##9"D(B")
(qdv "."	"$(0!$!%!&!0!1!,!-"O"P"x"T(B")
(qdv "/"	"$(0"_"a"3"5"`"b(B")
(qdv "0"	"$(0#O%M%W%a#b#l(B")
(qdv "1"	"$(0#P%D%N%X#Y#c#m(B")
(qdv "2"	"$(0#Q%E%O%Y#Z#d#n(B")
(qdv "3"	"$(0#R%F%P%Z#[#e(B")
(qdv "4"	"$(0#S%G%Q%[#\#f(B")
(qdv "5"	"$(0#T%H%R%\#]#g(B")
(qdv "6"	"$(0#U%I%S%]#^#h(B")
(qdv "7"	"$(0#V%J%T%^#_#i(B")
(qdv "8"	"$(0#W%K%U%_#`#j(B")
(qdv "9"	"$(0#X%L%V%`#a#k(B")
(qdv ":"	"$(0!(!+!3(B")
(qdv ";"	"$(0!'!2(B")
(qdv "<"	"$(0!R"A!N!P!T"9(B")
(qdv "="	"$(0"8"C";"=">(B")
(qdv ">"	"$(0!S"B!O!Q!U":(B")
(qdv "?"	"$(0!)!4(B")
(qdv "@"	"$(0"i"T(B")
(qdv "["	"$(0!b!J!L(B")
(qdv "\134"	"$(0"`"b"_"a(B")
(qdv "]"	"$(0!c!K!M(B")
(qd "^"	"$(0!T(B")
(qdv "_"	"$(0!;!=(B")
(qdv "`"	"$(0!d!e!j!k(B")
(qdv "graph"	"$(0#$#%#&#'#(#)#*#+#,#-#.#/#0#1#2#3#4#5#6#7#8#9#:#;#<#=#>#?#@#A#B#C#D#E#F#G#H#I#J#K#L#M#N(B")
(qdv "logo"	"$(0!n!o!p!q!r!s!t!u!v!w!x!y!z!{!|!}!~"!"""#"$"%"&"'"(")"*"+",(B")
(qdv "math"	"$(0"0"1"2"3"4"5"6"7"8"9":";"<"=">"?"@"A"B"C"D"E"F"G"H"I"J"K"L"M"N"O"P(B")
(qdv "symbol"	"$(0"R"Q"S"T"U"V"W"X"Y"Z"["\"]"^"_"`"a"b(B")
(qdv "unit"	"$(0"c"d"e"f"g"h"h"i"p"q"r"s"t"u"v"w"x"y"z"{"|"}"~#!#"##(B")
(qdv "{"	"$(0!`!B!D(B")
(qdv "|"	"$(0!6"^!:#:"](B")
(qdv "}"	"$(0!a!C!E(B")
(qdv "~"	"$(0!="D"<(B")

(quail-setup-current-package)
