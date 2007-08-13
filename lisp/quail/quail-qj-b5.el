(require 'quail)
;; # HANZI input table for cxterm
;; # Generated from QJ-b5.cit by cit2tit
;; # To be used by cxterm, convert me to .cit format first
;; # .cit version 1
;; ENCODE:	BIG5
;; MULTICHOICE:	NO
;; PROMPT:	$(0&d'GTT&,!J)A,1!K(B
;; #
;; COMMENT Copyright 1991 by Yongguang Zhang.      (ygz@cs.purdue.edu)
;; COMMENT Permission to use/modify/copy for any purpose is hereby granted.
;; COMMENT Absolutely no warranties.
;; COMMENT Modify by Wei-Chung Hwang, OCT 15, 1992.
;; # define keys
;; VALIDINPUTKEY:	\040!"\043$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMN
;; VALIDINPUTKEY:	OPQRSTUVWXYZ[\134]^_`abcdefghijklmnopqrstuvwxyz{|}~
;; BACKSPACE:	\010\177
;; DELETEALL:	\015\025
;; REPEATKEY:	\020\022
;; # the following line must not be removed
;; BEGINDICTIONARY
(quail-define-package "qj-b5" "$(0)A,1(B"
 nil
 "$(0&d'GTT&,!J)A,1!K(B
 Copyright 1991 by Yongguang Zhang.      (ygz@cs.purdue.edu)
 Permission to use/modify/copy for any purpose is hereby granted.
 Absolutely no warranties.
 Modify by Wei-Chung Hwang, OCT 15, 1992."
 '(
  ("." . quail-next-candidate-block)
  (">" . quail-next-candidate-block)
  ("," . quail-prev-candidate-block)
  ("<" . quail-prev-candidate-block)
  (" " . quail-select-current)
  )
 nil t)

;; #
(qd "\040"	"$(0!!(B")
(qd "!"	"$(0!*(B")
(qd "\""	"$(0!q(B")
(qd "\043"	"$(0!l(B")
(qd "$"	"$(0"l(B")
(qd "%"	"$(0"h(B")
(qd "&"	"$(0!m(B")
(qd "'"	"$(0!k(B")
(qd "("	"$(0!>(B")
(qd ")"	"$(0!?(B")
(qd "*"	"$(0"/(B")
(qd "+"	"$(0"0(B")
(qd ","	"$(0!"(B")
(qd "-"	"$(0"1(B")
(qd "."	"$(0!%(B")
(qd "/"	"$(0"_(B")
(qd "0"	"$(0#O(B")
(qd "1"	"$(0#P(B")
(qd "2"	"$(0#Q(B")
(qd "3"	"$(0#R(B")
(qd "4"	"$(0#S(B")
(qd "5"	"$(0#T(B")
(qd "6"	"$(0#U(B")
(qd "7"	"$(0#V(B")
(qd "8"	"$(0#W(B")
(qd "9"	"$(0#X(B")
(qd ":"	"$(0!((B")
(qd ";"	"$(0!'(B")
(qd "<"	"$(0!R(B")
(qd "="	"$(0"8(B")
(qd ">"	"$(0!S(B")
(qd "?"	"$(0!)(B")
(qd "@"	"$(0"i(B")
(qd "A"	"$(0#o(B")
(qd "B"	"$(0#p(B")
(qd "C"	"$(0#q(B")
(qd "D"	"$(0#r(B")
(qd "E"	"$(0#s(B")
(qd "F"	"$(0#t(B")
(qd "G"	"$(0#u(B")
(qd "H"	"$(0#v(B")
(qd "I"	"$(0#w(B")
(qd "J"	"$(0#x(B")
(qd "K"	"$(0#y(B")
(qd "L"	"$(0#z(B")
(qd "M"	"$(0#{(B")
(qd "N"	"$(0#|(B")
(qd "O"	"$(0#}(B")
(qd "P"	"$(0#~(B")
(qd "Q"	"$(0$!(B")
(qd "R"	"$(0$"(B")
(qd "S"	"$(0$#(B")
(qd "T"	"$(0$$(B")
(qd "U"	"$(0$%(B")
(qd "V"	"$(0$&(B")
(qd "W"	"$(0$'(B")
(qd "X"	"$(0$((B")
(qd "Y"	"$(0$)(B")
(qd "Z"	"$(0$*(B")
(qd "["	"$(0!J(B")
(qd "\134"	"$(0"`(B")
(qd "]"	"$(0!K(B")
(qd "^"	"$(0!T(B")
(qd "_"	"$(0!;(B")
(qd "`"	"$(0!j(B")
(qd "a"	"$(0$+(B")
(qd "b"	"$(0$,(B")
(qd "c"	"$(0$-(B")
(qd "d"	"$(0$.(B")
(qd "e"	"$(0$/(B")
(qd "f"	"$(0$0(B")
(qd "g"	"$(0$1(B")
(qd "h"	"$(0$2(B")
(qd "i"	"$(0$3(B")
(qd "j"	"$(0$4(B")
(qd "k"	"$(0$5(B")
(qd "l"	"$(0$6(B")
(qd "m"	"$(0$7(B")
(qd "n"	"$(0$8(B")
(qd "o"	"$(0$9(B")
(qd "p"	"$(0$:(B")
(qd "q"	"$(0$;(B")
(qd "r"	"$(0$<(B")
(qd "s"	"$(0$=(B")
(qd "t"	"$(0$>(B")
(qd "u"	"$(0$?(B")
(qd "v"	"$(0$@(B")
(qd "w"	"$(0$A(B")
(qd "x"	"$(0$B(B")
(qd "y"	"$(0$C(B")
(qd "z"	"$(0$D(B")
(qd "{"	"$(0!B(B")
(qd "|"	"$(0!6(B")
(qd "}"	"$(0!C(B")
(qd "~"	"$(0"D(B")

(quail-setup-current-package)
