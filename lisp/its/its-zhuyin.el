;; Basic ZhuYin Translation Table for Egg+Takana+cWnn
;; Coded by Hiroshi Kuribayashi (kuri@nff.ncl.omron.co.jp)

;; (0A(B!  (0B(B"  (0C(B#  (0D(B$  5%  6&  (0Q(B'  (0b(B(  (0c(B)  (0d(B   (0e$A!*(B (0f(B~  \|
;;   (0_(B   (0](B   (0g(B   (0\(B   (0J(B   (0a(B   (0i(B   (0^(B   (0[(B   (0F(B   @`  $A!0!.(B
;;     (0Z(B   (0Y(B    (0I(B   (0H(B  (0P(B   (0O(B   (0V(B   (0N(B   (0L(B   (0W$A#;(B (0X$A#:(B $A!1!/(B
;;       (0`(B   (0h(B   (0R(B   (0M(B   (0E(B   (0K(B  (0G(B    (0S$A#,(B (0T$A!#(B (0U$A#?(B $A!"(B

(its-define-mode "zhuyin" "$AW"(B" t)

(let ((its:make-terminal-state 'its:make-terminal-state-hangul))

(its-defrule "b"	"(0E(B")	;;; B
(its-defrule "p"	"(0F(B")	;;; P
(its-defrule "m"	"(0G(B")	;;; M
(its-defrule "f"	"(0H(B")	;;; F

(its-defrule "d"	"(0I(B")	;;; D
(its-defrule "t"	"(0J(B")	;;; T
(its-defrule "n"	"(0K(B")	;;; N
(its-defrule "l"	"(0L(B")	;;; L

(its-defrule "v"	"(0M(B")	;;; G
(its-defrule "k"	"(0N(B")	;;; K
(its-defrule "h"	"(0O(B")	;;; H

(its-defrule "g"	"(0P(B")	;;; J
(its-defrule "7"	"(0Q(B")	;;; Q
(its-defrule "c"	"(0R(B")	;;; X

(its-defrule ","	"(0S(B")	;;; Zh
(its-defrule "."	"(0T(B")	;;; Ch
(its-defrule "/"	"(0U(B")	;;; Sh
(its-defrule "j"	"(0V(B")	;;; R

(its-defrule ";"	"(0W(B")	;;; Z
(its-defrule ":"	"(0X(B")	;;; C
(its-defrule "s"	"(0Y(B")	;;; S

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(its-defrule "a"	"(0Z@(B")	;;; a
(its-defrule "a1"	"(0ZA(B")
(its-defrule "a2"	"(0ZB(B")
(its-defrule "a3"	"(0ZC(B")
(its-defrule "a4"	"(0ZD(B")

(its-defrule "i"	"(0^@(B")	;;; ai
(its-defrule "i1"	"(0^A(B")
(its-defrule "i2"	"(0^B(B")
(its-defrule "i3"	"(0^C(B")
(its-defrule "i4"	"(0^D(B")

(its-defrule "8"	"(0b@(B")	;;; an
(its-defrule "81"	"(0bA(B")
(its-defrule "82"	"(0bB(B")
(its-defrule "83"	"(0bC(B")
(its-defrule "84"	"(0bD(B")

(its-defrule "0"	"(0d@(B")	;;; ang
(its-defrule "01"	"(0dA(B")
(its-defrule "02"	"(0dB(B")
(its-defrule "03"	"(0dC(B")
(its-defrule "04"	"(0dD(B")

(its-defrule "z"	"(0`@(B")	;;; ao
(its-defrule "z1"	"(0`A(B")
(its-defrule "z2"	"(0`B(B")
(its-defrule "z3"	"(0`C(B")
(its-defrule "z4"	"(0`D(B")

(its-defrule "r"	"(0\@(B")	;;; e
(its-defrule "r1"	"(0\A(B")
(its-defrule "r2"	"(0\B(B")
(its-defrule "r3"	"(0\C(B")
(its-defrule "r4"	"(0\D(B")

(its-defrule "w"	"(0]@(B")	;;; (0:(B
(its-defrule "w1"	"(0]A(B")
(its-defrule "w2"	"(0]B(B")
(its-defrule "w3"	"(0]C(B")
(its-defrule "w4"	"(0]D(B")

(its-defrule "q"	"(0_@(B")	;;; ei
(its-defrule "q1"	"(0_A(B")
(its-defrule "q2"	"(0_B(B")
(its-defrule "q3"	"(0_C(B")
(its-defrule "q4"	"(0_D(B")

(its-defrule "9"	"(0c@(B")	;;; en
(its-defrule "91"	"(0cA(B")
(its-defrule "92"	"(0cB(B")
(its-defrule "93"	"(0cC(B")
(its-defrule "94"	"(0cD(B")

(its-defrule "-"	"(0e@(B")	;;; eng
(its-defrule "-1"	"(0eA(B")
(its-defrule "-2"	"(0eB(B")
(its-defrule "-3"	"(0eC(B")
(its-defrule "-4"	"(0eD(B")

(its-defrule "o"	"(0[@(B")	;;; o
(its-defrule "o1"	"(0[A(B")
(its-defrule "o2"	"(0[B(B")
(its-defrule "o3"	"(0[C(B")
(its-defrule "o4"	"(0[D(B")

(its-defrule "y"	"(0a@(B")	;;; ou
(its-defrule "y1"	"(0aA(B")
(its-defrule "y2"	"(0aB(B")
(its-defrule "y3"	"(0aC(B")
(its-defrule "y4"	"(0aD(B")

(its-defrule "^"	"(0f@(B")	;;; er
(its-defrule "^1"	"(0fA(B")
(its-defrule "^2"	"(0fB(B")
(its-defrule "^3"	"(0fC(B")
(its-defrule "^4"	"(0fD(B")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(its-defrule "e"	"(0g@(B")	;;; i
(its-defrule "e1"	"(0gA(B")
(its-defrule "e2"	"(0gB(B")
(its-defrule "e3"	"(0gC(B")
(its-defrule "e4"	"(0gD(B")

(its-defrule "ea"	"(0gZ@(B")	;;; ia
(its-defrule "ea1"	"(0gZA(B")
(its-defrule "ea2"	"(0gZB(B")
(its-defrule "ea3"	"(0gZC(B")
(its-defrule "ea4"	"(0gZD(B")

(its-defrule "e8"	"(0gb@(B")	;;; ian
(its-defrule "e81"	"(0gbA(B")
(its-defrule "e82"	"(0gbB(B")
(its-defrule "e83"	"(0gbC(B")
(its-defrule "e84"	"(0gbD(B")

(its-defrule "e0"	"(0gd@(B")	;;; iang
(its-defrule "e01"	"(0gdA(B")
(its-defrule "e02"	"(0gdB(B")
(its-defrule "e03"	"(0gdC(B")
(its-defrule "e04"	"(0gdD(B")

(its-defrule "ez"	"(0g`@(B")	;;; iao
(its-defrule "ez1"	"(0g`A(B")
(its-defrule "ez2"	"(0g`B(B")
(its-defrule "ez3"	"(0g`C(B")
(its-defrule "ez4"	"(0g`D(B")

(its-defrule "ew"	"(0g]@(B")	;;; ie
(its-defrule "ew1"	"(0g]A(B")
(its-defrule "ew2"	"(0g]B(B")
(its-defrule "ew3"	"(0g]C(B")
(its-defrule "ew4"	"(0g]D(B")

(its-defrule "e9"	"(0gc@(B")	;;; in
(its-defrule "e91"	"(0gcA(B")
(its-defrule "e92"	"(0gcB(B")
(its-defrule "e93"	"(0gcC(B")
(its-defrule "e94"	"(0gcD(B")

(its-defrule "e-"	"(0ge@(B")	;;; ing
(its-defrule "e-1"	"(0geA(B")
(its-defrule "e-2"	"(0geB(B")
(its-defrule "e-3"	"(0geC(B")
(its-defrule "e-4"	"(0geD(B")

(its-defrule "u-"	"(0ie@(B")	;;; iong
(its-defrule "u-1"	"(0ieA(B")
(its-defrule "u-2"	"(0ieB(B")
(its-defrule "u-3"	"(0ieC(B")
(its-defrule "u-4"	"(0ieD(B")

(its-defrule "ey"	"(0ga@(B")	;;; iou
(its-defrule "ey1"	"(0gaA(B")
(its-defrule "ey2"	"(0gaB(B")
(its-defrule "ey3"	"(0gaC(B")
(its-defrule "ey4"	"(0gaD(B")

(its-defrule "x-"	"(0he@(B")	;;; ong
(its-defrule "x-1"	"(0heA(B")
(its-defrule "x-2"	"(0heB(B")
(its-defrule "x-3"	"(0heC(B")
(its-defrule "x-4"	"(0heD(B")

(its-defrule "x"	"(0h@(B")	;;; u
(its-defrule "x1"	"(0hA(B")
(its-defrule "x2"	"(0hB(B")
(its-defrule "x3"	"(0hC(B")
(its-defrule "x4"	"(0hD(B")

(its-defrule "xa"	"(0hZ@(B")	;;; ua
(its-defrule "xa1"	"(0hZA(B")
(its-defrule "xa2"	"(0hZB(B")
(its-defrule "xa3"	"(0hZC(B")
(its-defrule "xa4"	"(0hZD(B")

(its-defrule "xi"	"(0h^@(B")	;;; uai
(its-defrule "xi1"	"(0h^A(B")
(its-defrule "xi2"	"(0h^B(B")
(its-defrule "xi3"	"(0h^C(B")
(its-defrule "xi4"	"(0h^D(B")

(its-defrule "x8"	"(0hb@(B")	;;; uan
(its-defrule "x81"	"(0hbA(B")
(its-defrule "x82"	"(0hbB(B")
(its-defrule "x83"	"(0hbC(B")
(its-defrule "x84"	"(0hbD(B")

(its-defrule "x0"	"(0hd@(B")	;;; uang
(its-defrule "x01"	"(0hdA(B")
(its-defrule "x02"	"(0hdB(B")
(its-defrule "x03"	"(0hdC(B")
(its-defrule "x04"	"(0hdD(B")

(its-defrule "xq"	"(0h_@(B")	;;; uei
(its-defrule "xq1"	"(0h_A(B")
(its-defrule "xq2"	"(0h_B(B")
(its-defrule "xq3"	"(0h_C(B")
(its-defrule "xq4"	"(0h_D(B")

(its-defrule "x9"	"(0hc@(B")	;;; uen
(its-defrule "x91"	"(0hcA(B")
(its-defrule "x92"	"(0hcB(B")
(its-defrule "x93"	"(0hcC(B")
(its-defrule "x94"	"(0hcD(B")

(its-defrule "xo"	"(0h[@(B")	;;; uo
(its-defrule "xo1"	"(0h[A(B")
(its-defrule "xo2"	"(0h[B(B")
(its-defrule "xo3"	"(0h[C(B")
(its-defrule "xo4"	"(0h[D(B")

(its-defrule "u"	"(0i@(B")	;;; (09(B
(its-defrule "u1"	"(0iA(B")
(its-defrule "u2"	"(0iB(B")
(its-defrule "u3"	"(0iC(B")
(its-defrule "u4"	"(0iD(B")

(its-defrule "u8"	"(0ib@(B")	;;; (09(Ban
(its-defrule "u81"	"(0ibA(B")
(its-defrule "u82"	"(0ibB(B")
(its-defrule "u83"	"(0ibC(B")
(its-defrule "u84"	"(0ibD(B")

(its-defrule "uw"	"(0i]@(B")	;;; (09(Be
(its-defrule "uw1"	"(0i]A(B")
(its-defrule "uw2"	"(0i]B(B")
(its-defrule "uw3"	"(0i]C(B")
(its-defrule "uw4"	"(0i]D(B")

(its-defrule "u9"	"(0ic@(B")	;;; (09(Bn
(its-defrule "u91"	"(0icA(B")
(its-defrule "u92"	"(0icB(B")
(its-defrule "u93"	"(0icC(B")
(its-defrule "u94"	"(0icD(B")

;;(its-defrule "ei"	"(0g^@(B")	;;; iai
;;(its-defrule "ei1"	"(0g^A(B")
;;(its-defrule "ei2"	"(0g^B(B")
;;(its-defrule "ei3"	"(0g^C(B")
;;(its-defrule "ei4"	"(0g^D(B")

;;(its-defrule "eo"	"(0g[@(B")	;;; io
;;(its-defrule "eo1"	"(0g[A(B")
;;(its-defrule "eo2"	"(0g[B(B")
;;(its-defrule "eo3"	"(0g[C(B")
;;(its-defrule "eo4"	"(0g[D(B")

(its-defrule "m1"	"(0GA(B")	;;; M
(its-defrule "hm"	"(0OGA(B")	;;; Hm
(its-defrule "h@"	"(0O(B@(0A(B")	;;; Hng
(its-defrule "@1"	"@(0A(B")	;;; Ng
(its-defrule "n1"	"(0KA(B")	;;; N

(its-defrule "<"  	"$A#,(B")
(its-defrule ">"	"$A!#(B")
(its-defrule "+"	"$A#;(B")
(its-defrule "*"	"$A#:(B")
(its-defrule "?"	"$A#?(B")
(its-defrule "_"	"$A!"(B")
(its-defrule "!"	"$A#!(B")
(its-defrule "["	"$A!0(B")
(its-defrule "]"	"$A!1(B")
(its-defrule "{"	"$A!.(B")
(its-defrule "}"	"$A!/(B")
(its-defrule "="	"$A!*(B")

)
