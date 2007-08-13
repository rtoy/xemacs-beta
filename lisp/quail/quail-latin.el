;; Quail packages for inputting various European characters.
;; Copyright (C) 1992 Free Software Foundation, Inc.
;; This file is part of Mule (MULtilingual Enhancement of GNU Emacs).

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

;;; 92.7.3   created for Mule Ver.0.9.5 by Takahashi N. <ntakahas@etl.go.jp>
;;;          The supported languages are: danish, esperanto, finnish,
;;;          french, german, italian, norwegian, scandinavian,
;;;          spanish, and swedish.
;;;          Also including "latin-1 package" for general purpose, and
;;;          "scandinavian package" for those who want to use the
;;;          nordic languages simultaneously.
;;; 92.7.10  modified by Takahashi N. <ntakahas@etl.go.jp>
;;;          packages for icelandic and turkish are added.
;;; 92.7.14  modified by Takahashi N. <ntakahas@etl.go.jp>
;;;          latin-2 package added.
;;; 92.10.22 modified by Takahashi N. <ntakahas@etl.go.jp>
;;;          latin-3, latin-4, latin-5 package added.
;;; 92.12.29 modified by Takahashi N. <ntakahas@etl.go.jp>
;;;          british package added.
;;; 93.1.14  modified by Takahashi N. <ntakahas@etl.go.jp>
;;;          esperanto package changed.

(require 'quail)

(quail-define-package "latin-1" "LATIN-1" t "Latin-1 encoding.

    effect   | postfix | examples
 ------------+---------+----------
    acute    |    '    | a' -> ,Aa(B
    grave    |    `    | a` -> ,A`(B
  circumflex |    ^    | a^ -> ,Ab(B
  diaeresis  |    \"    | a\" -> ,Ad(B
    tilde    |    ~    | a~ -> ,Ac(B
   cedilla   |    ,    | c, -> ,Ag(B
   nordic    |    /    | d/ -> ,Ap(B   t/ -> ,A~(B   a/ -> ,Ae(B   e/ -> ,Af(B   o/ -> ,Ax(B
   special   |   /<>   | s/ -> ,A_(B   ?/ -> ,A?(B   !/ -> ,A!(B   << -> ,A+(B   >> -> ,A;(B
" nil t)

(qd "A`" ?,A@(B)
(qd "A'" ?,AA(B)
(qd "A^" ?,AB(B)
(qd "A~" ?,AC(B)
(qd "A\"" ?,AD(B)
(qd "A/" ?,AE(B)
(qd "a`" ?,A`(B)
(qd "a'" ?,Aa(B)
(qd "a^" ?,Ab(B)
(qd "a~" ?,Ac(B)
(qd "a\"" ?,Ad(B)
(qd "a/" ?,Ae(B)

(qd "E`" ?,AH(B)
(qd "E'" ?,AI(B)
(qd "E^" ?,AJ(B)
(qd "E\"" ?,AK(B)
(qd "E/" ?,AF(B)
(qd "e`" ?,Ah(B)
(qd "e'" ?,Ai(B)
(qd "e^" ?,Aj(B)
(qd "e\"" ?,Ak(B)
(qd "e/" ?,Af(B)

(qd "I`" ?,AL(B)
(qd "I'" ?,AM(B)
(qd "I^" ?,AN(B)
(qd "I\"" ?,AO(B)
(qd "i`" ?,Al(B)
(qd "i'" ?,Am(B)
(qd "i^" ?,An(B)
(qd "i\"" ?,Ao(B)

(qd "O`" ?,AR(B)
(qd "O'" ?,AS(B)
(qd "O^" ?,AT(B)
(qd "O~" ?,AU(B)
(qd "O\"" ?,AV(B)
(qd "O/" ?,AX(B)
(qd "o`" ?,Ar(B)
(qd "o'" ?,As(B)
(qd "o^" ?,At(B)
(qd "o~" ?,Au(B)
(qd "o\"" ?,Av(B)
(qd "o/" ?,Ax(B)

(qd "U`" ?,AY(B)
(qd "U'" ?,AZ(B)
(qd "U^" ?,A[(B)
(qd "U\"" ?,A\(B)
(qd "u`" ?,Ay(B)
(qd "u'" ?,Az(B)
(qd "u^" ?,A{(B)
(qd "u\"" ?,A|(B)

(qd "Y'" ?,A](B)
(qd "y'" ?,A}(B)

(qd "D/" ?,AP(B)
(qd "d/" ?,Ap(B)

(qd "T/" ?,A^(B)
(qd "t/" ?,A~(B)

(qd "s/" ?,A_(B)

(qd "C," ?,AG(B)
(qd "c," ?,Ag(B)

(qd "N~" ?,AQ(B)
(qd "n~" ?,Aq(B)

(qd "?/" ?,A?(B)
(qd "!/" ?,A!(B)
(qd "<<" ?,A+(B)
(qd ">>" ?,A;(B)

(quail-setup-current-package)

(quail-define-package "latin-2" "LATIN-2" t "Latin-2 encoding.

    effect   | postfix | examples
 ------------+---------+----------
    acute    |    '    | a' -> ,Ba(B
    ogonek   |    ,    | a, -> ,B1(B
  diaeresis  |    \"    | a\" -> ,Bd(B
  circumflex |    ^    | a^ -> ,Bb(B
    breve    |    ~    | a~ -> ,Bc(B
   cedilla   |    ,    | c, -> ,Bg(B
    caron    |    ~    | c~ -> ,Bh(B
  dbl. acute |    ''   | o'' -> ,Bu(B
     ring    |    .    | u. -> ,By(B
     dot     |    .    | z. -> ,B?(B
    stroke   |    /    | d/ -> ,Bp(B
   special   |    /    | s/ -> ,B_(B
" nil t)

(qd "A'" ?,BA(B)
(qd "A," ?,B!(B)
(qd "A\"" ?,BD(B)
(qd "A^" ?,BB(B)
(qd "A~" ?,BC(B)
(qd "C'" ?,BF(B)
(qd "C," ?,BG(B)
(qd "C~" ?,BH(B)
(qd "D/" ?,BP(B)
(qd "D~" ?,BO(B)
(qd "E'" ?,BI(B)
(qd "E," ?,BJ(B)
(qd "E\"" ?,BK(B)
(qd "E~" ?,BL(B)
(qd "I'" ?,BM(B)
(qd "I^" ?,BN(B)
(qd "L'" ?,BE(B)
(qd "L/" ?,B#(B)
(qd "L~" ?,B%(B)
(qd "N'" ?,BQ(B)
(qd "N~" ?,BR(B)
(qd "O'" ?,BS(B)
(qd "O''" '(",BU(B" ",BS(B'"))
(qd "O\"" ?,BV(B)
(qd "O^" ?,BT(B)
(qd "R'" ?,B@(B)
(qd "R~" ?,BX(B)
(qd "S'" ?,B&(B)
(qd "S," ?,B*(B)
(qd "S~" ?,B)(B)
(qd "T," ?,B^(B)
(qd "T~" ?,B+(B)
(qd "U'" ?,BZ(B)
(qd "U''" '(",B[(B" ",BZ(B'"))
(qd "U\"" ?,B\(B)
(qd "U." ?,BY(B)
(qd "Y'" ?,B](B)
(qd "Z'" ?,B,(B)
(qd "Z." ?,B/(B)
(qd "Z~" ?,B.(B)
(qd "a'" ?,Ba(B)
(qd "a," ?,B1(B)
(qd "a\"" ?,Bd(B)
(qd "a^" ?,Bb(B)
(qd "a~" ?,Bc(B)
(qd "c'" ?,Bf(B)
(qd "c," ?,Bg(B)
(qd "c~" ?,Bh(B)
(qd "d/" ?,Bp(B)
(qd "d~" ?,Bo(B)
(qd "e'" ?,Bi(B)
(qd "e," ?,Bj(B)
(qd "e\"" ?,Bk(B)
(qd "e~" ?,Bl(B)
(qd "i'" ?,Bm(B)
(qd "i^" ?,Bn(B)
(qd "l'" ?,Be(B)
(qd "l/" ?,B3(B)
(qd "l~" ?,B5(B)
(qd "n'" ?,Bq(B)
(qd "n~" ?,Br(B)
(qd "o'" ?,Bs(B)
(qd "o''" '(",Bu(B" ",Bs(B'"))
(qd "o\"" ?,Bv(B)
(qd "o^" ?,Bt(B)
(qd "r'" ?,B`(B)
(qd "r~" ?,Bx(B)
(qd "s'" ?,B6(B)
(qd "s," ?,B:(B)
(qd "s/" ?,B_(B)
(qd "s~" ?,B9(B)
(qd "t," ?,B~(B)
(qd "t~" ?,B;(B)
(qd "u'" ?,Bz(B)
(qd "u''" '(",B{(B" ",Bz(B'"))
(qd "u\"" ?,B|(B)
(qd "u." ?,By(B)
(qd "y'" ?,B}(B)
(qd "z'" ?,B<(B)
(qd "z." ?,B?(B)
(qd "z~" ?,B>(B)

(quail-setup-current-package)

(quail-define-package "latin-3" "LATIN-3" t "Latin-3 encoding.

    effect   | postfix | examples
 ------------+---------+----------
    acute    |    '    | a' -> ,Ca(B
    grave    |    `    | a` -> ,C`(B
  circumflex |    ^    | a^ -> ,Cb(B
  diaeresis  |    \"    | a\" -> ,Cd(B
     dot     |    .    | c. -> ,Ce(B   i. -> ,C9(B   I. -> ,C)(B
   cedilla   |    ,    | c, -> ,Cg(B
    breve    |    ~    | g~ -> ,C;(B
    tilde    |    ~    | n~ -> ,Cq(B
   stroke    |    /    | h/ -> ,C1(B
   special   |    /    | s/ -> ,C_(B
" nil t)

(qd "A`" ?,C@(B)
(qd "A'" ?,CA(B)
(qd "A^" ?,CB(B)
(qd "A\"" ?,CD(B)
(qd "C." ?,CE(B)
(qd "C^" ?,CF(B)
(qd "C," ?,CG(B)
(qd "E`" ?,CH(B)
(qd "E'" ?,CI(B)
(qd "E^" ?,CJ(B)
(qd "E\"" ?,CK(B)
(qd "G~" ?,C+(B)
(qd "G." ?,CU(B)
(qd "G^" ?,CX(B)
(qd "H/" ?,C!(B)
(qd "H^" ?,C&(B)
(qd "I." ?,C)(B)
(qd "I`" ?,CL(B)
(qd "I'" ?,CM(B)
(qd "I^" ?,CN(B)
(qd "I\"" ?,CO(B)
(qd "J^" ?,C,(B)
(qd "N~" ?,CQ(B)
(qd "O`" ?,CR(B)
(qd "O'" ?,CS(B)
(qd "O^" ?,CT(B)
(qd "O\"" ?,CV(B)
(qd "S," ?,C*(B)
(qd "S^" ?,C^(B)
(qd "U`" ?,CY(B)
(qd "U'" ?,CZ(B)
(qd "U^" ?,C[(B)
(qd "U\"" ?,C\(B)
(qd "U~" ?,C](B)
(qd "Z." ?,C/(B)
(qd "a`" ?,C`(B)
(qd "a'" ?,Ca(B)
(qd "a^" ?,Cb(B)
(qd "a\"" ?,Cd(B)
(qd "c." ?,Ce(B)
(qd "c^" ?,Cf(B)
(qd "c," ?,Cg(B)
(qd "e`" ?,Ch(B)
(qd "e'" ?,Ci(B)
(qd "e^" ?,Cj(B)
(qd "e\"" ?,Ck(B)
(qd "g~" ?,C;(B)
(qd "g." ?,Cu(B)
(qd "g^" ?,Cx(B)
(qd "h/" ?,C1(B)
(qd "h^" ?,C6(B)
(qd "i." ?,C9(B)
(qd "i`" ?,Cl(B)
(qd "i'" ?,Cm(B)
(qd "i^" ?,Cn(B)
(qd "i\"" ?,Co(B)
(qd "j^" ?,C<(B)
(qd "n~" ?,Cq(B)
(qd "o`" ?,Cr(B)
(qd "o'" ?,Cs(B)
(qd "o^" ?,Ct(B)
(qd "o\"" ?,Cv(B)
(qd "s," ?,C:(B)
(qd "s/" ?,C_(B)
(qd "s^" ?,C~(B)
(qd "u`" ?,Cy(B)
(qd "u'" ?,Cz(B)
(qd "u^" ?,C{(B)
(qd "u\"" ?,C|(B)
(qd "u~" ?,C}(B)
(qd "z." ?,C?(B)

(quail-setup-current-package)

(quail-define-package "latin-4" "LATIN-4" t "Latin-4 encoding.

    effect   | postfix | examples
 ------------+---------+----------
    acute    |    '    | a' -> ,Da(B
  circumflex |    ^    | a^ -> ,Db(B
  diaeresis  |    \"    | a\" -> ,Dd(B
    ogonek   |    ,    | a, -> ,D1(B
    macron   |    -    | a- -> ,D`(B
    tilde    |    ~    | a~ -> ,Dc(B
    caron    |    ~    | c~ -> ,Dh(B
     dot     |    .    | e. -> ,Dl(B
   cedilla   |    ,    | k, -> ,Ds(B   g, -> ,D;(B
   stroke    |    /    | d/ -> ,Dp(B
   nordic    |    /    | a/ -> ,De(B   e/ -> ,Df(B   o/ -> ,Dx(B
   special   |    /    | s/ -> ,D_(B   n/ -> ,D?(B   k/ -> ,D"(B
" nil t)

(qd "A," ?,D!(B)
(qd "A-" ?,D@(B)
(qd "A'" ?,DA(B)
(qd "A^" ?,DB(B)
(qd "A~" ?,DC(B)
(qd "A\"" ?,DD(B)
(qd "A/" ?,DE(B)
(qd "C~" ?,DH(B)
(qd "D/" ?,DP(B)
(qd "E/" ?,DF(B)
(qd "E-" ?,D*(B)
(qd "E'" ?,DI(B)
(qd "E," ?,DJ(B)
(qd "E\"" ?,DK(B)
(qd "E." ?,DL(B)
(qd "G," ?,D+(B)
(qd "I~" ?,D%(B)
(qd "I," ?,DG(B)
(qd "I'" ?,DM(B)
(qd "I^" ?,DN(B)
(qd "I-" ?,DO(B)
(qd "K," ?,DS(B)
(qd "L," ?,D&(B)
(qd "N/" ?,D=(B)
(qd "N," ?,DQ(B)
(qd "O-" ?,DR(B)
(qd "O^" ?,DT(B)
(qd "O~" ?,DU(B)
(qd "O\"" ?,DV(B)
(qd "O/" ?,DX(B)
(qd "R," ?,D#(B)
(qd "S~" ?,D)(B)
(qd "T/" ?,D,(B)
(qd "U," ?,DY(B)
(qd "U'" ?,DZ(B)
(qd "U^" ?,D[(B)
(qd "U\"" ?,D\(B)
(qd "U~" ?,D](B)
(qd "U-" ?,D^(B)
(qd "Z~" ?,D.(B)
(qd "a," ?,D1(B)
(qd "a-" ?,D`(B)
(qd "a'" ?,Da(B)
(qd "a^" ?,Db(B)
(qd "a~" ?,Dc(B)
(qd "a\"" ?,Dd(B)
(qd "a/" ?,De(B)
(qd "c~" ?,Dh(B)
(qd "d/" ?,Dp(B)
(qd "e/" ?,Df(B)
(qd "e-" ?,D:(B)
(qd "e'" ?,Di(B)
(qd "e," ?,Dj(B)
(qd "e\"" ?,Dk(B)
(qd "e." ?,Dl(B)
(qd "g," ?,D;(B)
(qd "i~" ?,D5(B)
(qd "i," ?,Dg(B)
(qd "i'" ?,Dm(B)
(qd "i^" ?,Dn(B)
(qd "i-" ?,Do(B)
(qd "k/" ?,D"(B)
(qd "k," ?,Ds(B)
(qd "l," ?,D6(B)
(qd "n/" ?,D?(B)
(qd "n," ?,Dq(B)
(qd "o-" ?,Dr(B)
(qd "o^" ?,Dt(B)
(qd "o~" ?,Du(B)
(qd "o\"" ?,Dv(B)
(qd "o/" ?,Dx(B)
(qd "r," ?,D3(B)
(qd "s/" ?,D_(B)
(qd "s~" ?,D9(B)
(qd "t/" ?,D<(B)
(qd "u," ?,Dy(B)
(qd "u'" ?,Dz(B)
(qd "u^" ?,D{(B)
(qd "u\"" ?,D|(B)
(qd "u~" ?,D}(B)
(qd "u-" ?,D~(B)
(qd "z~" ?,D>(B)

(quail-setup-current-package)

(quail-define-package "latin-5" "LATIN-5" t "Latin-5 encoding.

    effect   | postfix | examples
 ------------+---------+----------
    acute    |    '    | a' -> ,Ma(B
    grave    |    `    | a` -> ,M`(B
  circumflex |    ^    | a^ -> ,Mb(B
  diaeresis  |    \"    | a\" -> ,Md(B
    tilde    |    ~    | a~ -> ,Mc(B
    breve    |    ~    | g~ -> ,Mp(B
   cedilla   |    ,    | c, -> ,Mg(B
     dot     |    .    | i. -> ,M}(B   I. -> ,M](B
   nordic    |    /    | a/ -> ,Me(B   e/ -> ,Mf(B   o/ -> ,Mx(B
   special   |    /    | s/ -> ,M_(B
" nil t)

(qd "A'" ?,MA(B)
(qd "A/" ?,ME(B)
(qd "A\"" ?,MD(B)
(qd "A^" ?,MB(B)
(qd "A`" ?,M@(B)
(qd "A~" ?,MC(B)
(qd "C," ?,MG(B)
(qd "E'" ?,MI(B)
(qd "E/" ?,MF(B)
(qd "E\"" ?,MK(B)
(qd "E^" ?,MJ(B)
(qd "E`" ?,MH(B)
(qd "G~" ?,MP(B)
(qd "I'" ?,MM(B)
(qd "I." ?,M](B)
(qd "I\"" ?,MO(B)
(qd "I^" ?,MN(B)
(qd "I`" ?,ML(B)
(qd "N~" ?,MQ(B)
(qd "O'" ?,MS(B)
(qd "O/" ?,MX(B)
(qd "O\"" ?,MV(B)
(qd "O^" ?,MT(B)
(qd "O`" ?,MR(B)
(qd "O~" ?,MU(B)
(qd "S," ?,M^(B)
(qd "U'" ?,MZ(B)
(qd "U\"" ?,M\(B)
(qd "U^" ?,M[(B)
(qd "U`" ?,MY(B)
(qd "a'" ?,Ma(B)
(qd "a/" ?,Me(B)
(qd "a\"" ?,Md(B)
(qd "a^" ?,Mb(B)
(qd "a`" ?,M`(B)
(qd "a~" ?,Mc(B)
(qd "c," ?,Mg(B)
(qd "e'" ?,Mi(B)
(qd "e/" ?,Mf(B)
(qd "e\"" ?,Mk(B)
(qd "e^" ?,Mj(B)
(qd "e`" ?,Mh(B)
(qd "g~" ?,Mp(B)
(qd "i'" ?,Mm(B)
(qd "i." ?,M}(B)
(qd "i\"" ?,Mo(B)
(qd "i^" ?,Mn(B)
(qd "i`" ?,Ml(B)
(qd "n~" ?,Mq(B)
(qd "o'" ?,Ms(B)
(qd "o/" ?,Mx(B)
(qd "o\"" ?,Mv(B)
(qd "o^" ?,Mt(B)
(qd "o`" ?,Mr(B)
(qd "o~" ?,Mu(B)
(qd "s," ?,M~(B)
(qd "s/" ?,M_(B)
(qd "u'" ?,Mz(B)
(qd "u\"" ?,M|(B)
(qd "u^" ?,M{(B)
(qd "u`" ?,My(B)
(qd "y\"" ?,M(B)

(quail-setup-current-package)

(quail-define-package "danish" "DANSK" t "Latin-1 encoding.

AE -> ,AF(B
OE -> ,AX(B
AA -> ,AE(B
E' -> ,AI(B
" nil t)

(qd "AE" ?,AF(B)
(qd "ae" ?,Af(B)

(qd "OE" ?,AX(B)
(qd "oe" ?,Ax(B)

(qd "AA" ?,AE(B)
(qd "aa" ?,Ae(B)

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(quail-setup-current-package)

(quail-define-package "esperanto" "ESPERANTO" t "Latin-3 encoding.

Preceding ^ or following x will produce accented characters,
e.g. ^C -> ,CF(B, Gx -> ,CX(B.
" nil t)

(qd "Cx" ?,CF(B)
(qd "^C" ?,CF(B)
(qd "cx" ?,Cf(B)
(qd "^c" ?,Cf(B)

(qd "Gx" ?,CX(B)
(qd "^G" ?,CX(B)
(qd "gx" ?,Cx(B)
(qd "^g" ?,Cx(B)

(qd "Hx" ?,C&(B)
(qd "^H" ?,C&(B)
(qd "hx" ?,C6(B)
(qd "^h" ?,C6(B)

(qd "Jx" ?,C,(B)
(qd "^J" ?,C,(B)
(qd "jx" ?,C<(B)
(qd "^j" ?,C<(B)

(qd "Sx" ?,C^(B)
(qd "^S" ?,C^(B)
(qd "sx" ?,C~(B)
(qd "^s" ?,C~(B)

(qd "Ux" ?,C](B)
(qd "^U" ?,C](B)
(qd "~U" ?,C](B)
(qd "ux" ?,C}(B)
(qd "^u" ?,C}(B)
(qd "~u" ?,C}(B)

(quail-setup-current-package)

(quail-define-package "finnish" "SUOMI" t "Latin-1 encoding.

AE -> ,AD(B
OE -> ,AV(B
" nil t)

(qd "AE" ?,AD(B)
(qd "ae" ?,Ad(B)

(qd "OE" ?,AV(B)
(qd "oe" ?,Av(B)

(quail-setup-current-package)

(quail-define-package "french" "FRAN,AG(BAIS" t "Latin-1 encoding.

` pour grave, ' pour aigu, ^ pour circonflexe, et \" pour tr,Ai(Bma.
Par exemple A` -> ,A@(B, E' -> ,AI(B.

,AG(B, ,A+(B, et ,A;(B sont produits par C/, <<, et >>.

<e dans l'o> n'est pas disponible.
" nil t)

(qd "A`" ?,A@(B)
(qd "A^" ?,AB(B)
(qd "a`" ?,A`(B)
(qd "a^" ?,Ab(B)

(qd "E`" ?,AH(B)
(qd "E'" ?,AI(B)
(qd "E^" ?,AJ(B)
(qd "E\"" ?,AK(B)
(qd "e`" ?,Ah(B)
(qd "e'" ?,Ai(B)
(qd "e^" ?,Aj(B)
(qd "e\"" ?,Ak(B)

(qd "I^" ?,AN(B)
(qd "I\"" ?,AO(B)
(qd "i^" ?,An(B)
(qd "i\"" ?,Ao(B)

(qd "O^" ?,AT(B)
(qd "o^" ?,At(B)

(qd "U`" ?,AY(B)
(qd "U^" ?,A[(B)
(qd "U\"" ?,A\(B)
(qd "u`" ?,Ay(B)
(qd "u^" ?,A{(B)
(qd "u\"" ?,A|(B)

(qd "C/" ?,AG(B)
(qd "c/" ?,Ag(B)

(qd "<<" ?,A+(B)
(qd ">>" ?,A;(B)

(quail-setup-current-package)

(quail-define-package "german" "DEUTSCH" t "Latin-1 encoding.

AE -> ,AD(B
OE -> ,AV(B
UE -> ,A\(B
sz -> ,A_(B

,A_(B can also be input by 'ss' followed by M-n.
" nil t)

(qd "AE" ?,AD(B)
(qd "ae" ?,Ad(B)

(qd "OE" ?,AV(B)
(qd "oe" ?,Av(B)

(qd "UE" ?,A\(B)
(qd "ue" ?,A|(B)

(qd "sz" ?,A_(B)
(qd "ss" '("ss" ?,A_(B))

(quail-setup-current-package)

(quail-define-package "icelandic" ",AM(BSLENSKA" t "Latin-1 encoding.

A' -> ,AA(B
E' -> ,AI(B
I' -> ,AM(B
O' -> ,AS(B
U' -> ,AZ(B
Y' -> ,A](B
AE -> ,AF(B
OE -> ,AV(B
D/ -> ,AP(B (eth)
T/ -> ,A^(B (thorn)
" nil t)

(qd "A'" ?,AA(B)
(qd "a'" ?,Aa(B)

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(qd "I'" ?,AM(B)
(qd "i'" ?,Am(B)

(qd "O'" ?,AS(B)
(qd "o'" ?,As(B)

(qd "U'" ?,AZ(B)
(qd "u'" ?,Az(B)

(qd "Y'" ?,A](B)
(qd "y'" ?,A}(B)

(qd "AE" ?,AF(B)
(qd "ae" ?,Af(B)

(qd "OE" ?,AV(B)
(qd "oe" ?,Av(B)

(qd "D/" ?,AP(B)
(qd "d/" ?,Ap(B)

(qd "T/" ?,A^(B)
(qd "t/" ?,A~(B)

(quail-setup-current-package)

(quail-define-package "italian" "ITALIANO" t "Latin-1 encoding.

A` -> ,A@(B
E` -> ,AH(B
I` -> ,AL(B
O` -> ,AR(B
U` -> ,AY(B
" nil t)

(qd "A`" ?,A@(B)
(qd "a`" ?,A`(B)

(qd "E`" ?,AH(B)
(qd "e`" ?,Ah(B)

(qd "I`" ?,AL(B)
(qd "i`" ?,Al(B)

(qd "O`" ?,AR(B)
(qd "o`" ?,Ar(B)

(qd "U`" ?,AY(B)
(qd "u`" ?,Ay(B)

(quail-setup-current-package)

(quail-define-package "norwegian" "NORSK" t "Latin-1 encoding.

AE -> ,AF(B
OE -> ,AX(B
AA -> ,AE(B
E' -> ,AI(B
" nil t)

(qd "AE" ?,AF(B)
(qd "ae" ?,Af(B)

(qd "OE" ?,AX(B)
(qd "oe" ?,Ax(B)

(qd "AA" ?,AE(B)
(qd "aa" ?,Ae(B)

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(quail-setup-current-package)

(quail-define-package "scandinavian" "SCANDINAVIAN" t "Latin-1 encoding.

Quail package for scandinavian languages (swidish, norwegian, danish, finnish).

AE -> ,AD(B or ,AF(B
OE -> ,AV(B or ,AX(B
AA -> ,AE(B
E' -> ,AI(B.

You can toggle between ,AD(B and ,AF(B, or between OE and ,AV(B, by typing M-n
when the character is underlined.
" nil)

(qd "AE" '(?,AD(B ?,AF(B))
(qd "ae" '(?,Ad(B ?,Af(B))

(qd "AA" ?,AE(B)
(qd "aa" ?,Ae(B)

(qd "OE" '(?,AV(B ?,AX(B))
(qd "oe" '(?,Av(B ?,Ax(B))

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(quail-setup-current-package)

(quail-define-package "spanish" "ESPA,AQ(BOL" t "Latin-1 encoding.

A' -> ,AA(B
E' -> ,AI(B
I' -> ,AM(B
O' -> ,AS(B
U' -> ,AZ(B
N~ -> ,AQ(B
!/ -> ,A!(B
?/ -> ,A?(B
" nil t)

(qd "A'" ?,AA(B)
(qd "a'" ?,Aa(B)

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(qd "I'" ?,AM(B)
(qd "i'" ?,Am(B)

(qd "O'" ?,AS(B)
(qd "o'" ?,As(B)

(qd "U'" ?,AZ(B)
(qd "u'" ?,Az(B)

(qd "N~" ?,AQ(B)
(qd "n~" ?,Aq(B)

(qd "?/" ?,A?(B)
(qd "!/" ?,A!(B)

(quail-setup-current-package)

(quail-define-package "swedish" "SVENSKA" t "Latin-1 encoding.

AA -> ,AE(B
AE -> ,AD(B
OE -> ,AV(B
E' -> ,AI(B
" nil t)

(qd "AA" ?,AE(B)
(qd "aa" ?,Ae(B)

(qd "AE" ?,AD(B)
(qd "ae" ?,Ad(B)

(qd "OE" ?,AV(B)
(qd "oe" ?,Av(B)

(qd "E'" ?,AI(B)
(qd "e'" ?,Ai(B)

(quail-setup-current-package)

(quail-define-package "turkish" "T,C|(Brk,Cg(Be" t "Latin-3 encoding.

Note for I, ,C9(B, ,C)(B, i.

A^ -> ,CB(B
C/ -> ,CG(B
G^ -> ,C+(B
I  -> I
i  -> ,C9(B
I' -> ,C)(B
i' -> i
O\" -> ,CV(B
S/ -> ,C*(B
U\" -> ,C\(B
U^ -> ,C[(B
" nil t)

(qd "A^" ?,CB(B)
(qd "a^" ?,Cb(B)

(qd "C/" ?,CG(B)
(qd "c/" ?,Cg(B)

(qd "G^" ?,C+(B)
(qd "g^" ?,C;(B)

(qd "I'" ?,C)(B)
(qd "i" ?,C9(B)
(qd "i'" ?i)

(qd "O\"" ?,CV(B)
(qd "o\"" ?,Cv(B)

(qd "S/" ?,C*(B)
(qd "s/" ?,C:(B)

(qd "U\"" ?,C\(B)
(qd "u\"" ?,C|(B)
(qd "U^" ?,C[(B)
(qd "u^" ?,C{(B)

(quail-setup-current-package)

(quail-define-package "british" "BRITISH" t "Latin-1 encoding.

# is replaced by ,A#(B." nil t)

(qd "#" '(?,A#(B ?#))

(quail-setup-current-package)

;; The following are various quail packages for those who think
;; the aboves are too awkward.  Supported languages and their
;; package name are:
;;
;; French	(frnch, azerty)
;; Icelandic	(iclndc)
;; Denish	(dnsh)
;; Norwegian	(nrwgn)
;; Swedish	(swdsh)
;; Finnish	(fnnsh)
;; German	(grmn)
;; Italian	(itln)
;; Spanish	(spnsh)
;; Dvorak	(dvorak)
;;
;;; 92.12.15  created for Mule Ver.0.9.6 by Takahashi N. <ntakahas@etl.go.jp>
;;; 92.12.29  modified by Takahashi N. <ntakahas@etl.go.jp>

;;
(quail-define-package "frnch" "FRN,AG(BS" t "Latin-1 encoding.
<e dans l'o> n'est pas disponible." nil t t t t)

;; ,Aj(B1  ,Ai(B2  ,Ah(B3  ,At(B4  ,An(B5  ,Ao(B6  ,Ab(B7  ,A{(B8  ,Ay(B9  ,A`(B0  -_  ,Ak(B+  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AgG(B  ,A|(B&
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ;:  '"  \|
;;    zZ  xX  cC  vV  bB  nN  mM  ,(  .)  !?

(qd "1" ?,Aj(B)
(qd "2" ?,Ai(B)
(qd "3" ?,Ah(B)
(qd "4" ?,At(B)
(qd "5" ?,An(B)
(qd "6" ?,Ao(B)
(qd "7" ?,Ab(B)
(qd "8" ?,A{(B)
(qd "9" ?,Ay(B)
(qd "0" ?,A`(B)
(qd "-" ?-)
(qd "=" ?,Ak(B)
(qd "`" ?`)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ag(B)
(qd "]" ?,A|(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?;)
(qd "'" ?')
(qd "\\" ?\\)
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?!)

(qd "!" ?1)
(qd "@" ?2)
(qd "#" ?3)
(qd "$" ?4)
(qd "%" ?5)
(qd "^" ?6)
(qd "&" ?7)
(qd "*" ?8)
(qd "(" ?9)
(qd ")" ?0)
(qd "_" ?_)
(qd "+" ?+)
(qd "~" ?~)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AG(B)
(qd "}" ?&)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?:)
(qd "\"" ?\")
(qd "|" ?|)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?()
(qd ">" ?))
(qd "?" ??)

(quail-setup-current-package)

;;
(quail-define-package "azerty" "AZERTY" t "Latin-1 encoding.

Similaire au clavier fran,Ag(Bais de SUN.
pr,Ai(Bfixes:  ^ pour circonflexe,  ,A((B pour tr,Ai(Bma.
<e dans l'o> n'est pas disponible." nil t t t t)

;; &1  ,Ai(B2  "3  '4  (5  ,A'(B6  ,Ah(B7  !8  ,Ag(B9  ,A`(B0  ),A0(B -_  @~
;;  aA  zZ  eE  rR  tT  yY  uU  iI  oO  pP  ^,A((B  `$
;;   qQ  sS  dD  fF  gG  hH  jJ  kK  lL  mM  ,Ay(B%  *|
;;    wW  xX  cC  vV  bB  nN  ,?  ;.  :/  =+

(qd "1" ?&)
(qd "2" ?,Ai(B)
(qd "3" ?\")
(qd "4" ?')
(qd "5" ?()
(qd "6" ?,A'(B)
(qd "7" ?,Ah(B)
(qd "8" ?!)
(qd "9" ?,Ag(B)
(qd "0" ?,A`(B)
(qd "-" ?))
(qd "=" ?-)
(qd "`" ?@)
(qd "q" ?a)
(qd "w" ?z)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?^)
(qd "]" ?`)
(qd "a" ?q)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?m)
(qd "'" ?,Ay(B)
(qd "\\" ?*)
(qd "z" ?w)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?,)
(qd "," ?;)
(qd "." ?:)
(qd "/" ?=)

(qd "!" ?1)
(qd "@" ?2)
(qd "#" ?3)
(qd "$" ?4)
(qd "%" ?5)
(qd "^" ?6)
(qd "&" ?7)
(qd "*" ?8)
(qd "(" ?9)
(qd ")" ?0)
(qd "_" ?,A0(B)
(qd "+" ?_)
(qd "~" ?~)
(qd "Q" ?A)
(qd "W" ?Z)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,A((B)
(qd "}" ?$)
(qd "A" ?Q)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?M)
(qd "\"" ?%)
(qd "|" ?|)
(qd "Z" ?W)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ??)
(qd "<" ?.)
(qd ">" ?/)
(qd "?" ?+)

(qd "[q" ?,Ab(B)
(qd "[e" ?,Aj(B)
(qd "[i" ?,An(B)
(qd "[o" ?,At(B)
(qd "[u" ?,A{(B)

(qd "{e" ?,Ak(B)
(qd "{i" ?,Ao(B)
(qd "{u" ?,A|(B)

(quail-setup-current-package)

;;
(quail-define-package "iclndc" ",AM(BSLNSK" t "Latin-1 encoding.

Dead accent is right to ,Af(B." nil t t t t)

;; 1!  2"  3#  4$  5%  6^  7&  8*  9(  0)  ,AvV(B  -_  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,ApP(B  '?
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AfF(B  ,A44(B  +*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  ,A~^(B


(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?,Av(B)
(qd "=" ?-)
(qd "`" ?`)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ap(B)
(qd "]" ?')
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Af(B)
(qd "'" ?,A4(B)
(qd "\\" ?+)
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?,A~(B)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?#)
(qd "$" ?$)
(qd "%" ?%)
(qd "^" ?^)
(qd "&" ?&)
(qd "*" ?*)
(qd "(" ?()
(qd ")" ?))
(qd "_" ?,AV(B)
(qd "+" ?_)
(qd "~" ?~)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AP(B)
(qd "}" ??)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AF(B)
(qd "\"" ?,A4(B)
(qd "|" ?*)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?,A^(B)

(qd "'a" ?,Aa(B)
(qd "'e" ?,Ai(B)
(qd "'i" ?,Am(B)
(qd "'o" ?,As(B)
(qd "'u" ?,Az(B)
(qd "'y" ?,A}(B)
(qd "'A" ?,AA(B)
(qd "'E" ?,AI(B)
(qd "'I" ?,AM(B)
(qd "'O" ?,AS(B)
(qd "'U" ?,AZ(B)
(qd "'Y" ?,A](B)

(quail-setup-current-package)

;;
(quail-define-package "dnsh" "DNSK" t "Latin-1 encoding.

Simulates SUN Danish keyboard." nil t t t t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A='(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AfF(B  ,AxX(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?+)
(qd "=" ?,A=(B)
(qd "`" ?~)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ae(B)
(qd "]" ?,Ai(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Af(B)
(qd "'" ?,Ax(B)
(qd "\\" ?')
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?#)
(qd "$" ?,A$(B)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?,A'(B)
(qd "~" ?^)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AE(B)
(qd "}" ?,AI(B)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AF(B)
(qd "\"" ?,AX(B)
(qd "|" ?*)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "nrwgn" "NRSK" t "Latin-1 encoding.

Simulates SUN Norwegian keyboard." nil t t t t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  |,A'(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AxX(B  ,AfF(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  '?

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?+)
(qd "=" ?|)
(qd "`" ?~)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ae(B)
(qd "]" ?,Ai(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Ax(B)
(qd "'" ?,Af(B)
(qd "\\" ?')
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?#)
(qd "$" ?,A$(B)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?,A'(B)
(qd "~" ?^)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AE(B)
(qd "}" ?,AI(B)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AX(B)
(qd "\"" ?,AF(B)
(qd "|" ?*)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "swdsh" "SVNSK" t "Latin-1 encoding.

Simulates SUN Swedish/Finnish keyboard." nil t t t t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A'=(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?+)
(qd "=" ?,A'(B)
(qd "`" ?~)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ae(B)
(qd "]" ?,Ai(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Av(B)
(qd "'" ?,Ad(B)
(qd "\\" ?')
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?#)
(qd "$" ?,A$(B)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?,A=(B)
(qd "~" ?^)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AE(B)
(qd "}" ?,AI(B)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AV(B)
(qd "\"" ?,AD(B)
(qd "|" ?*)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "fnnsh" "SM" t "Latin-1 encoding.

Simulates SUN Finnish/Swedish keyboard." nil t t t t)

;; 1!  2"  3#  4,A$(B  5%  6&  7/  8(  9)  0=  +?  ,A'=(B  ~^
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AeE(B  ,AiI(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  '*
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?+)
(qd "=" ?,A'(B)
(qd "`" ?~)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ae(B)
(qd "]" ?,Ai(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Av(B)
(qd "'" ?,Ad(B)
(qd "\\" ?')
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?#)
(qd "$" ?,A$(B)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?,A=(B)
(qd "~" ?^)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AE(B)
(qd "}" ?,AI(B)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AV(B)
(qd "\"" ?,AD(B)
(qd "|" ?*)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "grmn" "DTSCH" t "Latin-1 encoding.

Simulates SUN German keyboard." nil t t t t)

;; 1!  2"  3,A'(B  4$  5%  6&  7/  8(  9)  0=  ,A_(B?  [{  ]}
;;  qQ  wW  eE  rR  tT  zZ  uU  iI  oO  pP  ,A|\(B  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AvV(B  ,AdD(B  #^
;;    yY  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?,A_(B)
(qd "=" ?[)
(qd "`" ?])
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?z)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,A|(B)
(qd "]" ?+)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Av(B)
(qd "'" ?,Ad(B)
(qd "\\" ?#)
(qd "z" ?y)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?,A'(B)
(qd "$" ?$)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?{)
(qd "~" ?})
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Z)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,A\(B)
(qd "}" ?*)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AV(B)
(qd "\"" ?,AD(B)
(qd "|" ?^)
(qd "Z" ?Y)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "itln" "ITLN" t "Latin-1 encoding.

Simulates SUN Italian keyboard." nil t t t t)

;; 1!  2"  3,A#(B  4$  5%  6&  7/  8(  9)  0=  '?  ,Al(B^  `~
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,Ahi(B  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,Arg(B  ,A`0(B  ,Ay'(B
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?')
(qd "=" ?,Al(B)
(qd "`" ?`)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ah(B)
(qd "]" ?+)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Ar(B)
(qd "'" ?,A`(B)
(qd "\\" ?,Ay(B)
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?,A#(B)
(qd "$" ?$)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?^)
(qd "~" ?~)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,Ai(B)
(qd "}" ?*)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,Ag(B)
(qd "\"" ?,A0(B)
(qd "|" ?,A'(B)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "spnsh" "SPNSH" t "Latin-1 encoding." nil t t t t)

;; 1!  2"  3,A7(B  4$  5%  6&  7/  8(  9)  0=  '?  ,A!?(B  ,AmM(B
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  ,AiI(B  ,AsS(B
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  ,AqQ(B  ,AaA(B  ,AzZ(B
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?')
(qd "=" ?,A!(B)
(qd "`" ?,Am(B)
(qd "q" ?q)
(qd "w" ?w)
(qd "e" ?e)
(qd "r" ?r)
(qd "t" ?t)
(qd "y" ?y)
(qd "u" ?u)
(qd "i" ?i)
(qd "o" ?o)
(qd "p" ?p)
(qd "[" ?,Ai(B)
(qd "]" ?,As(B)
(qd "a" ?a)
(qd "s" ?s)
(qd "d" ?d)
(qd "f" ?f)
(qd "g" ?g)
(qd "h" ?h)
(qd "j" ?j)
(qd "k" ?k)
(qd "l" ?l)
(qd ";" ?,Aq(B)
(qd "'" ?,Aa(B)
(qd "\\" ?,Az(B)
(qd "z" ?z)
(qd "x" ?x)
(qd "c" ?c)
(qd "v" ?v)
(qd "b" ?b)
(qd "n" ?n)
(qd "m" ?m)
(qd "," ?,)
(qd "." ?.)
(qd "/" ?-)

(qd "!" ?!)
(qd "@" ?\")
(qd "#" ?,A7(B)
(qd "$" ?$)
(qd "%" ?%)
(qd "^" ?&)
(qd "&" ?/)
(qd "*" ?()
(qd "(" ?))
(qd ")" ?=)
(qd "_" ??)
(qd "+" ?,A?(B)
(qd "~" ?,AM(B)
(qd "Q" ?Q)
(qd "W" ?W)
(qd "E" ?E)
(qd "R" ?R)
(qd "T" ?T)
(qd "Y" ?Y)
(qd "U" ?U)
(qd "I" ?I)
(qd "O" ?O)
(qd "P" ?P)
(qd "{" ?,AI(B)
(qd "}" ?,AS(B)
(qd "A" ?A)
(qd "S" ?S)
(qd "D" ?D)
(qd "F" ?F)
(qd "G" ?G)
(qd "H" ?H)
(qd "J" ?J)
(qd "K" ?K)
(qd "L" ?L)
(qd ":" ?,AQ(B)
(qd "\"" ?,AA(B)
(qd "|" ?,AZ(B)
(qd "Z" ?Z)
(qd "X" ?X)
(qd "C" ?C)
(qd "V" ?V)
(qd "B" ?B)
(qd "N" ?N)
(qd "M" ?M)
(qd "<" ?;)
(qd ">" ?:)
(qd "?" ?_)

(quail-setup-current-package)

;;
(quail-define-package "dvorak" "DVORAK" t "Latin-1 encoding." nil t t t t)

;; 1!  2@  3#  4$  5%  6^  7&  8*  9(  0)  [{  ]}  `~
;;  '"  ,<  .>  pP  yY  fF  gG  cC  rR  lL  /?  =+
;;   aA  oO  eE  uU  iI  dD  hH  tT  nN  sS  -_  \|
;;    ;:  qQ  jJ  kK  xX  bB  mM  wW  vV  zZ

(qd "1" ?1)
(qd "2" ?2)
(qd "3" ?3)
(qd "4" ?4)
(qd "5" ?5)
(qd "6" ?6)
(qd "7" ?7)
(qd "8" ?8)
(qd "9" ?9)
(qd "0" ?0)
(qd "-" ?[)
(qd "=" ?])
(qd "`" ?`)
(qd "q" ?')
(qd "w" ?,)
(qd "e" ?.)
(qd "r" ?p)
(qd "t" ?y)
(qd "y" ?f)
(qd "u" ?g)
(qd "i" ?c)
(qd "o" ?r)
(qd "p" ?l)
(qd "[" ?/)
(qd "]" ?=)
(qd "a" ?a)
(qd "s" ?o)
(qd "d" ?e)
(qd "f" ?u)
(qd "g" ?i)
(qd "h" ?d)
(qd "j" ?h)
(qd "k" ?t)
(qd "l" ?n)
(qd ";" ?s)
(qd "'" ?-)
(qd "\\" ?\\)
(qd "z" ?;)
(qd "x" ?q)
(qd "c" ?j)
(qd "v" ?k)
(qd "b" ?x)
(qd "n" ?b)
(qd "m" ?m)
(qd "," ?w)
(qd "." ?v)
(qd "/" ?z)

(qd "!" ?!)
(qd "@" ?@)
(qd "#" ?#)
(qd "$" ?$)
(qd "%" ?%)
(qd "^" ?^)
(qd "&" ?&)
(qd "*" ?*)
(qd "(" ?()
(qd ")" ?))
(qd "_" ?{)
(qd "+" ?})
(qd "~" ?~)
(qd "Q" ?\")
(qd "W" ?<)
(qd "E" ?>)
(qd "R" ?P)
(qd "T" ?Y)
(qd "Y" ?F)
(qd "U" ?G)
(qd "I" ?C)
(qd "O" ?R)
(qd "P" ?L)
(qd "{" ??)
(qd "}" ?+)
(qd "A" ?A)
(qd "S" ?O)
(qd "D" ?E)
(qd "F" ?U)
(qd "G" ?I)
(qd "H" ?D)
(qd "J" ?H)
(qd "K" ?T)
(qd "L" ?N)
(qd ":" ?S)
(qd "\"" ?_)
(qd "|" ?|)
(qd "Z" ?:)
(qd "X" ?Q)
(qd "C" ?J)
(qd "V" ?K)
(qd "B" ?X)
(qd "N" ?B)
(qd "M" ?M)
(qd "<" ?W)
(qd ">" ?V)
(qd "?" ?Z)

(quail-setup-current-package)
