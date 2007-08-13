;; IPA package for Mule-2.0
;; Copyright (C) 1994 Free Software Foundation, Inc.
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

;; 94.05.23 Created by TAKAHASHI N. <ntakahas@etl.go.jp>

(require 'quail)

(quail-define-package "ipa" "IPA" t
"International Phonetic Alphabet for English, French, German and Italian.
Upside-down characters are obtained by a preceding slash (/)."
nil t)

(quail-defrule "i" ?,0 (B)
(quail-defrule "I" ?,0!(B)
(quail-defrule "e" ?,0"(B)
(quail-defrule "/3" ?,0#(B)
(quail-defrule "E" ?,0#(B)
(quail-defrule "ae" ?,0$(B)
(quail-defrule "a" ?,0%(B)
(quail-defrule "i-" ?,0&(B)
(quail-defrule "/e" ?,0'(B)
(quail-defrule "/a" ?,0((B)
(quail-defrule "/m" ?,0)(B)
(quail-defrule "&" ?,0*(B)
(quail-defrule "/v" ?,0+(B)
(quail-defrule "A" ?,0,(B)
(quail-defrule "o|" ?,0,(B)
(quail-defrule "y" ?,0-(B)
(quail-defrule "Y" ?,0.(B)
(quail-defrule "o/" ?,0/(B)
(quail-defrule "oe" ?,00(B)
(quail-defrule "OE" ?,01(B)
(quail-defrule "u-" ?,02(B)
(quail-defrule "o-" ?,03(B)
(quail-defrule "u" ?,04(B)
(quail-defrule "U" ?,05(B)
(quail-defrule "o" ?,06(B)
(quail-defrule "/c" ?,07(B)
(quail-defrule "/A" ?,08(B)
(quail-defrule "|o" ?,08(B)
(quail-defrule "e-" ?,0:(B)
(quail-defrule "e|" ?,0:(B)
(quail-defrule "/3~" ?,0;(B)
(quail-defrule "E~" ?,0;(B)
(quail-defrule "A~" ?,0<(B)
(quail-defrule "oe~" ?,0=(B)
(quail-defrule "/c~" ?,0>(B)
(quail-defrule "p" ?,0@(B)
(quail-defrule "b" ?,0A(B)
(quail-defrule "t" ?,0B(B)
(quail-defrule "d" ?,0C(B)
(quail-defrule "k" ?,0D(B)
(quail-defrule "g" ?,0E(B)
(quail-defrule "f" ?,0F(B)
(quail-defrule "v" ?,0G(B)
(quail-defrule "th" ?,0H(B)
(quail-defrule "dh" ?,0I(B)
(quail-defrule "s" ?,0J(B)
(quail-defrule "z" ?,0K(B)
(quail-defrule "sh" ?,0L(B)
(quail-defrule "zh" ?,0M(B)
(quail-defrule "3" ?,0M(B)
(quail-defrule "c," ?,0N(B)
(quail-defrule "x" ?,0O(B)
(quail-defrule "/R" ?,0P(B)
(quail-defrule "h" ?,0Q(B)
(quail-defrule "m" ?,0R(B)
(quail-defrule "n" ?,0S(B)
(quail-defrule "gn" ?,0T(B)
(quail-defrule "ng" ?,0U(B)
(quail-defrule "r" ?,0V(B)
(quail-defrule "R" ?,0W(B)
(quail-defrule "/r" ?,0X(B)
(quail-defrule "j" ?,0Y(B)
(quail-defrule "l" ?,0Z(B)
(quail-defrule "/y" ?,0[(B)
(quail-defrule "L" ?,0\(B)
(quail-defrule "/h" ?,0](B)
(quail-defrule "w" ?,0^(B)
(quail-defrule "M" ?,0_(B)
(quail-defrule "'" ?,0p(B)
(quail-defrule "`" ?,0q(B)
(quail-defrule ":" ?,0r(B)

(quail-setup-current-package)
