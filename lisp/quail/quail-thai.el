;; Quail packages for inputting Thai characters.
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

;;; 93.2.15  modified for Mule Ver.0.9.7.1 by K.Handa <handa@etl.go.jp>
;;;	Completely re-written.
;;; 93.8.5   modified for Mule Ver.1.1 by K.Handa <handa@etl.go.jp>
;;;	Bug in handling '0,TQi1(B' fixed.

(require 'quail)
(require 'thai)

(quail-define-package "thai" "Thai" t "TSCII encoding
	,TE(B# /,Tq(B _,Tr(B ,T@s(B ,T6t(B ,TXY(B ,TV0Qi1(B ,T$u(B ,T5v(B ,T(w(B ,T"x(B ,T*y(B ,T_o(B ,T#%(B
	 ,Tfp(B ,Td(B\" ,TS.(B ,T>1(B ,TP8(B ,TQm(B ,TUj(B ,TC3(B ,T9O(B ,TB-(B ,T:0(B ,TE(B,
	  ,T?D(B ,TK&(B ,T!/(B ,T4b(B ,T`,(B ,Tig(B ,Thk(B ,TRI(B ,TJH(B ,TG+(B ,T'F(B
	   ,T<(B( ,T;(B) ,Ta)(B ,TMN(B ,TTZ(B ,TWl(B 0,T7n1(B ,TA2(B ,TcL(B ,T=(B?

The difference from the ordinal Thai keyboard:
    ',T_(B' and ',To(B' are assigned to '\\' and '|' respectively,
    ',T#(B' and ',T%(B' are assigned to '`' and '~' respectively,
    Don't know where to assign characters ',Tz(B' and ',T{(B'." nil t t)

(eval-when-compile

(defconst quail-thai-C-map (list 'keymap (vector 'quail-thai-exact-char)))
(defconst quail-thai-CV-map (list 'keymap (vector 'quail-thai-compose)))
(defconst quail-thai-T-map (list 'keymap (vector 'quail-thai-compose)))

)

(defun quail-fetch-thai-map (ch)
  (aref (lookup-key (quail-map) "\0") ch))

(defun quail-thai-compose ()
  (interactive)
  (compose-string
   (mapconcat 'quail-fetch-thai-map quail-current-key "")))

(defun quail-thai-exact-char ()
  (interactive)
  (quail-fetch-thai-map quail-last-char))

(eval-when-compile

(defun quail-defrule-thai (ch str)
  (let ((vector-map (lookup-key (quail-map) "\0")))
    (if (null vector-map)
	(progn
	  (setq vector-map (make-vector 127 0))
	  (define-key (quail-map) "\0" vector-map)))
    (aset vector-map ch str))
  (let* ((chstr (char-to-string ch))
	 (ch-thai (string-to-char str))
	 (prop (cdr (assq ch-thai *thai-characters*))))	;93.8.5 by K.Handa
    (cond ((eq prop 'consonant)
	   (quail-defrule chstr 'quail-thai-C-prefix))
	  ((or (eq prop 'vowel-upper) (eq prop 'vowel-lower))
	   (quail-defrule chstr str)
	   (define-key quail-thai-C-map chstr 'quail-thai-CV-prefix))
	  ((eq prop 'tone)
	   (quail-defrule chstr str)
	   (define-key quail-thai-C-map chstr 'quail-thai-T-prefix)
	   (define-key quail-thai-CV-map chstr 'quail-thai-T-prefix))
	  ((eq prop 'vowel-upper-tone)
	   (quail-defrule chstr str)
	   (define-key quail-thai-C-map chstr 'quail-thai-T-prefix))
	  (t
	   (quail-defrule chstr str))))
  nil)

(put 'quail-defrule-thai 'byte-hunk-handler 'eval)

(defun quail-setup-constant (sym val))
(defun quail-setup-constant-handler (form)
  (list 'defconst
	(eval (nth 1 form))
	(list 'quote (symbol-value (eval (nth 1 form))))))
(put 'quail-setup-constant 'byte-hunk-handler 'quail-setup-constant-handler)

)

(quail-defrule-thai ?1 ",TE(B")
(quail-defrule-thai ?! "#")
(quail-defrule-thai ?2 "/")
(quail-defrule-thai ?@ ",Tq(B")
(quail-defrule-thai ?3 "_")
(quail-defrule-thai ?# ",Tr(B")
(quail-defrule-thai ?4 ",T@(B")
(quail-defrule-thai ?$ ",Ts(B")
(quail-defrule-thai ?5 ",T6(B")
(quail-defrule-thai ?% ",Tt(B")
(quail-defrule-thai ?6 ",TX(B")
(quail-defrule-thai ?^ ",TY(B")
(quail-defrule-thai ?7 ",TV(B")
(quail-defrule-thai ?& "0,TQi1(B")
(quail-defrule-thai ?8 ",T$(B")
(quail-defrule-thai ?* ",Tu(B")
(quail-defrule-thai ?9 ",T5(B")
(quail-defrule-thai ?\( ",Tv(B")
(quail-defrule-thai ?0 ",T((B")
(quail-defrule-thai ?\) ",Tw(B")
(quail-defrule-thai ?- ",T"(B")
(quail-defrule-thai ?_ ",Tx(B")
(quail-defrule-thai ?= ",T*(B")
(quail-defrule-thai ?+ ",Ty(B")
(quail-defrule-thai ?\\ ",T_(B")
(quail-defrule-thai ?| ",To(B")
(quail-defrule-thai ?` ",T#(B")
(quail-defrule-thai ?~ ",T%(B")

(quail-defrule-thai ?q ",Tf(B")
(quail-defrule-thai ?Q ",Tp(B")
(quail-defrule-thai ?w ",Td(B")
(quail-defrule-thai ?W "\"")
(quail-defrule-thai ?e ",TS(B")
(quail-defrule-thai ?E ",T.(B")
(quail-defrule-thai ?r ",T>(B")
(quail-defrule-thai ?R ",T1(B")
(quail-defrule-thai ?t ",TP(B")
(quail-defrule-thai ?T ",T8(B")
(quail-defrule-thai ?y ",TQ(B")
(quail-defrule-thai ?Y ",Tm(B")
(quail-defrule-thai ?u ",TU(B")
(quail-defrule-thai ?U ",Tj(B")
(quail-defrule-thai ?i ",TC(B")
(quail-defrule-thai ?I ",T3(B")
(quail-defrule-thai ?o ",T9(B")
(quail-defrule-thai ?O ",TO(B")
(quail-defrule-thai ?p ",TB(B")
(quail-defrule-thai ?P ",T-(B")
(quail-defrule-thai ?[ ",T:(B")
(quail-defrule-thai ?{ ",T0(B")
(quail-defrule-thai ?] ",TE(B")
(quail-defrule-thai ?} ",")

(quail-defrule-thai ?a ",T?(B")
(quail-defrule-thai ?A ",TD(B")
(quail-defrule-thai ?s ",TK(B")
(quail-defrule-thai ?S ",T&(B")
(quail-defrule-thai ?d ",T!(B")
(quail-defrule-thai ?D ",T/(B")
(quail-defrule-thai ?f ",T4(B")
(quail-defrule-thai ?F ",Tb(B")
(quail-defrule-thai ?g ",T`(B")
(quail-defrule-thai ?G ",T,(B")
(quail-defrule-thai ?h ",Ti(B")
(quail-defrule-thai ?H ",Tg(B")
(quail-defrule-thai ?j ",Th(B")
(quail-defrule-thai ?J ",Tk(B")
(quail-defrule-thai ?k ",TR(B")
(quail-defrule-thai ?K ",TI(B")
(quail-defrule-thai ?l ",TJ(B")
(quail-defrule-thai ?L ",TH(B")
(quail-defrule-thai ?\; ",TG(B")
(quail-defrule-thai ?: ",T+(B")
(quail-defrule-thai ?' ",T'(B")
(quail-defrule-thai ?\" ".")

(quail-defrule-thai ?z ",T<(B")
(quail-defrule-thai ?Z "(")
(quail-defrule-thai ?x ",T;(B")
(quail-defrule-thai ?X ")")
(quail-defrule-thai ?c ",Ta(B")
(quail-defrule-thai ?C ",T)(B")
(quail-defrule-thai ?v ",TM(B")
(quail-defrule-thai ?V ",TN(B")
(quail-defrule-thai ?b ",TT(B")
(quail-defrule-thai ?B ",TZ(B")
(quail-defrule-thai ?n ",TW(B")
(quail-defrule-thai ?N ",Tl(B")
(quail-defrule-thai ?m ",T7(B")
(quail-defrule-thai ?M ",Tn(B")
(quail-defrule-thai ?, ",TA(B")
(quail-defrule-thai ?< ",T2(B")
(quail-defrule-thai ?. ",Tc(B")
(quail-defrule-thai ?> ",TL(B")
(quail-defrule-thai ?/ ",T=(B")
(quail-defrule-thai ?\" ",TF(B")

(quail-setup-current-package)

(quail-setup-constant 'quail-thai-C-map quail-thai-C-map)
(quail-setup-constant 'quail-thai-CV-map quail-thai-CV-map)
(quail-setup-constant 'quail-thai-T-map quail-thai-T-map)

(fset 'quail-thai-C-prefix quail-thai-C-map)
(fset 'quail-thai-CV-prefix quail-thai-CV-map)
(fset 'quail-thai-T-prefix quail-thai-T-map)
