;; Copyright (C) 1998 Free Software Foundation, Inc. -*- coding: iso-8859-1 -*-
;; Copyright (C) 2010 Ben Wing.

;; Author: Martin Buchholz <martin@xemacs.org>
;; Maintainer: Martin Buchholz <martin@xemacs.org>
;; Created: 1998
;; Keywords: tests

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test basic Lisp engine functionality
;;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

(Check-Error wrong-number-of-arguments (setq setq-test-foo))
(Check-Error wrong-number-of-arguments (setq setq-test-foo 1 setq-test-bar))
(Check-Error wrong-number-of-arguments (setq-default setq-test-foo))
(Check-Error wrong-number-of-arguments (setq-default setq-test-foo 1 setq-test-bar))
(Assert (eq (setq)         nil))
(Assert (eq (setq-default) nil))
(Assert (eq (setq         setq-test-foo 42) 42))
(Assert (eq (setq-default setq-test-foo 42) 42))
(Assert (eq (setq         setq-test-foo 42 setq-test-bar 99) 99))
(Assert (eq (setq-default setq-test-foo 42 setq-test-bar 99) 99))

(macrolet ((test-setq (expected-result &rest body)
		      `(progn
			 (defun test-setq-fun () ,@body)
			 (Assert (eq ,expected-result (test-setq-fun)))
			 (byte-compile 'test-setq-fun)
			 (Assert (eq ,expected-result (test-setq-fun))))))
  (test-setq nil (setq))
  (test-setq nil (setq-default))
  (test-setq 42  (setq         test-setq-var 42))
  (test-setq 42  (setq-default test-setq-var 42))
  (test-setq 42  (setq         test-setq-bar 99 test-setq-var 42))
  (test-setq 42  (setq-default test-setq-bar 99 test-setq-var 42))
  )

(let ((my-vector [1 2 3 4])
      (my-bit-vector (bit-vector 1 0 1 0))
      (my-string "1234")
      (my-list '(1 2 3 4)))

  ;;(Assert (fooooo)) ;; Generate Other failure
  ;;(Assert (eq 1 2)) ;; Generate Assertion failure

  (dolist (sequence (list my-vector my-bit-vector my-string my-list))
    (Assert (sequencep sequence))
    (Assert (eq 4 (length sequence))))

  (dolist (array (list my-vector my-bit-vector my-string))
    (Assert (arrayp array)))

  (Assert (eq (elt my-vector 0) 1))
  (Assert (eq (elt my-bit-vector 0) 1))
  (Assert (eq (elt my-string 0) ?1))
  (Assert (eq (elt my-list 0) 1))

  (fillarray my-vector 5)
  (fillarray my-bit-vector 1)
  (fillarray my-string ?5)

  (dolist (array (list my-vector my-bit-vector))
    (Assert (eq 4 (length array))))

  (Assert (eq (elt my-vector 0) 5))
  (Assert (eq (elt my-bit-vector 0) 1))
  (Assert (eq (elt my-string 0) ?5))

  (Assert (eq (elt my-vector 3) 5))
  (Assert (eq (elt my-bit-vector 3) 1))
  (Assert (eq (elt my-string 3) ?5))

  (fillarray my-bit-vector 0)
  (Assert (eq 4 (length my-bit-vector)))
  (Assert (eq (elt my-bit-vector 2) 0))
  )

(defun make-circular-list (length)
  "Create evil emacs-crashing circular list of length LENGTH"
  (let ((circular-list
	 (make-list
	  length
	  'you-are-trapped-in-a-twisty-maze-of-cons-cells-all-alike)))
    (setcdr (last circular-list) circular-list)
    circular-list))

;;-----------------------------------------------------
;; Test `nconc'
;;-----------------------------------------------------
(defun make-list-012 () (list 0 1 2))

(Check-Error wrong-type-argument (nconc 'foo nil))

(dolist (length '(1 2 3 4 1000 2000))
  (Check-Error circular-list (nconc (make-circular-list length) 'foo))
  (Check-Error circular-list (nconc '(1 . 2) (make-circular-list length) 'foo))
  (Check-Error circular-list (nconc '(1 . 2) '(3 . 4) (make-circular-list length) 'foo)))

(Assert (eq (nconc) nil))
(Assert (eq (nconc nil) nil))
(Assert (eq (nconc nil nil) nil))
(Assert (eq (nconc nil nil nil) nil))

(let ((x (make-list-012))) (Assert (eq (nconc nil x) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x nil) x)))
(let ((x (make-list-012))) (Assert (eq (nconc nil x nil) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x (make-circular-list 3)) x)))

(Assert (equal (nconc '(1 . 2) '(3 . 4) '(5 . 6)) '(1 3 5 . 6)))

(let ((y (nconc (make-list-012) nil (list 3 4 5) nil)))
  (Assert (eq (length y) 6))
  (Assert (eq (nth 3 y) 3)))

;;-----------------------------------------------------
;; Test `last'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (last 'foo))
(Check-Error wrong-number-of-arguments (last))
(Check-Error wrong-number-of-arguments (last '(1 2) 1 1))
(Check-Error circular-list (last (make-circular-list 1)))
(Check-Error circular-list (last (make-circular-list 2000)))
(let ((x (list 0 1 2 3)))
  (Assert (eq (last nil) nil))
  (Assert (eq (last x 0) nil))
  (Assert (eq (last x  ) (cdddr x)))
  (Assert (eq (last x 1) (cdddr x)))
  (Assert (eq (last x 2) (cddr x)))
  (Assert (eq (last x 3) (cdr x)))
  (Assert (eq (last x 4) x))
  (Assert (eq (last x 9) x))
  (Assert (eq (last '(1 . 2) 0) 2))
  )

;;-----------------------------------------------------
;; Test `butlast' and `nbutlast'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (butlast  'foo))
(Check-Error wrong-type-argument (nbutlast 'foo))
(Check-Error wrong-number-of-arguments (butlast))
(Check-Error wrong-number-of-arguments (nbutlast))
(Check-Error wrong-number-of-arguments (butlast  '(1 2) 1 1))
(Check-Error wrong-number-of-arguments (nbutlast '(1 2) 1 1))
(Check-Error circular-list (butlast  (make-circular-list 1)))
(Check-Error circular-list (nbutlast (make-circular-list 1)))
(Check-Error circular-list (butlast  (make-circular-list 2000)))
(Check-Error circular-list (nbutlast (make-circular-list 2000)))

(let* ((x (list 0 1 2 3))
       (y (butlast x))
       (z (nbutlast x)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2)))
  (Assert (equal z y)))

(let* ((x (list 0 1 2 3 4))
       (y (butlast x 2))
       (z (nbutlast x 2)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2)))
  (Assert (equal z y)))

(let* ((x (list 0 1 2 3))
       (y (butlast x 0))
       (z (nbutlast x 0)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2 3)))
  (Assert (equal z y)))

(let* ((x (list* 0 1 2 3 4 5 6.0 ?7 ?8 (vector 'a 'b 'c)))
       (y (butlast x 0))
       (z (nbutlast x 0)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2 3 4 5 6.0 ?7 ?8)))
  (Assert (equal z y)))

(Assert (eq (butlast  '(x)) nil))
(Assert (eq (nbutlast '(x)) nil))
(Assert (eq (butlast  '()) nil))
(Assert (eq (nbutlast '()) nil))

(when (featurep 'bignum)
  (let* ((x (list* 0 1 2 3 4 5 6.0 ?7 ?8 (vector 'a 'b 'c)))
         (y (butlast x (* 2 most-positive-fixnum)))
         (z (nbutlast x (* 3 most-positive-fixnum))))
    (Assert (eq nil y) "checking butlast with a large bignum gives nil")
    (Assert (eq nil z) "checking nbutlast with a large bignum gives nil")
    (Check-Error wrong-type-argument
		 (nbutlast x (1- most-negative-fixnum))
                 "checking nbutlast with a negative bignum errors")))

;;-----------------------------------------------------
;; Test `copy-list'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (copy-list 'foo))
(Check-Error wrong-number-of-arguments (copy-list))
(Check-Error wrong-number-of-arguments (copy-list '(1 2) 1))
(Check-Error circular-list (copy-list (make-circular-list 1)))
(Check-Error circular-list (copy-list (make-circular-list 2000)))
(Assert (eq '() (copy-list '())))
(dolist (x '((1) (1 2) (1 2 3) (1 2 . 3)))
  (let ((y (copy-list x)))
    (Assert (and (equal x y) (not (eq x y))))))

;;-----------------------------------------------------
;; Test `ldiff'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (ldiff 'foo pi))
(Check-Error wrong-number-of-arguments (ldiff))
(Check-Error wrong-number-of-arguments (ldiff '(1 2)))
(Check-Error circular-list (ldiff (make-circular-list 1) nil))
(Check-Error circular-list (ldiff (make-circular-list 2000) nil))
(Assert (eq '() (ldiff '() pi)))
(dolist (x '((1) (1 2) (1 2 3) (1 2 . 3)))
  (let ((y (ldiff x nil)))
    (Assert (and (equal x y) (not (eq x y))))))

(let* ((vector (vector 'foo))
       (dotted `(1 2 3 ,pi 40 50 . ,vector))
       (dotted-pi `(1 2 3 . ,pi))
       without-vector without-pi)
  (Assert (equal dotted (ldiff dotted nil))
	  "checking ldiff handles dotted lists properly")
  (Assert (equal (butlast dotted 0) (ldiff dotted vector))
	  "checking ldiff discards dotted elements correctly")
  (Assert (equal (butlast dotted-pi 0) (ldiff dotted-pi (* 4 (atan 1))))
	  "checking ldiff handles float equivalence correctly"))

;;-----------------------------------------------------
;; Test `tailp'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (tailp pi 'foo))
(Check-Error wrong-number-of-arguments (tailp))
(Check-Error wrong-number-of-arguments (tailp '(1 2)))
(Check-Error circular-list (tailp nil (make-circular-list 1)))
(Check-Error circular-list (tailp nil (make-circular-list 2000)))
(Assert (null (tailp pi '()))
	"checking pi is not a tail of the list nil")
(Assert (tailp 3 '(1 2 . 3))
	"checking #'tailp works with a dotted integer.")
(Assert (tailp pi `(1 2 . ,(* 4 (atan 1))))
	"checking tailp works with non-eq dotted floats.")
(let ((list (make-list 2048 nil)))
  (Assert (tailp (nthcdr 2000 list) (nconc list list))
	  "checking #'tailp succeeds with circular LIST containing SUBLIST"))

;;-----------------------------------------------------
;; Test `endp'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (endp 'foo))
(Check-Error wrong-number-of-arguments (endp))
(Check-Error wrong-number-of-arguments (endp '(1 2) 'foo))
(Assert (endp nil) "checking nil is recognized as the end of a list")
(Assert (not (endp (list 200 200 4 0 9)))
	"checking a cons is not recognised as the end of a list")

;;-----------------------------------------------------
;; Arithmetic operations
;;-----------------------------------------------------

;; Test `+'
(Assert (eq (+ 1 1) 2))
(Assert (= (+ 1.0 1.0) 2.0))
(Assert (= (+ 1.0 3.0 0.0) 4.0))
(Assert (= (+ 1 1.0) 2.0))
(Assert (= (+ 1.0 1) 2.0))
(Assert (= (+ 1.0 1 1) 3.0))
(Assert (= (+ 1 1 1.0) 3.0))
(if (featurep 'bignum)
    (progn
      (Assert (bignump (1+ most-positive-fixnum)))
      (Assert (eq most-positive-fixnum (1- (1+ most-positive-fixnum))))
      (Assert (bignump (+ most-positive-fixnum 1)))
      (Assert (eq most-positive-fixnum (- (+ most-positive-fixnum 1) 1)))
      (Assert (= (1+ most-positive-fixnum) (- most-negative-fixnum)))
      (Assert (zerop (+ (* 3 most-negative-fixnum) (* 3 most-positive-fixnum)
			3))))
  (Assert (eq (1+ most-positive-fixnum) most-negative-fixnum))
  (Assert (eq (+ most-positive-fixnum 1) most-negative-fixnum)))

(when (featurep 'ratio)
  (let ((threefourths (read "3/4"))
	(threehalfs (read "3/2"))
	(bigpos (div (+ most-positive-fixnum 2) (1+ most-positive-fixnum)))
	(bigneg (div (+ most-positive-fixnum 2) most-negative-fixnum))
	(negone (div (1+ most-positive-fixnum) most-negative-fixnum)))
    (Assert (= negone -1))
    (Assert (= threehalfs (+ threefourths threefourths)))
    (Assert (zerop (+ bigpos bigneg)))))

;; Test `-'
(Check-Error wrong-number-of-arguments (-))
(Assert (eq (- 0) 0))
(Assert (eq (- 1) -1))
(dolist (one `(1 1.0 ?\1 ,(Int-to-Marker 1)))
  (Assert (= (+ 1 one) 2))
  (Assert (= (+ one) 1))
  (Assert (= (+ one) one))
  (Assert (= (- one) -1))
  (Assert (= (- one one) 0))
  (Assert (= (- one one one) -1))
  (Assert (= (- 0 one) -1))
  (Assert (= (- 0 one one) -2))
  (Assert (= (+ one 1) 2))
  (dolist (zero '(0 0.0 ?\0))
    (Assert (= (+ 1 zero) 1) zero)
    (Assert (= (+ zero 1) 1) zero)
    (Assert (= (- zero) zero) zero)
    (Assert (= (- zero) 0) zero)
    (Assert (= (- zero zero) 0) zero)
    (Assert (= (- zero one one) -2) zero)))

(Assert (= (- 1.5 1) .5))
(Assert (= (- 1 1.5) (- .5)))

(if (featurep 'bignum)
    (progn
      (Assert (bignump (1- most-negative-fixnum)))
      (Assert (eq most-negative-fixnum (1+ (1- most-negative-fixnum))))
      (Assert (bignump (- most-negative-fixnum 1)))
      (Assert (eq most-negative-fixnum (+ (- most-negative-fixnum 1) 1)))
      (Assert (= (1- most-negative-fixnum) (- 0 most-positive-fixnum 2)))
      (Assert (eq (- (- most-positive-fixnum most-negative-fixnum)
		     (* 2 most-positive-fixnum))
		  1)))
  (Assert (eq (1- most-negative-fixnum) most-positive-fixnum))
  (Assert (eq (- most-negative-fixnum 1) most-positive-fixnum)))

(when (featurep 'ratio)
  (let ((threefourths (read "3/4"))
	(threehalfs (read "3/2"))
	(bigpos (div (+ most-positive-fixnum 2) (1+ most-positive-fixnum)))
	(bigneg (div most-positive-fixnum most-negative-fixnum))
	(negone (div (1+ most-positive-fixnum) most-negative-fixnum)))
    (Assert (= (- negone) 1))
    (Assert (= threefourths (- threehalfs threefourths)))
    (Assert (= (- bigpos bigneg) 2))))

;; Test `/'

;; Test division by zero errors
(dolist (zero '(0 0.0 ?\0))
  (Check-Error arith-error (/ zero))
  (dolist (n1 `(42 42.0 ?\042 ,(Int-to-Marker 42)))
    (Check-Error arith-error (/ n1 zero))
    (dolist (n2 `(3 3.0 ?\03 ,(Int-to-Marker 3)))
      (Check-Error arith-error (/ n1 n2 zero)))))

;; Other tests for `/'
(Check-Error wrong-number-of-arguments (/))
(let (x)
  (Assert (= (/ (setq x 2))   0))
  (Assert (= (/ (setq x 2.0)) 0.5)))

(dolist (six '(6 6.0 ?\06))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (/ six two) three) (list six two three)))))

(dolist (three '(3 3.0 ?\03))
  (Assert (= (/ three 2.0) 1.5) three))
(dolist (two '(2 2.0 ?\02))
  (Assert (= (/ 3.0 two) 1.5) two))

(when (featurep 'bignum)
  (let* ((million 1000000)
	 (billion (* million 1000))	;; American, not British, billion
	 (trillion (* billion 1000)))
    (Assert (= (/ billion 1000) (/ trillion million) million 1000000.0))
    (Assert (= (/ billion -1000) (/ trillion (- million)) (- million)))
    (Assert (= (/ trillion 1000) billion 1000000000.0))
    (Assert (= (/ trillion -1000) (- billion) -1000000000.0))
    (Assert (= (/ trillion 10) (* 100 billion) 100000000000.0))
    (Assert (= (/ (- trillion) 10) (* -100 billion) -100000000000.0))))

(when (featurep 'ratio)
  (let ((half (div 1 2))
	(fivefourths (div 5 4))
	(fivehalfs (div 5 2)))
    (Assert (= half (read "3000000000/6000000000")))
    (Assert (= (/ fivehalfs fivefourths) 2))
    (Assert (= (/ fivefourths fivehalfs) half))
    (Assert (= (- half) (read "-3000000000/6000000000")))
    (Assert (= (/ fivehalfs (- fivefourths)) -2))
    (Assert (= (/ (- fivefourths) fivehalfs) (- half)))))

;; Test `*'
(Assert (= 1 (*)))

(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= 1 (* one)) one))

(dolist (two '(2 2.0 ?\02))
  (Assert (= 2 (* two)) two))

(dolist (six '(6 6.0 ?\06))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (* three two) six) (list three two six)))))

(dolist (three '(3 3.0 ?\03))
  (dolist (two '(2 2.0 ?\02))
    (Assert (= (* 1.5 two) three) (list two three))
    (dolist (five '(5 5.0 ?\05))
      (Assert (= 30 (* five two three)) (list five two three)))))

(when (featurep 'bignum)
  (let ((64K 65536))
    (Assert (= (* 64K 64K) (read "4294967296")))
    (Assert (= (* (- 64K) 64K) (read "-4294967296")))
    (Assert (/= (* -1 most-negative-fixnum) most-negative-fixnum))))

(when (featurep 'ratio)
  (let ((half (div 1 2))
	(fivefourths (div 5 4))
	(twofifths (div 2 5)))
    (Assert (= (* fivefourths twofifths) half))
    (Assert (= (* half twofifths) (read "3/15")))))

;; Test `+'
(Assert (= 0 (+)))

(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= 1 (+ one)) one))

(dolist (two '(2 2.0 ?\02))
  (Assert (= 2 (+ two)) two))

(dolist (five '(5 5.0 ?\05))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (+ three two) five) (list three two five))
      (Assert (= 10 (+ five two three)) (list five two three)))))

;; Test `max', `min'
(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= one (max one)) one)
  (Assert (= one (max one one)) one)
  (Assert (= one (max one one one)) one)
  (Assert (= one (min one)) one)
  (Assert (= one (min one one)) one)
  (Assert (= one (min one one one)) one)
  (dolist (two `(2 2.0 ?\02 ,(Int-to-Marker 2)))
    (Assert (= one (min one two)) (list one two))
    (Assert (= one (min one two two)) (list one two))
    (Assert (= one (min two two one)) (list one two))
    (Assert (= two (max one two)) (list one two))
    (Assert (= two (max one two two)) (list one two))
    (Assert (= two (max two two one)) (list one two))))

(when (featurep 'bignum)
  (let ((big (1+ most-positive-fixnum))
	(small (1- most-negative-fixnum)))
    (Assert (= big (max 1 1000000.0 most-positive-fixnum big)))
    (Assert (= small (min -1 -1000000.0 most-negative-fixnum small)))))

(when (featurep 'ratio)
  (let* ((big (1+ most-positive-fixnum))
	 (small (1- most-negative-fixnum))
	 (bigr (div (* 5 (1+ most-positive-fixnum)) 4))
	 (smallr (- bigr)))
    (Assert (= bigr (max 1 1000000.0 most-positive-fixnum big bigr)))
    (Assert (= smallr (min -1 -1000000.0 most-negative-fixnum small smallr)))))

;; The byte compiler has special handling for these constructs:
(let ((three 3) (five 5))
  (Assert (= (+ three five 1) 9))
  (Assert (= (+ 1 three five) 9))
  (Assert (= (+ three five -1) 7))
  (Assert (= (+ -1 three five) 7))
  (Assert (= (+ three 1) 4))
  (Assert (= (+ three -1) 2))
  (Assert (= (+ -1 three) 2))
  (Assert (= (+ -1 three) 2))
  (Assert (= (- three five 1) -3))
  (Assert (= (- 1 three five) -7))
  (Assert (= (- three five -1) -1))
  (Assert (= (- -1 three five) -9))
  (Assert (= (- three 1) 2))
  (Assert (= (- three 2 1) 0))
  (Assert (= (- 2 three 1) -2))
  (Assert (= (- three -1) 4))
  (Assert (= (- three 0) 3))
  (Assert (= (- three 0 five) -2))
  (Assert (= (- 0 three 0 five) -8))
  (Assert (= (- 0 three five) -8))
  (Assert (= (* three 2) 6))
  (Assert (= (* three -1 five) -15))
  (Assert (= (* three 1 five) 15))
  (Assert (= (* three 0 five) 0))
  (Assert (= (* three 2 five) 30))
  (Assert (= (/ three 1) 3))
  (Assert (= (/ three -1) -3))
  (Assert (= (/ (* five five) 2 2) 6))
  (Assert (= (/ 64 five 2) 6)))


;;-----------------------------------------------------
;; Logical bit-twiddling operations
;;-----------------------------------------------------
(Assert (= (logxor)  0))
(Assert (= (logior)  0))
(Assert (= (logand) -1))

(Check-Error wrong-type-argument (logxor 3.0))
(Check-Error wrong-type-argument (logior 3.0))
(Check-Error wrong-type-argument (logand 3.0))

(dolist (three '(3 ?\03))
  (Assert (eq 3 (logand three)) three)
  (Assert (eq 3 (logxor three)) three)
  (Assert (eq 3 (logior three)) three)
  (Assert (eq 3 (logand three three)) three)
  (Assert (eq 0 (logxor three three)) three)
  (Assert (eq 3 (logior three three))) three)

(dolist (one `(1 ?\01 ,(Int-to-Marker 1)))
  (dolist (two '(2 ?\02))
    (Assert (eq 0 (logand one two)) (list one two))
    (Assert (eq 3 (logior one two)) (list one two))
    (Assert (eq 3 (logxor one two)) (list one two)))
  (dolist (three '(3 ?\03))
    (Assert (eq 1 (logand one three)) (list one three))
    (Assert (eq 3 (logior one three)) (list one three))
    (Assert (eq 2 (logxor one three)) (list one three))))

;;-----------------------------------------------------
;; Test `%', mod
;;-----------------------------------------------------
(Check-Error wrong-number-of-arguments (%))
(Check-Error wrong-number-of-arguments (% 1))
(Check-Error wrong-number-of-arguments (% 1 2 3))

(Check-Error wrong-number-of-arguments (mod))
(Check-Error wrong-number-of-arguments (mod 1))
(Check-Error wrong-number-of-arguments (mod 1 2 3))

(Check-Error wrong-type-argument (% 10.0 2))
(Check-Error wrong-type-argument (% 10 2.0))

(flet ((test1 (x) (Assert (eql x (+ (% x 17) (* (/ x 17) 17))) x))
       (test2 (x) (Assert (eql (- x) (+ (% (- x) 17) (* (/ (- x) 17) 17))) x))
       (test3 (x) (Assert (eql x (+ (% (- x) 17) (* (/ (- x) 17) 17))) x))
       (test4 (x) (Assert (eql (% x -17) (- (% (- x) 17))) x))
       (test5 (x) (Assert (eql (% x -17) (% (- x) 17))) x))
  (test1 most-negative-fixnum)
  (if (featurep 'bignum)
      (progn
	(test2 most-negative-fixnum)
	(test4 most-negative-fixnum))
    (test3 most-negative-fixnum)
    (test5 most-negative-fixnum))
  (test1 most-positive-fixnum)
  (test2 most-positive-fixnum)
  (test4 most-positive-fixnum)
  (dotimes (j 30)
    (let ((x (random)))
      (if (eq x most-negative-fixnum) (setq x (1+ x)))
      (if (eq x most-positive-fixnum) (setq x (1- x)))
      (test1 x)
      (test2 x)
      (test4 x))))

(macrolet
    ((division-test (seven)
    `(progn
       (Assert (eq (% ,seven      2)  1))
       (Assert (eq (% ,seven     -2)  1))
       (Assert (eq (% (- ,seven)  2) -1))
       (Assert (eq (% (- ,seven) -2) -1))

       (Assert (eq (% ,seven      4)  3))
       (Assert (eq (% ,seven     -4)  3))
       (Assert (eq (% (- ,seven)  4) -3))
       (Assert (eq (% (- ,seven) -4) -3))

       (Assert (eq (%  35 ,seven)     0))
       (Assert (eq (% -35 ,seven)     0))
       (Assert (eq (%  35 (- ,seven)) 0))
       (Assert (eq (% -35 (- ,seven)) 0))

       (Assert (eq (mod ,seven      2)  1))
       (Assert (eq (mod ,seven     -2) -1))
       (Assert (eq (mod (- ,seven)  2)  1))
       (Assert (eq (mod (- ,seven) -2) -1))

       (Assert (eq (mod ,seven      4)  3))
       (Assert (eq (mod ,seven     -4) -1))
       (Assert (eq (mod (- ,seven)  4)  1))
       (Assert (eq (mod (- ,seven) -4) -3))

       (Assert (eq (mod  35 ,seven)     0))
       (Assert (eq (mod -35 ,seven)     0))
       (Assert (eq (mod  35 (- ,seven)) 0))
       (Assert (eq (mod -35 (- ,seven)) 0))

       (Assert (= (mod ,seven      2.0)  1.0))
       (Assert (= (mod ,seven     -2.0) -1.0))
       (Assert (= (mod (- ,seven)  2.0)  1.0))
       (Assert (= (mod (- ,seven) -2.0) -1.0))

       (Assert (= (mod ,seven      4.0)  3.0))
       (Assert (= (mod ,seven     -4.0) -1.0))
       (Assert (= (mod (- ,seven)  4.0)  1.0))
       (Assert (= (mod (- ,seven) -4.0) -3.0))

       (Assert (eq (% 0 ,seven) 0))
       (Assert (eq (% 0 (- ,seven)) 0))

       (Assert (eq (mod 0 ,seven) 0))
       (Assert (eq (mod 0 (- ,seven)) 0))

       (Assert (= (mod 0.0 ,seven) 0.0))
       (Assert (= (mod 0.0 (- ,seven)) 0.0)))))

  (division-test 7)
  (division-test ?\07)
  (division-test (Int-to-Marker 7)))

(when (featurep 'bignum)
  (let ((big (+ (* 7 most-positive-fixnum 6)))
	(negbig (- (* 7 most-negative-fixnum 6))))
    (= (% big (1+ most-positive-fixnum)) most-positive-fixnum)
    (= (% negbig (1- most-negative-fixnum)) most-negative-fixnum)
    (= (mod big (1+ most-positive-fixnum)) most-positive-fixnum)
    (= (mod negbig (1- most-negative-fixnum)) most-negative-fixnum)))

;;-----------------------------------------------------
;; Arithmetic comparison operations
;;-----------------------------------------------------
(Check-Error wrong-number-of-arguments (=))
(Check-Error wrong-number-of-arguments (<))
(Check-Error wrong-number-of-arguments (>))
(Check-Error wrong-number-of-arguments (<=))
(Check-Error wrong-number-of-arguments (>=))
(Check-Error wrong-number-of-arguments (/=))

;; One argument always yields t
(loop for x in `(1 1.0 ,(Int-to-Marker 1) ?z) do
  (Assert (eq t (=  x)) x)
  (Assert (eq t (<  x)) x)
  (Assert (eq t (>  x)) x)
  (Assert (eq t (>= x)) x)
  (Assert (eq t (<= x)) x)
  (Assert (eq t (/= x)) x)
  )

;; Type checking
(Check-Error wrong-type-argument (=  'foo 1))
(Check-Error wrong-type-argument (<= 'foo 1))
(Check-Error wrong-type-argument (>= 'foo 1))
(Check-Error wrong-type-argument (<  'foo 1))
(Check-Error wrong-type-argument (>  'foo 1))
(Check-Error wrong-type-argument (/= 'foo 1))

;; Meat
(dolist (one `(1 1.0 ,(Int-to-Marker 1) ?\01))
  (dolist (two '(2 2.0 ?\02))
    (Assert (<  one two) (list one two))
    (Assert (<= one two) (list one two))
    (Assert (<= two two) two)
    (Assert (>  two one) (list one two))
    (Assert (>= two one) (list one two))
    (Assert (>= two two) two)
    (Assert (/= one two) (list one two))
    (Assert (not (/= two two)) two)
    (Assert (not (< one one)) one)
    (Assert (not (> one one)) one)
    (Assert (<= one one two two) (list one two))
    (Assert (not (< one one two two)) (list one two))
    (Assert (>= two two one one) (list one two))
    (Assert (not (> two two one one)) (list one two))
    (Assert (= one one one) one)
    (Assert (not (= one one one two)) (list one two))
    (Assert (not (/= one two one)) (list one two))
    ))

(dolist (one `(1 1.0 ,(Int-to-Marker 1) ?\01))
  (dolist (two '(2 2.0 ?\02))
    (Assert (<  one two) (list one two))
    (Assert (<= one two) (list one two))
    (Assert (<= two two) two)
    (Assert (>  two one) (list one two))
    (Assert (>= two one) (list one two))
    (Assert (>= two two) two)
    (Assert (/= one two) (list one two))
    (Assert (not (/= two two)) two)
    (Assert (not (< one one)) one)
    (Assert (not (> one one)) one)
    (Assert (<= one one two two) (list one two))
    (Assert (not (< one one two two)) (list one two))
    (Assert (>= two two one one) (list one two))
    (Assert (not (> two two one one)) (list one two))
    (Assert (= one one one) one)
    (Assert (not (= one one one two)) (list one two))
    (Assert (not (/= one two one)) (list one two))
    ))

;; ad-hoc
(Assert (< 1 2))
(Assert (< 1 2 3 4 5 6))
(Assert (not (< 1 1)))
(Assert (not (< 2 1)))


(Assert (not (< 1 1)))
(Assert (< 1 2 3 4 5 6))
(Assert (<= 1 2 3 4 5 6))
(Assert (<= 1 2 3 4 5 6 6))
(Assert (not (< 1 2 3 4 5 6 6)))
(Assert (<= 1 1))

(Assert (not (eq (point) (point-marker))))
(Assert (= 1 (Int-to-Marker 1)))
(Assert (= (point) (point-marker)))

(when (featurep 'bignum)
  (let ((big1 (1+ most-positive-fixnum))
	(big2 (* 10 most-positive-fixnum))
	(small1 (1- most-negative-fixnum))
	(small2 (* 10 most-negative-fixnum)))
    (Assert (< small2 small1 most-negative-fixnum most-positive-fixnum big1
	       big2))
    (Assert (<= small2 small1 most-negative-fixnum most-positive-fixnum big1
		big2))
    (Assert (> big2 big1 most-positive-fixnum most-negative-fixnum small1
	       small2))
    (Assert (>= big2 big1 most-positive-fixnum most-negative-fixnum small1
		small2))
    (Assert (/= small2 small1 most-negative-fixnum most-positive-fixnum big1
		big2))))

(when (featurep 'ratio)
  (let ((big1 (div (* 10 most-positive-fixnum) 4))
	(big2 (div (* 5 most-positive-fixnum) 2))
	(big3 (div (* 7 most-positive-fixnum) 2))
	(small1 (div (* 10 most-negative-fixnum) 4))
	(small2 (div (* 5 most-negative-fixnum) 2))
	(small3 (div (* 7 most-negative-fixnum) 2)))
    (Assert (= big1 big2))
    (Assert (= small1 small2))
    (Assert (< small3 small1 most-negative-fixnum most-positive-fixnum big1
	       big3))
    (Assert (<= small3 small2 small1 most-negative-fixnum most-positive-fixnum
		big1 big2 big3))
    (Assert (> big3 big1 most-positive-fixnum most-negative-fixnum small1
	       small3))
    (Assert (>= big3 big2 big1 most-positive-fixnum most-negative-fixnum
		small1 small2 small3))
    (Assert (/= big3 big1 most-positive-fixnum most-negative-fixnum small1
		small3))))

;;-----------------------------------------------------
;; testing list-walker functions
;;-----------------------------------------------------
(macrolet
    ((test-fun
      (fun)
      `(progn
	 (Check-Error wrong-number-of-arguments (,fun))
	 (Check-Error wrong-number-of-arguments (,fun nil))
	 (Check-Error (malformed-list wrong-type-argument) (,fun nil 1))
	 ,@(loop for n in '(1 2 2000)
	     collect `(Check-Error circular-list (,fun 1 (make-circular-list ,n))))))
     (test-funs (&rest funs) `(progn ,@(loop for fun in funs collect `(test-fun ,fun)))))

  (test-funs member* member memq 
             assoc* assoc assq 
             rassoc* rassoc rassq 
             delete* delete delq 
             remove* remove remq 
             old-member old-memq 
             old-assoc old-assq 
             old-rassoc old-rassq 
             old-delete old-delq 
             remassoc remassq remrassoc remrassq))

(let ((x '((1 . 2) 3 (4 . 5))))
  (Assert (eq (assoc  1 x) (car x)))
  (Assert (eq (assq   1 x) (car x)))
  (Assert (eq (rassoc 1 x) nil))
  (Assert (eq (rassq  1 x) nil))
  (Assert (eq (assoc  2 x) nil))
  (Assert (eq (assq   2 x) nil))
  (Assert (eq (rassoc 2 x) (car x)))
  (Assert (eq (rassq  2 x) (car x)))
  (Assert (eq (assoc  3 x) nil))
  (Assert (eq (assq   3 x) nil))
  (Assert (eq (rassoc 3 x) nil))
  (Assert (eq (rassq  3 x) nil))
  (Assert (eq (assoc  4 x) (caddr x)))
  (Assert (eq (assq   4 x) (caddr x)))
  (Assert (eq (rassoc 4 x) nil))
  (Assert (eq (rassq  4 x) nil))
  (Assert (eq (assoc  5 x) nil))
  (Assert (eq (assq   5 x) nil))
  (Assert (eq (rassoc 5 x) (caddr x)))
  (Assert (eq (rassq  5 x) (caddr x)))
  (Assert (eq (assoc  6 x) nil))
  (Assert (eq (assq   6 x) nil))
  (Assert (eq (rassoc 6 x) nil))
  (Assert (eq (rassq  6 x) nil)))

(let ((x '(("1" . "2") "3" ("4" . "5"))))
  (Assert (eq (assoc  "1" x) (car x)))
  (Assert (eq (assq   "1" x) nil))
  (Assert (eq (rassoc "1" x) nil))
  (Assert (eq (rassq  "1" x) nil))
  (Assert (eq (assoc  "2" x) nil))
  (Assert (eq (assq   "2" x) nil))
  (Assert (eq (rassoc "2" x) (car x)))
  (Assert (eq (rassq  "2" x) nil))
  (Assert (eq (assoc  "3" x) nil))
  (Assert (eq (assq   "3" x) nil))
  (Assert (eq (rassoc "3" x) nil))
  (Assert (eq (rassq  "3" x) nil))
  (Assert (eq (assoc  "4" x) (caddr x)))
  (Assert (eq (assq   "4" x) nil))
  (Assert (eq (rassoc "4" x) nil))
  (Assert (eq (rassq  "4" x) nil))
  (Assert (eq (assoc  "5" x) nil))
  (Assert (eq (assq   "5" x) nil))
  (Assert (eq (rassoc "5" x) (caddr x)))
  (Assert (eq (rassq  "5" x) nil))
  (Assert (eq (assoc  "6" x) nil))
  (Assert (eq (assq   "6" x) nil))
  (Assert (eq (rassoc "6" x) nil))
  (Assert (eq (rassq  "6" x) nil)))

(flet ((a () (list '(1 . 2) 3 '(4 . 5))))
  (Assert (let* ((x (a)) (y (remassoc  1 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remassq   1 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remrassoc 1 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  1 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  2 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   2 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 2 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remrassq  2 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))

  (Assert (let* ((x (a)) (y (remassoc  3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  3 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  4 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remassq   4 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remrassoc 4 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  4 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  5 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   5 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 5 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remrassq  5 x))) (and (eq x y) (equal y '((1 . 2) 3)))))

  (Assert (let* ((x (a)) (y (remassoc  6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  6 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (delete     3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (delq       3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delete 3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delq   3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))

  (Assert (let* ((x (a)) (y (delete     '(1 . 2) x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (delq       '(1 . 2) x))) (and      (eq x y)  (equal y (a)))))
  (Assert (let* ((x (a)) (y (old-delete '(1 . 2) x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delq   '(1 . 2) x))) (and      (eq x y)  (equal y (a)))))
  )



(flet ((a () (list '("1" . "2") "3" '("4" . "5"))))
  (Assert (let* ((x (a)) (y (remassoc  "1" x))) (and (not (eq x y)) (equal y '("3" ("4" . "5"))))))
  (Assert (let* ((x (a)) (y (remassq   "1" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "1" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "1" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "2" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "2" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "2" x))) (and (not (eq x y)) (equal y '("3" ("4" . "5"))))))
  (Assert (let* ((x (a)) (y (remrassq  "2" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "3" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "4" x))) (and (eq x y) (equal y '(("1" . "2") "3")))))
  (Assert (let* ((x (a)) (y (remassq   "4" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "4" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "4" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "5" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "5" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "5" x))) (and (eq x y) (equal y '(("1" . "2") "3")))))
  (Assert (let* ((x (a)) (y (remrassq  "5" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "6" x))) (and (eq x y) (equal y (a))))))

;;-----------------------------------------------------
;; function-max-args, function-min-args
;;-----------------------------------------------------
(defmacro check-function-argcounts (fun min max)
  `(progn
     (Assert (eq (function-min-args ,fun) ,min))
     (Assert (eq (function-max-args ,fun) ,max))))

(check-function-argcounts 'prog1 1 nil)         ; special form
(check-function-argcounts 'command-execute 1 3)	; normal subr
(check-function-argcounts 'funcall 1 nil)       ; `MANY' subr
(check-function-argcounts 'garbage-collect 0 0) ; no args subr

;; Test interpreted and compiled functions
(loop for (arglist min max) in
  '(((arg1 arg2 &rest args) 2 nil)
    ((arg1 arg2 &optional arg3 arg4) 2 4)
    ((arg1 arg2 &optional arg3 arg4 &rest args) 2 nil)
    (() 0 0))
  do
  (eval
   `(progn
      (defun test-fun ,arglist nil)
      (check-function-argcounts '(lambda ,arglist nil) ,min ,max)
      (check-function-argcounts (byte-compile '(lambda ,arglist nil)) ,min ,max))))

;; Test subr-arity. 
(loop for (function-name arity) in
  '((let (1 . unevalled))
    (prog1 (1 . unevalled))
    (list (0 . many))
    (type-of (1 . 1))
    (garbage-collect (0 . 0)))
  do (Assert (equal (subr-arity (symbol-function function-name)) arity)))
  
(Check-Error wrong-type-argument (subr-arity
                                  (lambda () (message "Hi there!"))))
  
(Check-Error wrong-type-argument (subr-arity nil))

;;-----------------------------------------------------
;; Detection of cyclic variable indirection loops
;;-----------------------------------------------------
(fset 'test-sym1 'test-sym1)
(Check-Error cyclic-function-indirection (test-sym1))

(fset 'test-sym1 'test-sym2)
(fset 'test-sym2 'test-sym1)
(Check-Error cyclic-function-indirection (test-sym1))
(fmakunbound 'test-sym1) ; else macroexpand-internal infloops!
(fmakunbound 'test-sym2)

;;-----------------------------------------------------
;; Test `type-of'
;;-----------------------------------------------------
(Assert (eq (type-of load-path) 'cons))
(Assert (eq (type-of obarray) 'vector))
(Assert (eq (type-of 42) 'integer))
(Assert (eq (type-of ?z) 'character))
(Assert (eq (type-of "42") 'string))
(Assert (eq (type-of 'foo) 'symbol))
(Assert (eq (type-of (selected-device)) 'device))

;;-----------------------------------------------------
;; Test mapping functions
;;-----------------------------------------------------
(Check-Error wrong-type-argument (mapcar #'identity (current-buffer)))
(Assert (equal (mapcar #'identity load-path) load-path))
(Assert (equal (mapcar #'identity '(1 2 3)) '(1 2 3)))
(Assert (equal (mapcar #'identity "123") '(?1 ?2 ?3)))
(Assert (equal (mapcar #'identity [1 2 3]) '(1 2 3)))
(Assert (equal (mapcar #'identity #*010) '(0 1 0)))

(let ((z 0) (list (make-list 1000 1)))
  (mapc (lambda (x) (incf z x)) list)
  (Assert (eq 1000 z)))

(Check-Error wrong-type-argument (mapvector #'identity (current-buffer)))
(Assert (equal (mapvector #'identity '(1 2 3)) [1 2 3]))
(Assert (equal (mapvector #'identity "123") [?1 ?2 ?3]))
(Assert (equal (mapvector #'identity [1 2 3]) [1 2 3]))
(Assert (equal (mapvector #'identity #*010) [0 1 0]))

(Check-Error wrong-type-argument (mapconcat #'identity (current-buffer) "foo"))
(Assert (equal (mapconcat #'identity '("1" "2" "3") "|") "1|2|3"))
(Assert (equal (mapconcat #'identity ["1" "2" "3"]  "|") "1|2|3"))

;; The following 2 functions used to crash XEmacs via mapcar1().
;; We don't test the actual values of the mapcar, since they're undefined.
(Assert
 (let ((x (list (cons 1 1) (cons 2 2) (cons 3 3))))
   (mapcar
    (lambda (y)
      "Devious evil mapping function"
      (when (eq (car y) 2) ; go out onto a limb
	(setcdr x nil)     ; cut it off behind us
	(garbage-collect)) ; are we riding a magic broomstick?
      (car y))             ; sorry, hard landing
    x)))

(Assert
 (let ((x (list (cons 1 1) (cons 2 2) (cons 3 3))))
   (mapcar
    (lambda (y)
      "Devious evil mapping function"
      (when (eq (car y) 1)
	(setcdr (cdr x) 42)) ; drop a brick wall onto the freeway
      (car y))
    x)))

(Assert
 (equal
  (let ((list (list pi))) (mapcar* #'cons [1 2 3 4] (nconc list list)))
  `((1 . ,pi) (2 . ,pi) (3 . ,pi) (4 . ,pi)))
 "checking mapcar* behaves correctly when only one arg is circular")

(Assert (eql
 (length (multiple-value-list
          (car (mapcar #'(lambda (argument) (floor argument)) (list pi e)))))
 1)
 "checking multiple values are correctly discarded in mapcar")

(let ((malformed-list '(1 2 3 4 hi there . tail)))
  (Check-Error malformed-list (mapcar #'identity malformed-list))
  (Check-Error malformed-list (map nil #'eq [1 2 3 4]
				   malformed-list))
  (Check-Error malformed-list (list-length malformed-list)))

;;-----------------------------------------------------
;; Test vector functions
;;-----------------------------------------------------
(Assert (equal [1 2 3] [1 2 3]))
(Assert (equal [] []))
(Assert (not (equal [1 2 3] [])))
(Assert (not (equal [1 2 3] [1 2 4])))
(Assert (not (equal [0 2 3] [1 2 3])))
(Assert (not (equal [1 2 3] [1 2 3 4])))
(Assert (not (equal [1 2 3 4] [1 2 3])))
(Assert (equal (vector 1 2 3) [1 2 3]))
(Assert (equal (make-vector 3 1) [1 1 1]))

;;-----------------------------------------------------
;; Test bit-vector functions
;;-----------------------------------------------------
(Assert (equal #*010 #*010))
(Assert (equal #* #*))
(Assert (not (equal #*010 #*011)))
(Assert (not (equal #*010 #*)))
(Assert (not (equal #*110 #*010)))
(Assert (not (equal #*010 #*0100)))
(Assert (not (equal #*0101 #*010)))
(Assert (equal (bit-vector 0 1 0) #*010))
(Assert (equal (make-bit-vector 3 1) #*111))
(Assert (equal (make-bit-vector 3 0) #*000))

;;-----------------------------------------------------
;; Test buffer-local variables used as (ugh!) function parameters
;;-----------------------------------------------------
(make-local-variable 'test-emacs-buffer-local-variable)
(byte-compile
 (defun test-emacs-buffer-local-parameter (test-emacs-buffer-local-variable)
   (setq test-emacs-buffer-local-variable nil)))
(test-emacs-buffer-local-parameter nil)

;;-----------------------------------------------------
;; Test split-string
;;-----------------------------------------------------
;; Keep nulls, explicit SEPARATORS
;; Hrvoje didn't like the next 3 tests so I'm disabling them for now. -sb
;; I assume Hrvoje worried about the possibility of infloops. -sjt
(when test-harness-risk-infloops
  (Assert (equal (split-string "foo" "") '("" "f" "o" "o" "")))
  (Assert (equal (split-string "foo" "^") '("" "foo")))
  (Assert (equal (split-string "foo" "$") '("foo" ""))))
(Assert (equal (split-string "foo,bar" ",") '("foo" "bar")))
(Assert (equal (split-string ",foo,bar," ",") '("" "foo" "bar" "")))
(Assert (equal (split-string ",foo,bar," "^,") '("" "foo,bar,")))
(Assert (equal (split-string ",foo,bar," ",$") '(",foo,bar" "")))
(Assert (equal (split-string ",foo,,bar," ",") '("" "foo" "" "bar" "")))
(Assert (equal (split-string "foo,,,bar" ",") '("foo" "" "" "bar")))
(Assert (equal (split-string "foo,,bar,," ",") '("foo" "" "bar" "" "")))
(Assert (equal (split-string "foo,,bar" ",+") '("foo" "bar")))
(Assert (equal (split-string ",foo,,bar," ",+") '("" "foo" "bar" "")))
;; Omit nulls, explicit SEPARATORS
(when test-harness-risk-infloops
  (Assert (equal (split-string "foo" "" t) '("f" "o" "o")))
  (Assert (equal (split-string "foo" "^" t) '("foo")))
  (Assert (equal (split-string "foo" "$" t) '("foo"))))
(Assert (equal (split-string "foo,bar" "," t) '("foo" "bar")))
(Assert (equal (split-string ",foo,bar," "," t) '("foo" "bar")))
(Assert (equal (split-string ",foo,bar," "^," t) '("foo,bar,")))
(Assert (equal (split-string ",foo,bar," ",$" t) '(",foo,bar")))
(Assert (equal (split-string ",foo,,bar," "," t) '("foo" "bar")))
(Assert (equal (split-string "foo,,,bar" "," t) '("foo" "bar")))
(Assert (equal (split-string "foo,,bar,," "," t) '("foo" "bar")))
(Assert (equal (split-string "foo,,bar" ",+" t) '("foo" "bar")))
(Assert (equal (split-string ",foo,,bar," ",+" t) '("foo" "bar")))
;; "Double-default" case
(Assert (equal (split-string "foo bar") '("foo" "bar")))
(Assert (equal (split-string " foo bar ") '("foo" "bar")))
(Assert (equal (split-string " foo  bar ") '("foo" "bar")))
(Assert (equal (split-string "foo   bar") '("foo" "bar")))
(Assert (equal (split-string "foo  bar  ") '("foo" "bar")))
(Assert (equal (split-string "foobar") '("foobar")))
;; Semantics are identical to "double-default" case!  Fool ya?
(Assert (equal (split-string "foo bar" nil t) '("foo" "bar")))
(Assert (equal (split-string " foo bar " nil t) '("foo" "bar")))
(Assert (equal (split-string " foo  bar " nil t) '("foo" "bar")))
(Assert (equal (split-string "foo   bar" nil t) '("foo" "bar")))
(Assert (equal (split-string "foo  bar  " nil t) '("foo" "bar")))
(Assert (equal (split-string "foobar" nil t) '("foobar")))
;; Perverse "anti-double-default" case
(Assert (equal (split-string "foo bar" split-string-default-separators)
	       '("foo" "bar")))
(Assert (equal (split-string " foo bar " split-string-default-separators)
	       '("" "foo" "bar" "")))
(Assert (equal (split-string " foo  bar " split-string-default-separators)
	       '("" "foo" "bar" "")))
(Assert (equal (split-string "foo   bar" split-string-default-separators)
	       '("foo" "bar")))
(Assert (equal (split-string "foo  bar  " split-string-default-separators)
	       '("foo" "bar" "")))
(Assert (equal (split-string "foobar" split-string-default-separators)
	       '("foobar")))

;;-----------------------------------------------------
;; Test split-string-by-char
;;-----------------------------------------------------

(Assert
 (equal
  (split-string-by-char
   #r"re\:ee:this\\is\\text\\\\:oo\ps:
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen\: Geister br�ten.\\
Serum concentrations of vitamin E: (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein"
  ?: ?\\)
  '("re:ee" "this\\is\\text\\\\" "oops" "
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen: Geister br�ten.\\
Serum concentrations of vitamin E" " (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein")))
(Assert
 (equal
  (split-string-by-char
   #r"re\:ee:this\\is\\text\\\\:oo\ps:
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen\: Geister br�ten.\\
Serum concentrations of vitamin E: (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein"
   ?: ?\x00)
  '("re\\" "ee" "this\\\\is\\\\text\\\\\\\\" "oo\\ps" "
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen\\" " Geister br�ten.\\\\
Serum concentrations of vitamin E" " (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein")))
(Assert
 (equal
  (split-string-by-char
   #r"re\:ee:this\\is\\text\\\\:oo\ps:
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen\: Geister br�ten.\\
Serum concentrations of vitamin E: (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein" ?\\)
  '("re" ":ee:this" "" "is" "" "text" "" "" "" ":oo" "ps:
Eine Sprache, die stagnirt, ist zu vergleichen mit einem See, dem der
bisherige Quellenzuflu� versiegt oder abgeleitet wird. Aus dem Wasser,
wor�ber der Geist Gottes schwebte, wird Sumpf und Moder, wor�ber die
unreinen" ": Geister br�ten." "" "
Serum concentrations of vitamin E: (alpha-tocopherol) depend on the liver,
which takes up the nutrient after the various forms are absorbed from the
small intestine. The liver preferentially resecretes only alpha-tocopherol
via the hepatic alpha-tocopherol transfer protein")))

;;-----------------------------------------------------
;; Test near-text buffer functions.
;;-----------------------------------------------------
(with-temp-buffer
  (erase-buffer)
  (Assert (eq (char-before) nil))
  (Assert (eq (char-before (point)) nil))
  (Assert (eq (char-before (point-marker)) nil))
  (Assert (eq (char-before (point) (current-buffer)) nil))
  (Assert (eq (char-before (point-marker) (current-buffer)) nil))
  (Assert (eq (char-after) nil))
  (Assert (eq (char-after (point)) nil))
  (Assert (eq (char-after (point-marker)) nil))
  (Assert (eq (char-after (point) (current-buffer)) nil))
  (Assert (eq (char-after (point-marker) (current-buffer)) nil))
  (Assert (eq (preceding-char) 0))
  (Assert (eq (preceding-char (current-buffer)) 0))
  (Assert (eq (following-char) 0))
  (Assert (eq (following-char (current-buffer)) 0))
  (insert "foobar")
  (Assert (eq (char-before) ?r))
  (Assert (eq (char-after) nil))
  (Assert (eq (preceding-char) ?r))
  (Assert (eq (following-char) 0))
  (goto-char (point-min))
  (Assert (eq (char-before) nil))
  (Assert (eq (char-after) ?f))
  (Assert (eq (preceding-char) 0))
  (Assert (eq (following-char) ?f))
  )

;;-----------------------------------------------------
;; Test plist manipulation functions.
;;-----------------------------------------------------
(let ((sym (make-symbol "test-symbol")))
  (Assert (eq t (get* sym t t)))
  (Assert (eq t (get  sym t t)))
  (Assert (eq t (getf nil t t)))
  (Assert (eq t (plist-get nil t t)))
  (put sym 'bar 'baz)
  (Assert (eq 'baz (get sym 'bar)))
  (Assert (eq 'baz (getf '(bar baz) 'bar)))
  (Assert (eq 'baz (getf (symbol-plist sym) 'bar)))
  (Assert (eq 2 (getf '(1 2) 1)))
  (Assert (eq 4 (put sym 3 4)))
  (Assert (eq 4 (get sym 3)))
  (Assert (eq t (remprop sym 3)))
  (Assert (eq nil (remprop sym 3)))
  (Assert (eq 5 (get sym 3 5)))
  )

(loop for obj in
  (list (make-symbol "test-symbol")
	"test-string"
	(make-extent nil nil nil)
	(make-face 'test-face))
  do
  (Assert (eq 2 (get obj ?1 2)) obj)
  (Assert (eq 4 (put obj ?3 4)) obj)
  (Assert (eq 4 (get obj ?3)) obj)
  (when (or (stringp obj) (symbolp obj))
    (Assert (equal '(?3 4) (object-plist obj)) obj))
  (Assert (eq t (remprop obj ?3)) obj)
  (when (or (stringp obj) (symbolp obj))
    (Assert (eq '() (object-plist obj)) obj))
  (Assert (eq nil (remprop obj ?3)) obj)
  (when (or (stringp obj) (symbolp obj))
    (Assert (eq '() (object-plist obj)) obj))
  (Assert (eq 5 (get obj ?3 5)) obj)
  )

(Check-Error-Message
 error "Object type has no properties"
 (get 2 'property))

(Check-Error-Message
 error "Object type has no settable properties"
 (put (current-buffer) 'property 'value))

(Check-Error-Message
 error "Object type has no removable properties"
 (remprop ?3 'property))

(Check-Error-Message
 error "Object type has no properties"
 (object-plist (symbol-function 'car)))

(Check-Error-Message
 error "Can't remove property from object"
 (remprop (make-extent nil nil nil) 'detachable))

;;-----------------------------------------------------
;; Test subseq
;;-----------------------------------------------------
(Assert (equal (subseq nil 0) nil))
(Assert (equal (subseq [1 2 3] 0) [1 2 3]))
(Assert (equal (subseq [1 2 3] 1 -1) [2]))
(Assert (equal (subseq "123" 0) "123"))
(Assert (equal (subseq "1234" -3 -1) "23"))
(Assert (equal (subseq #*0011 0) #*0011))
(Assert (equal (subseq #*0011 -3 3) #*01))
(Assert (equal (subseq '(1 2 3) 0) '(1 2 3)))
(Assert (equal (subseq '(1 2 3 4) -3 nil) '(2 3 4)))

(Check-Error wrong-type-argument (subseq 3 2))
(Check-Error args-out-of-range (subseq [1 2 3] -42))
(Check-Error args-out-of-range (subseq [1 2 3] 0 42))

(Check-Error wrong-type-argument (substring-no-properties nil 4))
(Check-Error wrong-type-argument (substring-no-properties "hi there" pi))
(Check-Error wrong-type-argument (substring-no-properties "hi there" 0))

;;-----------------------------------------------------
;; Time-related tests
;;-----------------------------------------------------
(Assert (= (length (current-time-string)) 24))

;;-----------------------------------------------------
;; format test
;;-----------------------------------------------------
(Assert (string= (format "%d" 10) "10"))
(Assert (string= (format "%o" 8) "10"))
(Assert (string= (format "%b" 2) "10"))
(Assert (string= (format "%x" 31) "1f"))
(Assert (string= (format "%X" 31) "1F"))
(Assert (string= (format "%b" 0) "0"))
(Assert (string= (format "%b" 3) "11"))
;; MS-Windows uses +002 in its floating-point numbers.  #### We should
;; perhaps fix this, but writing our own floating-point support in doprnt.c
;; is very hard.
(Assert (or (string= (format "%e" 100) "1.000000e+02")
	    (string= (format "%e" 100) "1.000000e+002")))
(Assert (or (string= (format "%E" 100) "1.000000E+02")
	    (string= (format "%E" 100) "1.000000E+002")))
(Assert (or (string= (format "%E" 100) "1.000000E+02")
	    (string= (format "%E" 100) "1.000000E+002")))
(Assert (string= (format "%f" 100) "100.000000"))
(Assert (string= (format "%7.3f" 12.12345) " 12.123"))
(Assert (string= (format "%07.3f" 12.12345) "012.123"))
(Assert (string= (format "%-7.3f" 12.12345) "12.123 "))
(Assert (string= (format "%-07.3f" 12.12345) "12.123 "))
(Assert (string= (format "%g" 100.0) "100"))
(Assert (or (string= (format "%g" 0.000001) "1e-06")
	    (string= (format "%g" 0.000001) "1e-006")))
(Assert (string= (format "%g" 0.0001) "0.0001"))
(Assert (string= (format "%G" 100.0) "100"))
(Assert (or (string= (format "%G" 0.000001) "1E-06")
	    (string= (format "%G" 0.000001) "1E-006")))
(Assert (string= (format "%G" 0.0001) "0.0001"))

(Assert (string= (format "%2$d%1$d" 10 20) "2010"))
(Assert (string= (format "%-d" 10) "10"))
(Assert (string= (format "%-4d" 10) "10  "))
(Assert (string= (format "%+d" 10) "+10"))
(Assert (string= (format "%+d" -10) "-10"))
(Assert (string= (format "%+4d" 10) " +10"))
(Assert (string= (format "%+4d" -10) " -10"))
(Assert (string= (format "% d" 10) " 10"))
(Assert (string= (format "% d" -10) "-10"))
(Assert (string= (format "% 4d" 10) "  10"))
(Assert (string= (format "% 4d" -10) " -10"))
(Assert (string= (format "%0d" 10) "10"))
(Assert (string= (format "%0d" -10) "-10"))
(Assert (string= (format "%04d" 10) "0010"))
(Assert (string= (format "%04d" -10) "-010"))
(Assert (string= (format "%*d" 4 10) "  10"))
(Assert (string= (format "%*d" 4 -10) " -10"))
(Assert (string= (format "%*d" -4 10) "10  "))
(Assert (string= (format "%*d" -4 -10) "-10 "))
(Assert (string= (format "%#d" 10) "10"))
(Assert (string= (format "%#o" 8) "010"))
(Assert (string= (format "%#x" 16) "0x10"))
(Assert (or (string= (format "%#e" 100) "1.000000e+02")
	    (string= (format "%#e" 100) "1.000000e+002")))
(Assert (or (string= (format "%#E" 100) "1.000000E+02")
	    (string= (format "%#E" 100) "1.000000E+002")))
(Assert (string= (format "%#f" 100) "100.000000"))
(Assert (string= (format "%#g" 100.0) "100.000"))
(Assert (or (string= (format "%#g" 0.000001) "1.00000e-06")
	    (string= (format "%#g" 0.000001) "1.00000e-006")))
(Assert (string= (format "%#g" 0.0001) "0.000100000"))
(Assert (string= (format "%#G" 100.0) "100.000"))
(Assert (or (string= (format "%#G" 0.000001) "1.00000E-06")
	    (string= (format "%#G" 0.000001) "1.00000E-006")))
(Assert (string= (format "%#G" 0.0001) "0.000100000"))
(Assert (string= (format "%.1d" 10) "10"))
(Assert (string= (format "%.4d" 10) "0010"))
;; Combination of `-', `+', ` ', `0', `#', `.', `*'
(Assert (string= (format "%-04d" 10) "10  "))
(Assert (string= (format "%-*d" 4 10) "10  "))
;; #### Correctness of this behavior is questionable.
;; It might be better to signal error.
(Assert (string= (format "%-*d" -4 10) "10  "))
;; These behavior is not specified.
;; (format "%-+d" 10)
;; (format "%- d" 10)
;; (format "%-01d" 10)
;; (format "%-#4x" 10)
;; (format "%-.1d" 10)

(Assert (string= (format "%01.1d" 10) "10"))
(Assert (string= (format "%03.1d" 10) " 10"))
(Assert (string= (format "%01.3d" 10) "010"))
(Assert (string= (format "%1.3d" 10) "010"))
(Assert (string= (format "%3.1d" 10) " 10"))

;;; The following two tests used to use 1000 instead of 100,
;;; but that merely found buffer overflow bugs in Solaris sprintf().
(Assert (= 102 (length (format "%.100f" 3.14))))
(Assert (= 100 (length (format "%100f" 3.14))))

;;; Check for 64-bit cleanness on LP64 platforms.
(Assert (= (read (format "%d"  most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%ld" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%u"  most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%lu" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%d"  most-negative-fixnum)) most-negative-fixnum))
(Assert (= (read (format "%ld" most-negative-fixnum)) most-negative-fixnum))

;; These used to crash. 
(Assert (eql (read (format "%f" 1.2e+302)) 1.2e+302))
(Assert (eql (read (format "%.1000d" 1)) 1))

;;; "%u" is undocumented, and Emacs Lisp has no unsigned type.
;;; What to do if "%u" is used with a negative number?
;;; For non-bignum XEmacsen, the most reasonable thing seems to be to print an
;;; un-read-able number.  The printed value might be useful to a human, if not
;;; to Emacs Lisp.
;;; For bignum XEmacsen, we make %u with a negative value throw an error.
(if (featurep 'bignum)
    (progn
      (Check-Error wrong-type-argument (format "%u" most-negative-fixnum))
      (Check-Error wrong-type-argument (format "%u" -1)))
  (Check-Error invalid-read-syntax (read (format "%u" most-negative-fixnum)))
  (Check-Error invalid-read-syntax (read (format "%u" -1))))

;; Check all-completions ignore element start with space.
(Assert (not (all-completions "" '((" hidden" . "object")))))
(Assert (all-completions " " '((" hidden" . "object"))))

(let* ((literal-with-uninterned
	'(first-element
	  [#1=#:G32976 #2=#:G32974 #3=#:G32971 #4=#:G32969 alias
		       #s(hash-table size 256 data (969 ?\xF9 55 ?7 166 ?\xA6))
		       #5=#:G32970 #6=#:G32972]))
       (print-readably t)
       (print-gensym t)
       (printed-with-uninterned (prin1-to-string literal-with-uninterned))
       (awkward-regexp "#1=#")
       (first-match-start (string-match awkward-regexp
					printed-with-uninterned)))
  (Assert (null (string-match awkward-regexp printed-with-uninterned
			      (1+ first-match-start)))))

(let ((char-table-with-string #s(char-table data (?\x00 "text")))
      (char-table-with-symbol #s(char-table data (?\x00 text))))
  (Assert (not (string-equal (prin1-to-string char-table-with-string)
                             (prin1-to-string char-table-with-symbol)))
          "Check that char table elements are quoted correctly when printing"))


(let ((test-file-name
       (make-temp-file (expand-file-name "sR4KDwU" (temp-directory))
		       nil ".el")))
  (find-file test-file-name)
  (erase-buffer)
  (insert 
       "\
;; Lisp should not be able to modify #$, which is
;; Vload_file_name_internal of lread.c.
(Check-Error setting-constant (aset #$ 0 ?\\ ))

;; But modifying load-file-name should work:
(let ((new-char ?\\ )
      old-char)
  (setq old-char (aref load-file-name 0))
  (if (= new-char old-char)
      (setq new-char ?/))
  (aset load-file-name 0 new-char)
  (Assert (= new-char (aref load-file-name 0))
	  \"Check that we can modify the string value of load-file-name\"))

(let* ((new-load-file-name \"hi there\")
       (load-file-name new-load-file-name))
  (Assert (eq new-load-file-name load-file-name)
	  \"Checking that we can bind load-file-name successfully.\"))

")
   (write-region (point-min) (point-max) test-file-name nil 'quiet)
   (set-buffer-modified-p nil)
   (kill-buffer nil)
   (load test-file-name nil t nil)
   (delete-file test-file-name))

(flet ((cl-floor (x &optional y)
	 (let ((q (floor x y)))
	   (list q (- x (if y (* y q) q)))))
       (cl-ceiling (x &optional y)
	 (let ((res (cl-floor x y)))
	   (if (= (car (cdr res)) 0) res
	     (list (1+ (car res)) (- (car (cdr res)) (or y 1))))))
       (cl-truncate (x &optional y)
	 (if (eq (>= x 0) (or (null y) (>= y 0)))
	     (cl-floor x y) (cl-ceiling x y)))
       (cl-round (x &optional y)
	 (if y
	     (if (and (integerp x) (integerp y))
		 (let* ((hy (/ y 2))
			(res (cl-floor (+ x hy) y)))
		   (if (and (= (car (cdr res)) 0)
			    (= (+ hy hy) y)
			    (/= (% (car res) 2) 0))
		       (list (1- (car res)) hy)
		     (list (car res) (- (car (cdr res)) hy))))
	       (let ((q (round (/ x y))))
		 (list q (- x (* q y)))))
	   (if (integerp x) (list x 0)
	     (let ((q (round x)))
	       (list q (- x q))))))
       (Assert-rounding (first second &key
			 one-floor-result two-floor-result 
			 one-ffloor-result two-ffloor-result 
			 one-ceiling-result two-ceiling-result
			 one-fceiling-result two-fceiling-result
			 one-round-result two-round-result
			 one-fround-result two-fround-result
			 one-truncate-result two-truncate-result
			 one-ftruncate-result two-ftruncate-result)
	 (Assert (equal one-floor-result (multiple-value-list
					  (floor first)))
		 (format "checking (floor %S) gives %S"
			 first one-floor-result))
	 (Assert (equal one-floor-result (multiple-value-list
					  (floor first 1)))
		 (format "checking (floor %S 1) gives %S"
			 first one-floor-result))
	 (Check-Error arith-error (floor first 0))
	 (Check-Error arith-error (floor first 0.0))
	 (Assert (equal two-floor-result (multiple-value-list
					  (floor first second)))
		 (format
		  "checking (floor %S %S) gives %S"
		  first second two-floor-result))
	 (Assert (equal (cl-floor first second)
			(multiple-value-list (floor first second)))
		 (format
		  "checking (floor %S %S) gives the same as the old code"
		  first second))
	 (Assert (equal one-ffloor-result (multiple-value-list
					   (ffloor first)))
		 (format "checking (ffloor %S) gives %S"
			 first one-ffloor-result))
	 (Assert (equal one-ffloor-result (multiple-value-list
					   (ffloor first 1)))
		 (format "checking (ffloor %S 1) gives %S"
			 first one-ffloor-result))
	 (Check-Error arith-error (ffloor first 0))
	 (Check-Error arith-error (ffloor first 0.0))
	 (Assert (equal two-ffloor-result (multiple-value-list
					   (ffloor first second)))
		 (format "checking (ffloor %S %S) gives %S"
			 first second two-ffloor-result))
	 (Assert (equal one-ceiling-result (multiple-value-list
					    (ceiling first)))
		 (format "checking (ceiling %S) gives %S"
			 first one-ceiling-result))
	 (Assert (equal one-ceiling-result (multiple-value-list
					    (ceiling first 1)))
		 (format "checking (ceiling %S 1) gives %S"
			 first one-ceiling-result))
	 (Check-Error arith-error (ceiling first 0))
	 (Check-Error arith-error (ceiling first 0.0))
	 (Assert (equal two-ceiling-result (multiple-value-list
					    (ceiling first second)))
		 (format "checking (ceiling %S %S) gives %S"
			 first second two-ceiling-result))
	 (Assert (equal (cl-ceiling first second)
			(multiple-value-list (ceiling first second)))
		 (format
		  "checking (ceiling %S %S) gives the same as the old code"
		  first second))
	 (Assert (equal one-fceiling-result (multiple-value-list
					     (fceiling first)))
		 (format "checking (fceiling %S) gives %S"
			 first one-fceiling-result))
	 (Assert (equal one-fceiling-result (multiple-value-list
					     (fceiling first 1)))
		 (format "checking (fceiling %S 1) gives %S"
			 first one-fceiling-result))
	 (Check-Error arith-error (fceiling first 0))
	 (Check-Error arith-error (fceiling first 0.0))
	 (Assert (equal two-fceiling-result (multiple-value-list
					  (fceiling first second)))
		 (format "checking (fceiling %S %S) gives %S"
			 first second two-fceiling-result))
	 (Assert (equal one-round-result (multiple-value-list
					  (round first)))
		 (format "checking (round %S) gives %S"
			 first one-round-result))
	 (Assert (equal one-round-result (multiple-value-list
					  (round first 1)))
		 (format "checking (round %S 1) gives %S"
			 first one-round-result))
	 (Check-Error arith-error (round first 0))
	 (Check-Error arith-error (round first 0.0))
	 (Assert (equal two-round-result (multiple-value-list
					  (round first second)))
		 (format "checking (round %S %S) gives %S"
			 first second two-round-result))
	 (Assert (equal one-fround-result (multiple-value-list
					   (fround first)))
		 (format "checking (fround %S) gives %S"
			 first one-fround-result))
	 (Assert (equal one-fround-result (multiple-value-list
					   (fround first 1)))
		 (format "checking (fround %S 1) gives %S"
			 first one-fround-result))
	 (Check-Error arith-error (fround first 0))
	 (Check-Error arith-error (fround first 0.0))
	 (Assert (equal two-fround-result (multiple-value-list
					   (fround first second)))
		 (format "checking (fround %S %S) gives %S"
			 first second two-fround-result))
	 (Assert (equal (cl-round first second)
			(multiple-value-list (round first second)))
		 (format
		  "checking (round %S %S) gives the same as the old code"
		  first second))
	 (Assert (equal one-truncate-result (multiple-value-list
					     (truncate first)))
		 (format "checking (truncate %S) gives %S"
			 first one-truncate-result))
	 (Assert (equal one-truncate-result (multiple-value-list
					     (truncate first 1)))
		 (format "checking (truncate %S 1) gives %S"
			 first one-truncate-result))
	 (Check-Error arith-error (truncate first 0))
	 (Check-Error arith-error (truncate first 0.0))
	 (Assert (equal two-truncate-result (multiple-value-list
					     (truncate first second)))
		 (format "checking (truncate %S %S) gives %S"
			 first second two-truncate-result))
	 (Assert (equal (cl-truncate first second)
			(multiple-value-list (truncate first second)))
		 (format
		  "checking (truncate %S %S) gives the same as the old code"
		  first second))
	 (Assert (equal one-ftruncate-result (multiple-value-list
					      (ftruncate first)))
		 (format "checking (ftruncate %S) gives %S"
			 first one-ftruncate-result))
	 (Assert (equal one-ftruncate-result (multiple-value-list
					      (ftruncate first 1)))
		 (format "checking (ftruncate %S 1) gives %S"
			 first one-ftruncate-result))
	 (Check-Error arith-error (ftruncate first 0))
	 (Check-Error arith-error (ftruncate first 0.0))
	 (Assert (equal two-ftruncate-result (multiple-value-list
					      (ftruncate first second)))
		 (format "checking (ftruncate %S %S) gives %S"
			 first second two-ftruncate-result)))
       (Assert-rounding-floating (pie ee)
	 (let ((pie-type (type-of pie)))
	   (assert (eq pie-type (type-of ee)) t
		   "This code assumes the two arguments have the same type.")
	   (Assert-rounding pie ee
  	    :one-floor-result (list 3 (- pie 3))
            :two-floor-result (list 1 (- pie (* 1 ee)))
            :one-ffloor-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-ffloor-result (list (coerce 1 pie-type) (- pie (* 1.0 ee)))
            :one-ceiling-result (list 4 (- pie 4))
            :two-ceiling-result (list 2 (- pie (* 2 ee)))
            :one-fceiling-result (list (coerce 4 pie-type) (- pie 4.0))
            :two-fceiling-result (list (coerce 2 pie-type) (- pie (* 2.0 ee)))
            :one-round-result (list 3 (- pie 3))
            :two-round-result (list 1 (- pie (* 1 ee)))
            :one-fround-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-fround-result (list (coerce 1 pie-type) (- pie (* 1.0 ee)))
            :one-truncate-result (list 3 (- pie 3))
            :two-truncate-result (list 1 (- pie (* 1 ee)))
            :one-ftruncate-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-ftruncate-result (list (coerce 1 pie-type)
					(- pie (* 1.0 ee))))
  	 (Assert-rounding pie (- ee)
            :one-floor-result (list 3 (- pie 3))
            :two-floor-result (list -2 (- pie (* -2 (- ee))))
            :one-ffloor-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-ffloor-result (list (coerce -2 pie-type)
				     (- pie (* -2.0 (- ee))))
            :one-ceiling-result (list 4 (- pie 4))
            :two-ceiling-result (list -1 (- pie (* -1 (- ee))))
            :one-fceiling-result (list (coerce 4 pie-type) (- pie 4.0))
            :two-fceiling-result (list (coerce -1 pie-type)
				       (- pie (* -1.0 (- ee))))
            :one-round-result (list 3 (- pie 3))
            :two-round-result (list -1 (- pie (* -1 (- ee))))
            :one-fround-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-fround-result (list (coerce -1 pie-type)
				     (- pie (* -1.0 (- ee))))
            :one-truncate-result (list 3 (- pie 3))
            :two-truncate-result (list -1 (- pie (* -1 (- ee))))
            :one-ftruncate-result (list (coerce 3 pie-type) (- pie 3.0))
            :two-ftruncate-result (list (coerce -1 pie-type)
					(- pie (* -1.0 (- ee)))))
  	 (Assert-rounding (- pie) ee
            :one-floor-result (list -4 (- (- pie) -4))
            :two-floor-result (list -2 (- (- pie) (* -2 ee)))
            :one-ffloor-result (list (coerce -4 pie-type) (- (- pie) -4.0))
            :two-ffloor-result (list (coerce -2 pie-type)
				     (- (- pie) (* -2.0 ee)))
            :one-ceiling-result (list -3 (- (- pie) -3))
            :two-ceiling-result (list -1 (- (- pie) (* -1 ee)))
            :one-fceiling-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-fceiling-result (list (coerce -1 pie-type)
				       (- (- pie) (* -1.0 ee)))
            :one-round-result (list -3 (- (- pie) -3))
            :two-round-result (list -1 (- (- pie) (* -1 ee)))
            :one-fround-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-fround-result (list (coerce -1 pie-type)
				     (- (- pie) (* -1.0 ee)))
            :one-truncate-result (list -3 (- (- pie) -3))
            :two-truncate-result (list -1 (- (- pie) (* -1 ee)))
            :one-ftruncate-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-ftruncate-result (list (coerce -1 pie-type)
					(- (- pie) (* -1.0 ee))))
  	 (Assert-rounding (- pie) (- ee)
            :one-floor-result (list -4 (- (- pie) -4))
            :two-floor-result (list 1 (- (- pie) (* 1 (- ee))))
            :one-ffloor-result (list (coerce -4 pie-type) (- (- pie) -4.0))
            :two-ffloor-result (list (coerce 1 pie-type)
				     (- (- pie) (* 1.0 (- ee))))
            :one-ceiling-result (list -3 (- (- pie) -3))
            :two-ceiling-result (list 2 (- (- pie) (* 2 (- ee))))
            :one-fceiling-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-fceiling-result (list (coerce 2 pie-type)
				       (- (- pie) (* 2.0 (- ee))))
            :one-round-result (list -3 (- (- pie) -3))
            :two-round-result (list 1 (- (- pie) (* 1 (- ee))))
            :one-fround-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-fround-result (list (coerce 1 pie-type)
				     (- (- pie) (* 1.0 (- ee))))
            :one-truncate-result (list -3 (- (- pie) -3))
            :two-truncate-result (list 1 (- (- pie) (* 1 (- ee))))
            :one-ftruncate-result (list (coerce -3 pie-type) (- (- pie) -3.0))
            :two-ftruncate-result (list (coerce 1 pie-type)
					(- (- pie) (* 1.0 (- ee)))))
  	 (Assert-rounding ee pie
            :one-floor-result (list 2 (- ee 2))
            :two-floor-result (list 0 ee)
            :one-ffloor-result (list (coerce 2 pie-type) (- ee 2.0))
            :two-ffloor-result (list (coerce 0 pie-type) ee)
            :one-ceiling-result (list 3 (- ee 3))
            :two-ceiling-result (list 1 (- ee pie))
            :one-fceiling-result (list (coerce 3 pie-type) (- ee 3.0))
            :two-fceiling-result (list (coerce 1 pie-type) (- ee pie))
            :one-round-result (list 3 (- ee 3))
            :two-round-result (list 1 (- ee (* 1 pie)))
            :one-fround-result (list (coerce 3 pie-type) (- ee 3.0))
            :two-fround-result (list (coerce 1 pie-type) (- ee (* 1.0 pie)))
            :one-truncate-result (list 2 (- ee 2))
            :two-truncate-result (list 0 ee)
            :one-ftruncate-result (list (coerce 2 pie-type) (- ee 2.0))
            :two-ftruncate-result (list (coerce 0 pie-type) ee))
  	 (Assert-rounding ee (- pie)
            :one-floor-result (list 2 (- ee 2))
            :two-floor-result (list -1 (- ee (* -1 (- pie))))
            :one-ffloor-result (list (coerce 2 pie-type) (- ee 2.0))
            :two-ffloor-result (list (coerce -1 pie-type)
				     (- ee (* -1.0 (- pie))))
            :one-ceiling-result (list 3 (- ee 3))
            :two-ceiling-result (list 0 ee)
            :one-fceiling-result (list (coerce 3 pie-type) (- ee 3.0))
            :two-fceiling-result (list (coerce 0 pie-type) ee)
            :one-round-result (list 3 (- ee 3))
            :two-round-result (list -1 (- ee (* -1 (- pie))))
            :one-fround-result (list (coerce 3 pie-type) (- ee 3.0))
            :two-fround-result (list (coerce -1 pie-type)
				     (- ee (* -1.0 (- pie))))
            :one-truncate-result (list 2 (- ee 2))
            :two-truncate-result (list 0 ee)
            :one-ftruncate-result (list (coerce 2 pie-type) (- ee 2.0))
            :two-ftruncate-result (list (coerce 0 pie-type) ee)))))
    ;; First, two integers: 
  (Assert-rounding 27 8 :one-floor-result '(27 0) :two-floor-result '(3 3)
    :one-ffloor-result '(27.0 0) :two-ffloor-result '(3.0 3)
    :one-ceiling-result '(27 0) :two-ceiling-result '(4 -5)
    :one-fceiling-result '(27.0 0) :two-fceiling-result '(4.0 -5)
    :one-round-result '(27 0) :two-round-result '(3 3)
    :one-fround-result '(27.0 0) :two-fround-result '(3.0 3)
    :one-truncate-result '(27 0) :two-truncate-result '(3 3)
    :one-ftruncate-result '(27.0 0) :two-ftruncate-result '(3.0 3))
  (Assert-rounding 27 -8 :one-floor-result '(27 0)  :two-floor-result '(-4 -5)
    :one-ffloor-result '(27.0 0) :two-ffloor-result '(-4.0 -5) 
    :one-ceiling-result '(27 0) :two-ceiling-result '(-3 3)
    :one-fceiling-result '(27.0 0)  :two-fceiling-result '(-3.0 3)
    :one-round-result '(27 0) :two-round-result '(-3 3)
    :one-fround-result '(27.0 0) :two-fround-result '(-3.0 3)
    :one-truncate-result '(27 0) :two-truncate-result '(-3 3)
    :one-ftruncate-result '(27.0 0)  :two-ftruncate-result '(-3.0 3))
  (Assert-rounding -27 8
    :one-floor-result '(-27 0) :two-floor-result '(-4 5)
    :one-ffloor-result '(-27.0 0) :two-ffloor-result '(-4.0 5)
    :one-ceiling-result '(-27 0) :two-ceiling-result '(-3 -3)
    :one-fceiling-result '(-27.0 0) :two-fceiling-result '(-3.0 -3)
    :one-round-result '(-27 0) :two-round-result '(-3 -3)
    :one-fround-result '(-27.0 0) :two-fround-result '(-3.0 -3)
    :one-truncate-result '(-27 0) :two-truncate-result '(-3 -3)
    :one-ftruncate-result '(-27.0 0) :two-ftruncate-result '(-3.0 -3))
  (Assert-rounding -27 -8
    :one-floor-result '(-27 0) :two-floor-result '(3 -3)
    :one-ffloor-result '(-27.0 0) :two-ffloor-result '(3.0 -3)
    :one-ceiling-result '(-27 0) :two-ceiling-result '(4 5)
    :one-fceiling-result '(-27.0 0) :two-fceiling-result '(4.0 5)
    :one-round-result '(-27 0) :two-round-result '(3 -3)
    :one-fround-result '(-27.0 0) :two-fround-result '(3.0 -3)
    :one-truncate-result '(-27 0) :two-truncate-result '(3 -3)
    :one-ftruncate-result '(-27.0 0) :two-ftruncate-result '(3.0 -3))
  (Assert-rounding 8 27
    :one-floor-result '(8 0) :two-floor-result '(0 8)
    :one-ffloor-result '(8.0 0) :two-ffloor-result '(0.0 8)
    :one-ceiling-result '(8 0) :two-ceiling-result '(1 -19)
    :one-fceiling-result '(8.0 0) :two-fceiling-result '(1.0 -19)
    :one-round-result '(8 0) :two-round-result '(0 8)
    :one-fround-result '(8.0 0) :two-fround-result '(0.0 8)
    :one-truncate-result '(8 0) :two-truncate-result '(0 8)
    :one-ftruncate-result '(8.0 0) :two-ftruncate-result '(0.0 8))
  (Assert-rounding 8 -27
    :one-floor-result '(8 0) :two-floor-result '(-1 -19)
    :one-ffloor-result '(8.0 0) :two-ffloor-result '(-1.0 -19)
    :one-ceiling-result '(8 0) :two-ceiling-result '(0 8)
    :one-fceiling-result '(8.0 0) :two-fceiling-result '(0.0 8)
    :one-round-result '(8 0) :two-round-result '(0 8)
    :one-fround-result '(8.0 0) :two-fround-result '(0.0 8)
    :one-truncate-result '(8 0) :two-truncate-result '(0 8)
    :one-ftruncate-result '(8.0 0) :two-ftruncate-result '(0.0 8))
  (Assert-rounding -8 27
    :one-floor-result '(-8 0) :two-floor-result '(-1 19)
    :one-ffloor-result '(-8.0 0) :two-ffloor-result '(-1.0 19)
    :one-ceiling-result '(-8 0) :two-ceiling-result '(0 -8)
    :one-fceiling-result '(-8.0 0) :two-fceiling-result '(0.0 -8)
    :one-round-result '(-8 0) :two-round-result '(0 -8)
    :one-fround-result '(-8.0 0) :two-fround-result '(0.0 -8)
    :one-truncate-result '(-8 0) :two-truncate-result '(0 -8)
    :one-ftruncate-result '(-8.0 0) :two-ftruncate-result '(0.0 -8))
  (Assert-rounding -8 -27
    :one-floor-result '(-8 0) :two-floor-result '(0 -8)
    :one-ffloor-result '(-8.0 0) :two-ffloor-result '(0.0 -8)
    :one-ceiling-result '(-8 0) :two-ceiling-result '(1 19)
    :one-fceiling-result '(-8.0 0) :two-fceiling-result '(1.0 19)
    :one-round-result '(-8 0) :two-round-result '(0 -8)
    :one-fround-result '(-8.0 0) :two-fround-result '(0.0 -8)
    :one-truncate-result '(-8 0) :two-truncate-result '(0 -8)
    :one-ftruncate-result '(-8.0 0) :two-ftruncate-result '(0.0 -8))
  (Assert-rounding 32 4
    :one-floor-result '(32 0) :two-floor-result '(8 0)
    :one-ffloor-result '(32.0 0) :two-ffloor-result '(8.0 0)
    :one-ceiling-result '(32 0) :two-ceiling-result '(8 0)
    :one-fceiling-result '(32.0 0) :two-fceiling-result '(8.0 0)
    :one-round-result '(32 0) :two-round-result '(8 0)
    :one-fround-result '(32.0 0) :two-fround-result '(8.0 0)
    :one-truncate-result '(32 0) :two-truncate-result '(8 0)
    :one-ftruncate-result '(32.0 0) :two-ftruncate-result '(8.0 0))
  (Assert-rounding 32 -4
    :one-floor-result '(32 0) :two-floor-result '(-8 0)
    :one-ffloor-result '(32.0 0) :two-ffloor-result '(-8.0 0)
    :one-ceiling-result '(32 0) :two-ceiling-result '(-8 0)
    :one-fceiling-result '(32.0 0) :two-fceiling-result '(-8.0 0)
    :one-round-result '(32 0) :two-round-result '(-8 0)
    :one-fround-result '(32.0 0) :two-fround-result '(-8.0 0)
    :one-truncate-result '(32 0) :two-truncate-result '(-8 0)
    :one-ftruncate-result '(32.0 0) :two-ftruncate-result '(-8.0 0))
  (Assert-rounding 12 9
    :one-floor-result '(12 0) :two-floor-result '(1 3)
    :one-ffloor-result '(12.0 0) :two-ffloor-result '(1.0 3)
    :one-ceiling-result '(12 0) :two-ceiling-result '(2 -6)
    :one-fceiling-result '(12.0 0) :two-fceiling-result '(2.0 -6)
    :one-round-result '(12 0) :two-round-result '(1 3)
    :one-fround-result '(12.0 0) :two-fround-result '(1.0 3)
    :one-truncate-result '(12 0) :two-truncate-result '(1 3)
    :one-ftruncate-result '(12.0 0) :two-ftruncate-result '(1.0 3))
  (Assert-rounding 10 4
    :one-floor-result '(10 0) :two-floor-result '(2 2)
    :one-ffloor-result '(10.0 0) :two-ffloor-result '(2.0 2)
    :one-ceiling-result '(10 0) :two-ceiling-result '(3 -2)
    :one-fceiling-result '(10.0 0) :two-fceiling-result '(3.0 -2)
    :one-round-result '(10 0) :two-round-result '(2 2)
    :one-fround-result '(10.0 0) :two-fround-result '(2.0 2)
    :one-truncate-result '(10 0) :two-truncate-result '(2 2)
    :one-ftruncate-result '(10.0 0) :two-ftruncate-result '(2.0 2))
  (Assert-rounding 14 4
    :one-floor-result '(14 0) :two-floor-result '(3 2)
    :one-ffloor-result '(14.0 0) :two-ffloor-result '(3.0 2)
    :one-ceiling-result '(14 0) :two-ceiling-result '(4 -2)
    :one-fceiling-result '(14.0 0) :two-fceiling-result '(4.0 -2)
    :one-round-result '(14 0) :two-round-result '(4 -2)
    :one-fround-result '(14.0 0) :two-fround-result '(4.0 -2)
    :one-truncate-result '(14 0) :two-truncate-result '(3 2)
    :one-ftruncate-result '(14.0 0) :two-ftruncate-result '(3.0 2))
  ;; Now, two floats:
  (Assert-rounding-floating pi e)
  (when (featurep 'bigfloat)
    (Assert-rounding-floating (coerce pi 'bigfloat) (coerce e 'bigfloat)))
  (when (featurep 'bignum)
    (assert (not (evenp most-positive-fixnum)) t
      "In the unlikely event that most-positive-fixnum is even, rewrite this.")
    (Assert-rounding (1+ most-positive-fixnum) (* 2 most-positive-fixnum)
      :one-floor-result `(,(1+ most-positive-fixnum) 0)
      :two-floor-result `(0 ,(1+ most-positive-fixnum))
      :one-ffloor-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-ffloor-result `(0.0 ,(1+ most-positive-fixnum))
      :one-ceiling-result `(,(1+ most-positive-fixnum) 0)
      :two-ceiling-result `(1 ,(1+ (- most-positive-fixnum)))
      :one-fceiling-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-fceiling-result `(1.0 ,(1+ (- most-positive-fixnum)))
      :one-round-result `(,(1+ most-positive-fixnum) 0)
      :two-round-result `(1 ,(1+ (- most-positive-fixnum)))
      :one-fround-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-fround-result `(1.0 ,(1+ (- most-positive-fixnum)))
      :one-truncate-result `(,(1+ most-positive-fixnum) 0)
      :two-truncate-result `(0 ,(1+ most-positive-fixnum))
      :one-ftruncate-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-ftruncate-result `(0.0 ,(1+ most-positive-fixnum)))
    (Assert-rounding (1+ most-positive-fixnum) (- (* 2 most-positive-fixnum))
      :one-floor-result `(,(1+ most-positive-fixnum) 0)
      :two-floor-result `(-1 ,(1+ (- most-positive-fixnum)))
      :one-ffloor-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-ffloor-result `(-1.0 ,(1+ (- most-positive-fixnum)))
      :one-ceiling-result `(,(1+ most-positive-fixnum) 0)
      :two-ceiling-result `(0 ,(1+ most-positive-fixnum))
      :one-fceiling-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-fceiling-result `(0.0 ,(1+ most-positive-fixnum))
      :one-round-result `(,(1+ most-positive-fixnum) 0)
      :two-round-result `(-1 ,(1+ (- most-positive-fixnum)))
      :one-fround-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-fround-result `(-1.0 ,(1+ (- most-positive-fixnum)))
      :one-truncate-result `(,(1+ most-positive-fixnum) 0)
      :two-truncate-result `(0 ,(1+ most-positive-fixnum))
      :one-ftruncate-result `(,(float (1+ most-positive-fixnum)) 0)
      :two-ftruncate-result `(0.0 ,(1+ most-positive-fixnum)))
    (Assert-rounding (- (1+ most-positive-fixnum)) (* 2 most-positive-fixnum)
      :one-floor-result `(,(- (1+ most-positive-fixnum)) 0)
      :two-floor-result `(-1 ,(1- most-positive-fixnum))
      :one-ffloor-result `(,(float (- (1+ most-positive-fixnum))) 0)
      :two-ffloor-result `(-1.0 ,(1- most-positive-fixnum))
      :one-ceiling-result `(,(- (1+ most-positive-fixnum)) 0)
      :two-ceiling-result `(0 ,(- (1+ most-positive-fixnum)))
      :one-fceiling-result `(,(float (- (1+ most-positive-fixnum))) 0)
      :two-fceiling-result `(0.0 ,(- (1+ most-positive-fixnum)))
      :one-round-result `(,(- (1+ most-positive-fixnum)) 0)
      :two-round-result `(-1 ,(1- most-positive-fixnum))
      :one-fround-result `(,(float (- (1+ most-positive-fixnum))) 0)
      :two-fround-result `(-1.0 ,(1- most-positive-fixnum))
      :one-truncate-result `(,(- (1+ most-positive-fixnum)) 0)
      :two-truncate-result `(0 ,(- (1+ most-positive-fixnum)))
      :one-ftruncate-result `(,(float (- (1+ most-positive-fixnum))) 0)
      :two-ftruncate-result `(0.0 ,(- (1+ most-positive-fixnum))))
    ;; Test the handling of values with .5: 
    (Assert-rounding (1+ (* 2 most-positive-fixnum)) 2
      :one-floor-result `(,(1+ (* 2 most-positive-fixnum)) 0)
      :two-floor-result `(,most-positive-fixnum 1)
      :one-ffloor-result `(,(float (1+ (* 2 most-positive-fixnum))) 0)
      ;; We can't just call #'float here; we must use code that converts a
      ;; bignum with value most-positive-fixnum (the creation of which is
      ;; not directly possible in Lisp) to a float, not code that converts
      ;; the fixnum with value most-positive-fixnum to a float. The eval is
      ;; to avoid compile-time optimisation that can break this.
      :two-ffloor-result `(,(eval '(- (1+ most-positive-fixnum) 1 0.0)) 1)
      :one-ceiling-result `(,(1+ (* 2 most-positive-fixnum)) 0)
      :two-ceiling-result `(,(1+ most-positive-fixnum) -1)
      :one-fceiling-result `(,(float (1+ (* 2 most-positive-fixnum))) 0)
      :two-fceiling-result `(,(float (1+ most-positive-fixnum)) -1)
      :one-round-result `(,(1+ (* 2 most-positive-fixnum)) 0)
      :two-round-result `(,(1+ most-positive-fixnum) -1)
      :one-fround-result `(,(float (1+ (* 2 most-positive-fixnum))) 0)
      :two-fround-result `(,(float (1+ most-positive-fixnum)) -1)
      :one-truncate-result `(,(1+ (* 2 most-positive-fixnum)) 0)
      :two-truncate-result `(,most-positive-fixnum 1)
      :one-ftruncate-result `(,(float (1+ (* 2 most-positive-fixnum))) 0)
      ;; See the comment above on :two-ffloor-result:
      :two-ftruncate-result `(,(eval '(- (1+ most-positive-fixnum) 1 0.0)) 1))
    (Assert-rounding (1+ (* 2 (1- most-positive-fixnum))) 2
      :one-floor-result `(,(1+ (* 2 (1- most-positive-fixnum))) 0)
      :two-floor-result `(,(1- most-positive-fixnum) 1)
      :one-ffloor-result `(,(float (1+ (* 2 (1- most-positive-fixnum)))) 0)
      ;; See commentary above on float conversions.
      :two-ffloor-result `(,(eval '(- (1+ most-positive-fixnum) 2 0.0)) 1)
      :one-ceiling-result `(,(1+ (* 2 (1- most-positive-fixnum))) 0)
      :two-ceiling-result `(,most-positive-fixnum -1)
      :one-fceiling-result `(,(float (1+ (* 2 (1- most-positive-fixnum)))) 0)
      :two-fceiling-result `(,(eval '(- (1+ most-positive-fixnum) 1 0.0)) -1)
      :one-round-result `(,(1+ (* 2 (1- most-positive-fixnum))) 0)
      :two-round-result `(,(1- most-positive-fixnum) 1)
      :one-fround-result `(,(float (1+ (* 2 (1- most-positive-fixnum)))) 0)
      :two-fround-result `(,(eval '(- (1+ most-positive-fixnum) 2 0.0)) 1)
      :one-truncate-result `(,(1+ (* 2 (1- most-positive-fixnum))) 0)
      :two-truncate-result `(,(1- most-positive-fixnum) 1)
      :one-ftruncate-result `(,(float (1+ (* 2 (1- most-positive-fixnum)))) 0)
      ;; See commentary above
      :two-ftruncate-result `(,(eval '(- (1+ most-positive-fixnum) 2 0.0))
			      1)))
  (when (featurep 'ratio)
    (Assert-rounding (read "4/3") (read "8/7")
     :one-floor-result '(1 1/3) :two-floor-result '(1 4/21)
     :one-ffloor-result '(1.0 1/3) :two-ffloor-result '(1.0 4/21)
     :one-ceiling-result '(2 -2/3) :two-ceiling-result '(2 -20/21)
     :one-fceiling-result '(2.0 -2/3) :two-fceiling-result '(2.0 -20/21)
     :one-round-result '(1 1/3) :two-round-result '(1 4/21)
     :one-fround-result '(1.0 1/3) :two-fround-result '(1.0 4/21)
     :one-truncate-result '(1 1/3) :two-truncate-result '(1 4/21)
     :one-ftruncate-result '(1.0 1/3) :two-ftruncate-result '(1.0 4/21))
    (Assert-rounding (read "-4/3") (read "8/7")
     :one-floor-result '(-2 2/3) :two-floor-result '(-2 20/21)
     :one-ffloor-result '(-2.0 2/3) :two-ffloor-result '(-2.0 20/21)
     :one-ceiling-result '(-1 -1/3) :two-ceiling-result '(-1 -4/21)
     :one-fceiling-result '(-1.0 -1/3) :two-fceiling-result '(-1.0 -4/21)
     :one-round-result '(-1 -1/3) :two-round-result '(-1 -4/21)
     :one-fround-result '(-1.0 -1/3) :two-fround-result '(-1.0 -4/21)
     :one-truncate-result '(-1 -1/3) :two-truncate-result '(-1 -4/21)
     :one-ftruncate-result '(-1.0 -1/3) :two-ftruncate-result '(-1.0 -4/21))))

;; Run this function in a Common Lisp with two arguments to get results that
;; we should compare against, above. Though note the dancing-around with the
;; bigfloats and bignums above, too; you can't necessarily just use the
;; output here.

(defun generate-rounding-output (first second)
  (let ((print-readably t))
    (princ first)
    (princ " ")
    (princ second)
    (princ " :one-floor-result ")
    (princ (list 'quote (multiple-value-list (floor first))))
    (princ " :two-floor-result ")
    (princ (list 'quote (multiple-value-list (floor first second))))
    (princ " :one-ffloor-result ")
    (princ (list 'quote (multiple-value-list (ffloor first))))
    (princ " :two-ffloor-result ")
    (princ (list 'quote (multiple-value-list (ffloor first second))))
    (princ " :one-ceiling-result ")
    (princ (list 'quote (multiple-value-list (ceiling first))))
    (princ " :two-ceiling-result ")
    (princ (list 'quote (multiple-value-list (ceiling first second))))
    (princ " :one-fceiling-result ")
    (princ (list 'quote (multiple-value-list (fceiling first))))
    (princ " :two-fceiling-result ")
    (princ (list 'quote (multiple-value-list (fceiling first second))))
    (princ " :one-round-result ")
    (princ (list 'quote (multiple-value-list (round first))))
    (princ " :two-round-result ")
    (princ (list 'quote (multiple-value-list (round first second))))
    (princ " :one-fround-result ")
    (princ (list 'quote (multiple-value-list (fround first))))
    (princ " :two-fround-result ")
    (princ (list 'quote (multiple-value-list (fround first second))))
    (princ " :one-truncate-result ")
    (princ (list 'quote (multiple-value-list (truncate first))))
    (princ " :two-truncate-result ")
    (princ (list 'quote (multiple-value-list (truncate first second))))
    (princ " :one-ftruncate-result ")
    (princ (list 'quote (multiple-value-list (ftruncate first))))
    (princ " :two-ftruncate-result ")
    (princ (list 'quote (multiple-value-list (ftruncate first second))))))

;; Multiple value tests. 

(flet ((foo (x y) 
	 (floor (+ x y) y))
       (foo-zero (x y)
	 (values (floor (+ x y) y)))
       (multiple-value-function-returning-t ()
	 (values t pi e degrees-to-radians radians-to-degrees))
       (multiple-value-function-returning-nil ()
	 (values nil pi e radians-to-degrees degrees-to-radians))
       (function-throwing-multiple-values ()
	 (let* ((listing '(0 3 4 nil "string" symbol))
		(tail listing)
		elt)
	   (while t
	     (setq tail (cdr listing)
		   elt (car listing)
		   listing tail)
	     (when (null elt)
	       (throw 'VoN61Lo4Y (multiple-value-function-returning-t)))))))
  (Assert
   (= (+ (floor 5 3) (floor 19 4)) (+ 1 4) 5)
   "Checking that multiple values are discarded correctly as func args")
  (Assert
   (= 2 (length (multiple-value-list (foo 400 (1+ most-positive-fixnum)))))
   "Checking multiple values are passed through correctly as return values")
  (Assert
   (= 1 (length (multiple-value-list
		 (foo-zero 400 (1+ most-positive-fixnum)))))
   "Checking multiple values are discarded correctly when forced")
  (Check-Error setting-constant (setq multiple-values-limit 20))
  (Assert (equal '(-1 1)
		 (multiple-value-list (floor -3 4)))
	  "Checking #'multiple-value-list gives a sane result")
  (let ((ey 40000)
	(bee "this is a string")
	(cee #s(hash-table size 256 data (969 ?\xF9))))
    (Assert (equal
	     (multiple-value-list (values ey bee cee))
	     (multiple-value-list (values-list (list ey bee cee))))
	    "Checking that #'values and #'values-list are correctly related")
    (Assert (equal
	     (multiple-value-list (values-list (list ey bee cee)))
	     (multiple-value-list (apply #'values (list ey bee cee))))
	    "Checking #'values-list and #'apply with #values are correctly related"))
  (Assert (= (multiple-value-call #'+ (floor 5 3) (floor 19 4)) 10)
	  "Checking #'multiple-value-call gives reasonable results.")
  (Assert (= (multiple-value-call (values '+ '*) (floor 5 3) (floor 19 4)) 10)
	  "Checking #'multiple-value-call correct when first arg multiple.")
  (Assert (= 1 (length (multiple-value-list (prog1 (floor pi) "hi there"))))
	  "Checking #'prog1 does not pass back multiple values")
  (Assert (= 2 (length (multiple-value-list
			(multiple-value-prog1 (floor pi) "hi there"))))
	  "Checking #'multiple-value-prog1 passes back multiple values")
  (multiple-value-bind (floored remainder this-is-nil)
      (floor pi 1.0)
    (Assert (= floored 3)
	    "Checking floored bound correctly")
    (Assert (eql remainder (- pi 3.0))
	    "Checking remainder bound correctly") 
    (Assert (null this-is-nil)
	    "Checking trailing arg bound but nil"))
  (let ((ey 40000)
	(bee "this is a string")
	(cee #s(hash-table size 256 data (969 ?\xF9))))
    (multiple-value-setq (ey bee cee)
      (ffloor e 1.0))
    (Assert (eql 2.0 ey) "Checking ey set correctly")
    (Assert (eql bee (- e 2.0)) "Checking bee set correctly")
    (Assert (null cee) "Checking cee set to nil correctly"))
  (Assert (= 3 (length (multiple-value-list (eval '(values nil t pi)))))
	  "Checking #'eval passes back multiple values")
  (Assert (= 2 (length (multiple-value-list (apply #'floor '(5 3)))))
	  "Checking #'apply passes back multiple values")
  (Assert (= 2 (length (multiple-value-list (funcall #'floor 5 3))))
	  "Checking #'funcall passes back multiple values")
  (Assert (equal '(1 2) (multiple-value-list 
			 (multiple-value-call #'floor (values 5 3))))
	  "Checking #'multiple-value-call passes back multiple values correctly")
  (Assert (= 1 (length (multiple-value-list
			(and (multiple-value-function-returning-nil) t))))
	  "Checking multiple values from non-trailing forms discarded by #'and")
  (Assert (= 5 (length (multiple-value-list 
			(and t (multiple-value-function-returning-nil)))))
	  "Checking multiple values from final forms not discarded by #'and")
  (Assert (= 1 (length (multiple-value-list
			(or (multiple-value-function-returning-t) t))))
	  "Checking multiple values from non-trailing forms discarded by #'and")
  (Assert (= 5 (length (multiple-value-list 
			(or nil (multiple-value-function-returning-t)))))
	  "Checking multiple values from final forms not discarded by #'and")
  (Assert (= 1 (length (multiple-value-list
			(cond ((multiple-value-function-returning-t))))))
	  "Checking cond doesn't pass back multiple values in tests.")
  (Assert (equal (list nil pi e radians-to-degrees degrees-to-radians)
		 (multiple-value-list
		  (cond (t (multiple-value-function-returning-nil)))))
	  "Checking cond passes back multiple values in clauses.")
  (Assert (= 1 (length (multiple-value-list
			(prog1 (multiple-value-function-returning-nil)))))
	  "Checking prog1 discards multiple values correctly.")
  (Assert (= 5 (length (multiple-value-list
			(multiple-value-prog1
			 (multiple-value-function-returning-nil)))))
	  "Checking multiple-value-prog1 passes back multiple values correctly.")
  (Assert (equal (list t pi e degrees-to-radians radians-to-degrees)
	  (multiple-value-list
	   (catch 'VoN61Lo4Y (function-throwing-multiple-values)))))
  (Assert (equal (list t pi e degrees-to-radians radians-to-degrees)
	  (multiple-value-list
	   (loop
	     for eye in `(a b c d ,e f g ,nil ,pi)
	     do (when (null eye)
		  (return (multiple-value-function-returning-t))))))
   "Checking #'loop passes back multiple values correctly.")
  (Assert
   (null (or))
   "Checking #'or behaves correctly with zero arguments.")
  (Assert (eq t (and))
   "Checking #'and behaves correctly with zero arguments.")
  (Assert (= (* 3.0 (- pi 3.0))
      (letf (((values three one-four-one-five-nine) (floor pi)))
        (* three one-four-one-five-nine)))
   "checking letf handles #'values in a basic sense"))

;; #'equalp tests.
(let ((string-variable "aBcDeeFgH\u00Edj")
      (eacute-character ?\u00E9)
      (Eacute-character ?\u00c9)
      (+base-chars+ (loop
		      with res = (make-string 96 ?\x20)
		      for int-char from #x20 to #x7f
		      for char being each element in-ref res
		      do (setf char (int-to-char int-char))
		      finally return res)))

  (macrolet
      ((equalp-equal-list-tests (equal-list)
	 (let (res)
	   (setq equal-lists (eval equal-list))
	   (loop for li in equal-lists do
	     (loop for (x . tail) on li do
	       (loop for y in tail do
		 (push `(Assert (equalp ,(quote-maybe x)
					,(quote-maybe y))) res)
		 (push `(Assert (equalp ,(quote-maybe y)
					,(quote-maybe x))) res)
                 (push `(Assert (eql (equalp-hash ,(quote-maybe y))
                                     (equalp-hash ,(quote-maybe x))))
                       res))))
	   (cons 'progn (nreverse res))))
       (equalp-diff-list-tests (diff-list)
	 (let (res)
	   (setq diff-list (eval diff-list))
	   (loop for (x . tail) on diff-list do
	     (loop for y in tail do
	       (push `(Assert (not (equalp ,(quote-maybe x)
					   ,(quote-maybe y)))) res)
	       (push `(Assert (not (equalp ,(quote-maybe y)
					   ,(quote-maybe x)))) res)))
	   (cons 'progn (nreverse res))))
       (Assert-equalp (object-one object-two &optional failing-case description)
         `(progn
           (Assert (equalp ,object-one ,object-two)
                   ,@(if failing-case
                         (list failing-case description)))
           (Assert (eql (equalp-hash ,object-one) (equalp-hash ,object-two))))))
    (equalp-equal-list-tests
     `(,@(when (featurep 'bignum)
	  (read "((111111111111111111111111111111111111111111111111111
		111111111111111111111111111111111111111111111111111.0))"))
       (0 0.0 0.000 -0 -0.0 -0.000 #b0 ,@(when (featurep 'ratio) '(0/5 -0/5)))
       (21845 #b101010101010101 #x5555)
       (1.5 1.500000000000000000000000000000000000000000000000000000000
	    ,@(when (featurep 'ratio) '(3/2)))
       ;; Can't use this, these values aren't `='.
       ;;(-12345678901234567890123457890123457890123457890123457890123457890
       ;; -12345678901234567890123457890123457890123457890123457890123457890.0)
       (-55 -55.000 ,@(when (featurep 'ratio) '(-110/2)))))
    (equalp-diff-list-tests
     `(0 1 2 3 1000 5000000000
       ,@(when (featurep 'bignum)
	   (read "(5555555555555555555555555555555555555
                       -5555555555555555555555555555555555555)"))
       -1 -2 -3 -1000 -5000000000 
       1/2 1/3 2/3 8/2 355/113
       ,@(when (featurep 'ratio) (mapcar* #'/ '(3/2 3/2) '(0.2 0.7)))
       55555555555555555555555555555555555555555/2718281828459045
       0.111111111111111111111111111111111111111111111111111111111111111
       1e+300 1e+301 -1e+300 -1e+301))

    (Assert-equalp "hi there" "Hi There"
                   "checking equalp isn't case-sensitive")
    (Assert-equalp
     99 99.0
     "checking equalp compares numerical values of different types")
    (Assert (null (equalp 99 ?c))
            "checking equalp does not convert characters to numbers")
    ;; Fixed in Hg d0ea57eb3de4.
    (Assert (null (equalp "hi there" [hi there]))
            "checking equalp doesn't error with string and non-string")
    (Assert-equalp
     "ABCDEEFGH\u00CDJ" string-variable
     "checking #'equalp is case-insensitive with an upcased constant") 
    (Assert-equalp
     "abcdeefgh\xedj" string-variable
     "checking #'equalp is case-insensitive with a downcased constant")
    (Assert-equalp string-variable string-variable
                   "checking #'equalp works when handed the same string twice")
    (Assert (equalp string-variable "aBcDeeFgH\u00Edj")
            "check #'equalp is case-insensitive with a variable-cased constant")
    (Assert-equalp "" (bit-vector)
                   "check empty string and empty bit-vector are #'equalp.")
    (Assert-equalp
     (string) (bit-vector)
     "check empty string and empty bit-vector are #'equalp, no constants")
    (Assert-equalp "hi there" (vector ?h ?i ?\  ?t ?h ?e ?r ?e)
                   "check string and vector with same contents #'equalp")
    (Assert-equalp
     (string ?h ?i ?\  ?t ?h ?e ?r ?e)
     (vector ?h ?i ?\  ?t ?h ?e ?r ?e)
     "check string and vector with same contents #'equalp, no constants")
    (Assert-equalp
     [?h ?i ?\  ?t ?h ?e ?r ?e]
     (string ?h ?i ?\  ?t ?h ?e ?r ?e)
     "check string and vector with same contents #'equalp, vector constant")
    (Assert-equalp [0 1.0 0.0 0 1]
                   (bit-vector 0 1 0 0 1)
                   "check vector and bit-vector with same contents #'equalp,\
 vector constant")
    (Assert (not (equalp [0 2 0.0 0 1]
                  (bit-vector 0 1 0 0 1)))
            "check vector and bit-vector with different contents not #'equalp,\
 vector constant")
    (Assert-equalp #*01001
                   (vector 0 1.0 0.0 0 1)
	  "check vector and bit-vector with same contents #'equalp,\
 bit-vector constant")
    (Assert-equalp ?\u00E9 Eacute-character
                   "checking characters are case-insensitive, one constant")
    (Assert (not (equalp ?\u00E9 (aref (format "%c" ?a) 0)))
            "checking distinct characters are not equalp, one constant")
    (Assert-equalp t (and)
                   "checking symbols are correctly #'equalp")
    (Assert (not (equalp t (or nil '#:t)))
            "checking distinct symbols with the same name are not #'equalp")
    (Assert-equalp #s(char-table type generic data (?\u0080 "hi-there"))
                   (let ((aragh (make-char-table 'generic)))
                     (put-char-table ?\u0080 "hi-there" aragh)
                     aragh)
                   "checking #'equalp succeeds correctly, char-tables")
    (Assert-equalp #s(char-table type generic data (?\u0080 "hi-there"))
                   (let ((aragh (make-char-table 'generic)))
                     (put-char-table ?\u0080 "HI-THERE" aragh)
                     aragh)
                   "checking #'equalp succeeds correctly, char-tables")
    (Assert (not (equalp #s(char-table type generic data (?\u0080 "hi-there"))
                  (let ((aragh (make-char-table 'generic)))
                    (put-char-table ?\u0080 "hi there" aragh)
                    aragh)))
            "checking #'equalp fails correctly, char-tables")))

;; There are more tests available for equalp here: 
;;
;; http://www.parhasard.net/xemacs/equalp-tests.el
;;
;; They are taken from Paul Dietz' GCL ANSI test suite, licensed under the
;; LGPL and part of GNU Common Lisp; the GCL people didn't respond to
;; several requests for information on who owned the copyright for the
;; files, so I haven't included the tests with XEmacs. Anyone doing XEmacs
;; development on equalp should still run them, though. Aidan Kehoe, Thu Dec
;; 31 14:53:52 GMT 2009. 

(loop
  for special-form in '(multiple-value-call setq-default quote throw
			save-current-buffer and or)
  with not-special-form = nil
  do
  (Assert (special-form-p special-form)
	  (format "checking %S is a special operator" special-form))
  (setq not-special-form 
	(intern (format "%s-gMAu" (symbol-name special-form))))
  (Assert (not (special-form-p not-special-form))
	  (format "checking %S is a special operator" special-form))
  (Assert (not (functionp special-form))
	  (format "checking %S is not a function" special-form)))

(loop
  for real-function in '(find-file quote-maybe + - find-file-read-only)
  do (Assert (functionp real-function)
	     (format "checking %S is a function" real-function)))

;; #'member, #'assoc tests.

(when (featurep 'bignum)
  (let* ((member*-list `(0 9 342 [hi there] ,(1+ most-positive-fixnum) 0
			 0.0 ,(1- most-negative-fixnum) nil))
	 (assoc*-list (loop
			for elt in member*-list
			collect (cons elt (random))))
	 (hashing (make-hash-table :test 'eql))
	 hashed-bignum)
    (macrolet
	((1+most-positive-fixnum ()
	   (1+ most-positive-fixnum))
	 (1-most-negative-fixnum ()
	   (1- most-negative-fixnum))
	 (*-2-most-positive-fixnum ()
	   (* 2 most-positive-fixnum))) 
      (Assert (eq
	       (member* (1+ most-positive-fixnum) member*-list)
	       (member* (1+ most-positive-fixnum) member*-list :test #'eql))
	      "checking #'member* correct if #'eql not explicitly specified")
      (Assert (eq
	       (assoc* (1+ most-positive-fixnum) assoc*-list)
	       (assoc* (1+ most-positive-fixnum) assoc*-list :test #'eql))
	      "checking #'assoc* correct if #'eql not explicitly specified")
      (Assert (eq
	       (rassoc* (1- most-negative-fixnum) assoc*-list)
	       (rassoc* (1- most-negative-fixnum) assoc*-list :test #'eql))
	      "checking #'rassoc* correct if #'eql not explicitly specified")
      (Assert (eql (1+most-positive-fixnum) (1+ most-positive-fixnum))
	      "checking #'eql handles a bignum literal properly.")
      (Assert (eq 
	       (member* (1+most-positive-fixnum) member*-list)
	       (member* (1+ most-positive-fixnum) member*-list :test #'equal))
	      "checking #'member* compiler macro correct with literal bignum")
      (Assert (eq
	       (assoc* (1+most-positive-fixnum) assoc*-list)
	       (assoc* (1+ most-positive-fixnum) assoc*-list :test #'equal))
	      "checking #'assoc* compiler macro correct with literal bignum")
      (puthash (setq hashed-bignum (*-2-most-positive-fixnum)) 
	       (gensym) hashing)
      (Assert (eq
	       (gethash (* 2 most-positive-fixnum) hashing)
	       (gethash hashed-bignum hashing))
	      "checking hashing works correctly with #'eql tests and bignums"))))

;; 
(when (decode-char 'ucs #x0192)
  (Check-Error
   invalid-state
   (let ((str "aaaaaaaaaaaaa")
	 (called 0)
	 modified)
     (reduce #'+ str
	     :key #'(lambda (object)
		      (prog1
			  object
			(incf called) 
			(or modified
			    (and (> called 5)
				 (setq modified
				       (fill str (read #r"?\u0192")))))))))))

(Assert
 (eql 55
      (let ((sequence '(1 2 3 4 5 6 7 8 9 10))
	    (called 0)
	    modified)
	(reduce #'+
		sequence
		:key
		#'(lambda (object) (prog1
				       object
				     (incf called)
				     (and (eql called 5)
					  (setcdr (nthcdr 3 sequence) nil))
				     (garbage-collect))))))
 "checking we can amputate lists without crashing #'reduce")

(Assert (not (eq t (canonicalize-inst-list
		    `(((mswindows) . [string :data ,(make-string 20 0)])
		      ((tty) . [string :data " "])) 'image t)))
	"checking mswindows is always available as a specifier tag")

(Assert (not (eq t (canonicalize-inst-list
		    `(((mswindows) . [nothing])
		      ((tty) . [string :data " "]))
		    'image t)))
	"checking the correct syntax for a nothing image specifier works")

(Check-Error-Message invalid-argument "^Invalid specifier tag set"
		     (canonicalize-inst-list
		      `(((,(gensym)) . [nothing])
			((tty) . [string :data " "]))
		      'image))

(Check-Error-Message invalid-argument "^Unrecognized keyword"
		     (canonicalize-inst-list
		      `(((mswindows) . [nothing :data "hi there"])
			((tty) . [string :data " "])) 'image))

;; If we combine both the specifier inst list problems, we get the
;; unrecognized keyword error first, not the invalid specifier tag set
;; error. This is a little unintuitive; the specifier tag set thing is
;; processed first, and would seem to be more important. But anyone writing
;; code needs to solve both problems, it's reasonable to ask them to do it
;; in series rather than in parallel.

(when (featurep 'ratio)
  (Assert (not (eql '1/2 (read (prin1-to-string (intern "1/2")))))
	  "checking symbols with ratio-like names are printed distinctly")
  (Assert (not (eql '1/5 (read (prin1-to-string (intern "2/10")))))
	  "checking symbol named \"2/10\" not eql to ratio 1/5 on read"))

(let* ((count 0)
       (list (map-into (make-list 2048 nil) #'(lambda () (decf count))))
       (expected (append list '(1))))
  (Assert (equal expected (merge 'list list '(1) #'<))
	  "checking merge's circularity checks are sane"))

(flet ((list-nreverse (list)
	 (do ((list1 list (cdr list1))
	      (list2 nil (prog1 list1 (setcdr list1 list2))))
	     ((atom list1) list2))))
  (let* ((integers (loop for i from 0 to 6000 collect i))
	 (characters (mapcan #'(lambda (integer)
				 (if (char-int-p integer)
				     (list (int-char integer)))) integers))
	 (fourth-bit #'(lambda (integer) (ash (logand #x10 integer) -4)))
	 (bits (mapcar fourth-bit integers))
	 (vector (vconcat integers))
	 (string (concat characters))
	 (bit-vector (bvconcat bits)))
    (Assert (equal (reverse vector)
	     (vconcat (list-nreverse (copy-list integers)))))
    (Assert (eq vector (nreverse vector)))
    (Assert (equal vector (vconcat (list-nreverse (copy-list integers)))))
    (Assert (equal (reverse string)
	     (concat (list-nreverse (copy-list characters)))))
    (Assert (eq string (nreverse string)))
    (Assert (equal string (concat (list-nreverse (copy-list characters)))))
    (Assert (eq bit-vector (nreverse bit-vector)))
    (Assert (equal (bvconcat (list-nreverse (copy-list bits))) bit-vector))
    (Assert (not (equal bit-vector
			(mapcar fourth-bit
				(loop for i from 0 to 6000 collect i)))))))

(Check-Error wrong-type-argument (self-insert-command 'self-insert-command))
(Check-Error wrong-type-argument (make-list 'make-list 'make-list))
(Check-Error wrong-type-argument (make-vector 'make-vector 'make-vector))
(Check-Error wrong-type-argument (make-bit-vector 'make-bit-vector
						  'make-bit-vector))
(Check-Error wrong-type-argument (make-byte-code '(&rest ignore) "\xc0\x87" [4]
						 'ignore))
(Check-Error wrong-type-argument (make-string ?a ?a))
(Check-Error wrong-type-argument (nth-value 'nth-value (truncate pi e)))
(Check-Error wrong-type-argument (make-hash-table :test #'eql :size :size))
(Check-Error wrong-type-argument
	     (accept-process-output nil 'accept-process-output))
(Check-Error wrong-type-argument
	     (accept-process-output nil 2000 'accept-process-output))
(Check-Error wrong-type-argument
             (self-insert-command 'self-insert-command))
(Check-Error wrong-type-argument (string-to-number "16" 'string-to-number))
(Check-Error wrong-type-argument (move-to-column 'move-to-column))
(stop-profiling)
(Check-Error wrong-type-argument (start-profiling (float most-positive-fixnum)))
(stop-profiling)
(Check-Error wrong-type-argument
             (fill '(1 2 3 4 5) 1 :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill [1 2 3 4 5] 1 :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill "1 2 3 4 5" ?1 :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill #*10101010 1 :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill '(1 2 3 4 5) 1 :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill [1 2 3 4 5] 1 :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill "1 2 3 4 5" ?1 :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (fill #*10101010 1 :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons '(1 2 3 4 5) :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons [1 2 3 4 5] :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons "1 2 3 4 5" :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons #*10101010 :start (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons '(1 2 3 4 5) :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons [1 2 3 4 5] :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons "1 2 3 4 5" :end (float most-positive-fixnum)))
(Check-Error wrong-type-argument
             (reduce #'cons #*10101010 :end (float most-positive-fixnum)))

(when (featurep 'bignum)
  (Check-Error args-out-of-range
	       (self-insert-command (* 2 most-positive-fixnum)))
  (Check-Error args-out-of-range
	       (make-list (* 3 most-positive-fixnum) 'make-list))
  (Check-Error args-out-of-range
	       (make-vector (* 4 most-positive-fixnum) 'make-vector))
  (Check-Error args-out-of-range
	       (make-bit-vector (+ 2 most-positive-fixnum) 'make-bit-vector))
  (Check-Error args-out-of-range
	       (make-byte-code '(&rest ignore) "\xc0\x87" [4]
			       (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
	       (make-byte-code '(&rest ignore) "\xc0\x87" [4]
			       #x10000))
  (Check-Error args-out-of-range
	       (make-string (* 4 most-positive-fixnum) ?a))
  (Check-Error args-out-of-range
	       (nth-value most-positive-fixnum (truncate pi e)))
  (Check-Error args-out-of-range
	       (make-hash-table :test #'equalp :size (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
	       (accept-process-output nil 4294967))
  (Check-Error args-out-of-range
	       (accept-process-output nil 10 (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (self-insert-command (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (string-to-number "16" (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (recent-keys (1+ most-positive-fixnum)))
  (when (featurep 'xbm)
    (Check-Error-Message
     invalid-argument
     "^data is too short for width and height"
     (set-face-background-pixmap
      'left-margin
      `[xbm :data (20 ,(* 2 most-positive-fixnum) "random-text")])))
  (Check-Error args-out-of-range
               (move-to-column (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (move-to-column (1- most-negative-fixnum)))
  (stop-profiling)
  (when (< most-positive-fixnum (lsh 1 32))
    ;; We only support machines with integers of 32 bits or more. If
    ;; most-positive-fixnum is less than 2^32, we're on a 32-bit machine,
    ;; and it's appropriate to test start-profiling with a bignum.
    (Assert (eq nil (start-profiling (* most-positive-fixnum 2)))))
  (stop-profiling)
  (Check-Error args-out-of-range
               (fill '(1 2 3 4 5) 1 :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill [1 2 3 4 5] 1 :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill "1 2 3 4 5" ?1 :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill #*10101010 1 :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill '(1 2 3 4 5) 1 :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill [1 2 3 4 5] 1 :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill "1 2 3 4 5" ?1 :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (fill #*10101010 1 :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons '(1 2 3 4 5) :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons [1 2 3 4 5] :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons "1 2 3 4 5" :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons #*10101010 :start (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons '(1 2 3 4 5) :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons [1 2 3 4 5] :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons "1 2 3 4 5" :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (reduce #'cons #*10101010 :end (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (replace '(1 2 3 4 5) [5 4 3 2 1]
                        :start1 (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (replace '(1 2 3 4 5) [5 4 3 2 1]
                        :start2 (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (replace '(1 2 3 4 5) [5 4 3 2 1]
                        :end1 (1+ most-positive-fixnum)))
  (Check-Error args-out-of-range
               (replace '(1 2 3 4 5) [5 4 3 2 1]
                        :end2 (1+ most-positive-fixnum))))

(symbol-macrolet
    ((list-length 2048) (vector-length 512) (string-length (* 8192 2)))
  (let ((list
         ;; CIRCULAR_LIST_SUSPICION_LENGTH is 1024, it's helpful if this list
         ;; is longer than that.
         (make-list list-length 'make-list)) 
        (vector (make-vector vector-length 'make-vector))
        (bit-vector (make-bit-vector vector-length 1))
        (string (make-string string-length
                             (or (decode-char 'ucs #x20ac) ?\xFF)))
        (item 'cons))
    (macrolet
        ((construct-item-sequence-checks (&rest functions)
           (cons
            'progn
            (mapcan
             #'(lambda (function)
                 `((Check-Error args-out-of-range
                                (,function item list
                                           :start (1+ list-length)
                                           :end (1+ list-length)))
                   (Check-Error wrong-type-argument
                                (,function item list :start -1
                                           :end list-length))
                   (Check-Error args-out-of-range
                                (,function item list :end (* 2 list-length)))
                   (Check-Error args-out-of-range
                                (,function item vector
                                           :start (1+ vector-length)
                                           :end (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function item vector :start -1))
                   (Check-Error args-out-of-range
                                (,function item vector
                                           :end (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function item bit-vector
                                           :start (1+ vector-length)
                                           :end (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function item bit-vector :start -1))
                   (Check-Error args-out-of-range
                                (,function item bit-vector
                                           :end (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function item string
                                           :start (1+ string-length)
                                           :end (1+ string-length)))
                   (Check-Error wrong-type-argument
                                (,function item string :start -1))
                   (Check-Error args-out-of-range
                                (,function item string
                                           :end (* 2 string-length)))))
             functions)))
         (construct-one-sequence-checks (&rest functions)
           (cons
            'progn
            (mapcan
             #'(lambda (function)
                 `((Check-Error args-out-of-range
                                (,function (copy-sequence list)
                                           :start (1+ list-length)
                                           :end (1+ list-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence list)
                                           :start -1 :end list-length))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence list)
                                           :end (* 2 list-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence vector)
                                           :start (1+ vector-length)
                                           :end (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence vector) :start -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence vector)
                                           :end (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence bit-vector)
                                           :start (1+ vector-length)
                                           :end (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence bit-vector)
                                           :start -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence bit-vector)
                                           :end (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence string)
                                           :start (1+ string-length)
                                           :end (1+ string-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence string) :start -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence string)
                                           :end (* 2 string-length)))))
             functions)))
         (construct-two-sequence-checks (&rest functions)
           (cons
            'progn
            (mapcan
             #'(lambda (function)
                 `((Check-Error args-out-of-range
                                (,function (copy-sequence list)
                                           (copy-sequence list)
                                           :start1 (1+ list-length)
                                           :end1 (1+ list-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence list)
                                           (copy-sequence list)
                                           :start1 -1 :end1 list-length))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence list)
                                           (copy-sequence list)
                                           :end1 (* 2 list-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence vector)
                                           (copy-sequence vector)
                                           :start1 (1+ vector-length)
                                           :end1 (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function
                                 (copy-sequence vector)
                                 (copy-sequence vector) :start1 -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence vector)
                                           (copy-sequence vector)
                                           :end1 (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence bit-vector)
                                           (copy-sequence bit-vector)
                                           :start1 (1+ vector-length)
                                           :end1 (1+ vector-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence bit-vector)
                                           (copy-sequence bit-vector)
                                           :start1 -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence bit-vector)
                                           (copy-sequence bit-vector)
                                           :end1 (* 2 vector-length)))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence string)
                                           (copy-sequence string)
                                           :start1 (1+ string-length)
                                           :end1 (1+ string-length)))
                   (Check-Error wrong-type-argument
                                (,function (copy-sequence string)
                                           (copy-sequence string) :start1 -1))
                   (Check-Error args-out-of-range
                                (,function (copy-sequence string)
                                           (copy-sequence string)
                                           :end1 (* 2 string-length)))))
             functions))))
      (construct-item-sequence-checks count position find delete* remove*
                                      reduce)
      (construct-one-sequence-checks delete-duplicates remove-duplicates)
      (construct-two-sequence-checks replace mismatch search))))

(let* ((list (list 1 2 3 4 5 6 7 120 'hi-there '#:everyone))
       (vector (map 'vector #'identity list))
       (bit-vector (map 'bit-vector
			#'(lambda (object) (if (fixnump object) 1 0)) list))
       (string (map 'string 
		    #'(lambda (object) (or (and (fixnump object)
						(int-char object))
					   (decode-char 'ucs #x20ac))) list))
       (gensym (gensym)))
  (Assert (null (find 'not-in-it list)))
  (Assert (null (find 'not-in-it vector)))
  (Assert (null (find 'not-in-it bit-vector)))
  (Assert (null (find 'not-in-it string)))
  (loop
    for elt being each element in vector using (index position)
    do
    (Assert (eq elt (find elt list)))
    (Assert (eq (elt list position) (find elt vector))))
  (Assert (eq gensym (find 'not-in-it list :default gensym)))
  (Assert (eq gensym (find 'not-in-it vector :default gensym)))
  (Assert (eq gensym (find 'not-in-it bit-vector :default gensym)))
  (Assert (eq gensym (find 'not-in-it string :default gensym)))
  (Assert (eq 'hi-there (find 'hi-there list)))
  ;; Different uninterned symbols with the same name.
  (Assert (not (eq '#1=#:everyone (find '#1# list))))

  ;; Test concatenate.
  (Assert (equal list (concatenate 'list vector)))
  (Assert (equal list (concatenate 'list (subseq vector 0 4)
				   (subseq list 4))))
  (Assert (equal vector (concatenate 'vector list)))
  (Assert (equal vector (concatenate `(vector * ,(length vector)) list)))
  (Assert (equal string (concatenate `(vector character ,(length string))
				     (append string nil))))
  (Assert (equal bit-vector (concatenate 'bit-vector (subseq bit-vector 0 4)
					 (append (subseq bit-vector 4) nil))))
  (Assert (equal bit-vector (concatenate `(vector bit ,(length bit-vector))
					 (subseq bit-vector 0 4)
					 (append (subseq bit-vector 4) nil)))))

;;-----------------------------------------------------
;; Test `block', `return-from'
;;-----------------------------------------------------
(Assert (eql 1 (block outer
		 (flet ((outtahere (n) (return-from outer n)))
		   (block outer (outtahere 1)))
		 2))
	"checking `block' and `return-from' are lexically scoped correctly")

;; Other tests are available in Paul Dietz' test suite, and pass. The above,
;; which we used to fail, is based on a test in the Hyperspec. We still
;; behave incorrectly when compiled for the contorted-example function of
;; CLTL2, whence the following test:

(flet ((needs-lexical-context (first second third)
	 (if (eql 0 first)
	     (funcall second)
	   (block awkward
	     (+ 5 (needs-lexical-context
		   (1- first)
		   third
		   #'(lambda () (return-from awkward 0)))
		first)))))
  (if (compiled-function-p (symbol-function 'needs-lexical-context))
      (Known-Bug-Expect-Failure
       (Assert (eql 0 (needs-lexical-context 2 nil nil))
	"the function special operator doesn't create a lexical context."))
    (Assert (eql 0 (needs-lexical-context 2 nil nil)))))

;;; end of lisp-tests.el
