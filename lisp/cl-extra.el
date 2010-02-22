;;; cl-extra.el --- Common Lisp extensions for XEmacs Lisp (part two)

;; Copyright (C) 1993,2000,2003  Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Maintainer: XEmacs Development Team
;; Version: 2.02
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; This file is dumped with XEmacs.

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains portions of the Common Lisp extensions
;; package which are autoloaded since they are relatively obscure.

;; See cl.el for Change Log.


;;; Code:
;; XEmacs addition
(eval-when-compile
  (require 'obsolete))

(or (memq 'cl-19 features)
    (error "Tried to load `cl-extra' before `cl'!"))


;;; Type coercion.

(defun coerce (x type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier."
  (cond ((eq type 'list) (if (listp x) x (append x nil)))
	((eq type 'vector) (if (vectorp x) x (vconcat x)))
	((eq type 'string) (if (stringp x) x (concat x)))
	((eq type 'array) (if (arrayp x) x (vconcat x)))
	((and (eq type 'character) (stringp x) (= (length x) 1)) (aref x 0))
	((and (eq type 'character) (symbolp x)) (coerce (symbol-name x) type))
	;; XEmacs addition character <-> integer coercions
	((and (eq type 'character) (char-int-p x)) (int-char x))
	((and (eq type 'integer) (characterp x)) (char-int x))
	((eq type 'float) (float x))
	;; XEmacs addition: enhanced numeric type coercions
	((and-fboundp 'coerce-number
	   (memq type '(integer ratio bigfloat))
	   (coerce-number x type)))
	;; XEmacs addition: bit-vector coercion
	((or (eq type 'bit-vector)
	     (eq type 'simple-bit-vector))
	 (if (bit-vector-p x) x (apply 'bit-vector (append x nil))))
	;; XEmacs addition: weak-list coercion
	((eq type 'weak-list)
	 (if (weak-list-p x) x
	   (let ((wl (make-weak-list)))
	     (set-weak-list-list wl (if (listp x) x (append x nil)))
	     wl)))
	((and
	  (consp type)
	  (or (eq (car type) 'vector)
	      (eq (car type) 'simple-array)
	      (eq (car type) 'simple-vector))
	  (cond
	   ((equal (cdr-safe type) '(*))
	    (coerce x 'vector))
	   ((equal (cdr-safe type) '(bit))
	    (coerce x 'bit-vector))
	   ((equal (cdr-safe type) '(character))
	    (coerce x 'string)))))
	((typep x type) x)
	(t (error "Can't coerce %s to type %s" x type))))


;;;;; Predicates.
;;
;;;; I'd actually prefer not to have this inline, the space
;;;; vs. amount-it's-called trade-off isn't reasonable, but that would
;;;; introduce bytecode problems with the compiler macro in cl-macs.el.
;;(defsubst cl-string-vector-equalp (cl-string cl-vector)
;;  "Helper function for `equalp', which see."
;;;  (check-argument-type #'stringp cl-string)
;;;  (check-argument-type #'vector cl-vector)
;;  (let ((cl-i (length cl-string))
;;	cl-char cl-other)
;;    (when (= cl-i (length cl-vector))
;;      (while (and (>= (setq cl-i (1- cl-i)) 0)
;;		  (or (eq (setq cl-char (aref cl-string cl-i))
;;			  (setq cl-other (aref cl-vector cl-i)))
;;		      (and (characterp cl-other) ; Note we want to call this
;;						 ; as rarely as possible, it
;;						 ; doesn't have a bytecode.
;;			   (eq (downcase cl-char) (downcase cl-other))))))
;;      (< cl-i 0))))
;;
;;;; See comment on cl-string-vector-equalp above.
;;(defsubst cl-bit-vector-vector-equalp (cl-bit-vector cl-vector)
;;  "Helper function for `equalp', which see."
;;;  (check-argument-type #'bit-vector-p cl-bit-vector)
;;;  (check-argument-type #'vectorp cl-vector)
;;  (let ((cl-i (length cl-bit-vector))
;;	cl-other)
;;    (when (= cl-i (length cl-vector))
;;      (while (and (>= (setq cl-i (1- cl-i)) 0)
;;		  (numberp (setq cl-other (aref cl-vector cl-i)))
;;		  ;; Differs from clisp here.
;;		  (= (aref cl-bit-vector cl-i) cl-other)))
;;      (< cl-i 0))))
;;
;;;; These two helper functions call equalp recursively, the two above have no
;;;; need to.
;;(defsubst cl-vector-array-equalp (cl-vector cl-array)
;;  "Helper function for `equalp', which see."
;;;  (check-argument-type #'vector cl-vector)
;;;  (check-argument-type #'arrayp cl-array)
;;  (let ((cl-i (length cl-vector)))
;;    (when (= cl-i (length cl-array))
;;      (while (and (>= (setq cl-i (1- cl-i)) 0)
;;		  (equalp (aref cl-vector cl-i) (aref cl-array cl-i))))
;;      (< cl-i 0))))
;;
;;(defsubst cl-hash-table-contents-equalp (cl-hash-table-1 cl-hash-table-2)
;;  "Helper function for `equalp', which see."
;;  (symbol-macrolet
;;      ;; If someone has gone and fished the uninterned symbol out of this
;;      ;; function's constants vector, and subsequently stored it as a value
;;      ;; in a hash table, it's their own damn fault when
;;      ;; `cl-hash-table-contents-equalp' gives the wrong answer.
;;      ((equalp-default '#:equalp-default))
;;    (loop
;;      for x-key being the hash-key in cl-hash-table-1
;;      using (hash-value x-value)
;;      with y-value = nil
;;      always (and (not (eq equalp-default
;;			   (setq y-value (gethash x-key cl-hash-table-2
;;						  equalp-default))))
;;		  (equalp y-value x-value)))))
;;
;;(defun equalp (x y)
;;  "Return t if two Lisp objects have similar structures and contents.
;;
;;This is like `equal', except that it accepts numerically equal
;;numbers of different types (float, integer, bignum, bigfloat), and also
;;compares strings and characters case-insensitively.
;;
;;Arrays (that is, strings, bit-vectors, and vectors) of the same length and
;;with contents that are `equalp' are themselves `equalp'.
;;
;;Two hash tables are `equalp' if they have the same test (see
;;`hash-table-test'), if they have the same number of entries, and if, for
;;each entry in one hash table, its key is equivalent to a key in the other
;;hash table using the hash table test, and its value is `equalp' to the other
;;hash table's value for that key."
;;  (cond ((eq x y))
;;	((stringp x)
;;	 (if (stringp y)
;;	     (eq t (compare-strings x nil nil y nil nil t))
;;	   (if (vectorp y)
;;	       (cl-string-vector-equalp x y)
;;	     ;; bit-vectors and strings are only equalp if they're
;;	     ;; zero-length:
;;	     (and (equal "" x) (equal #* y)))))
;;	((numberp x)
;;	 (and (numberp y) (= x y)))
;;	((consp x)
;;	 (while (and (consp x) (consp y) (equalp (car x) (car y)))
;;	   (setq x (cdr x) y (cdr y)))
;;	 (and (not (consp x)) (equalp x y)))
;;	(t
;;	 ;; From here on, the type tests don't (yet) have bytecodes.
;;	 (let ((x-type (type-of x)))
;;	   (cond ((eq 'vector x-type)
;;		  (if (stringp y)
;;		      (cl-string-vector-equalp y x)
;;		    (if (vectorp y)
;;			(cl-vector-array-equalp x y)
;;		      (if (bit-vector-p y)
;;			  (cl-bit-vector-vector-equalp y x)))))
;;		 ((eq 'character x-type)
;;		  (and (characterp y)
;;		       ;; If the characters are actually identical, the
;;		       ;; first eq test will have caught them above; we only
;;		       ;; need to check them case-insensitively here.
;;		       (eq (downcase x) (downcase y))))
;;		 ((eq 'hash-table x-type)
;;		  (and (hash-table-p y)
;;		       (eq (hash-table-test x) (hash-table-test y))
;;		       (= (hash-table-count x) (hash-table-count y))
;;		       (cl-hash-table-contents-equalp x y)))
;;		 ((eq 'bit-vector x-type)
;;		  (if (bit-vector-p y)
;;		      (equal x y)
;;		    (if (vectorp y)
;;			(cl-bit-vector-vector-equalp x y)
;;		      ;; bit-vectors and strings are only equalp if they're
;;		      ;; zero-length:
;;		      (and (equal "" y) (equal #* x)))))
;;		 (t (equal x y)))))))

;; XEmacs; #'map, #'mapc, #'mapl, #'maplist, #'mapcon, #'some and #'every
;; are now in C, together with #'map-into, which was never in this file.

(defun notany (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of every element of SEQ or SEQs."
  (not (apply 'some cl-pred cl-seq cl-rest)))

(defun notevery (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of some element of SEQ or SEQs."
  (not (apply 'every cl-pred cl-seq cl-rest)))

;;; Support for `loop'.
(defalias 'cl-map-keymap 'map-keymap)

(defun cl-map-keymap-recursively (cl-func-rec cl-map &optional cl-base)
  (or cl-base
      (setq cl-base (copy-sequence [0])))
  (map-keymap
   (function
    (lambda (cl-key cl-bind)
      (aset cl-base (1- (length cl-base)) cl-key)
      (if (keymapp cl-bind)
	  (cl-map-keymap-recursively
	   cl-func-rec cl-bind
	   (vconcat cl-base (list 0)))
	(funcall cl-func-rec cl-base cl-bind))))
   cl-map))

(defun cl-map-intervals (cl-func &optional cl-what cl-prop cl-start cl-end)
  (or cl-what (setq cl-what (current-buffer)))
  (if (bufferp cl-what)
      (let (cl-mark cl-mark2 (cl-next t) cl-next2)
	(with-current-buffer cl-what
	  (setq cl-mark (copy-marker (or cl-start (point-min))))
	  (setq cl-mark2 (and cl-end (copy-marker cl-end))))
	(while (and cl-next (or (not cl-mark2) (< cl-mark cl-mark2)))
	  (setq cl-next (if cl-prop (next-single-property-change
				     cl-mark cl-prop cl-what)
			  (next-property-change cl-mark cl-what))
		cl-next2 (or cl-next (with-current-buffer cl-what
				       (point-max))))
	  (funcall cl-func (prog1 (marker-position cl-mark)
			     (set-marker cl-mark cl-next2))
		   (if cl-mark2 (min cl-next2 cl-mark2) cl-next2)))
	(set-marker cl-mark nil) (if cl-mark2 (set-marker cl-mark2 nil)))
    (or cl-start (setq cl-start 0))
    (or cl-end (setq cl-end (length cl-what)))
    (while (< cl-start cl-end)
      (let ((cl-next (or (if cl-prop (next-single-property-change
				      cl-start cl-prop cl-what)
			   (next-property-change cl-start cl-what))
			 cl-end)))
	(funcall cl-func cl-start (min cl-next cl-end))
	(setq cl-start cl-next)))))

(defun cl-map-overlays (cl-func &optional cl-buffer cl-start cl-end cl-arg)
  (or cl-buffer (setq cl-buffer (current-buffer)))
  (with-fboundp '(overlay-start overlay-end overlays-at next-overlay-change)
    (if-fboundp 'overlay-lists

	;; This is the preferred algorithm, though overlay-lists is
	;; undocumented.
	(let (cl-ovl)
	  (with-current-buffer cl-buffer
	    (setq cl-ovl (overlay-lists))
	    (if cl-start (setq cl-start (copy-marker cl-start)))
	    (if cl-end (setq cl-end (copy-marker cl-end))))
	  (setq cl-ovl (nconc (car cl-ovl) (cdr cl-ovl)))
	  (while (and cl-ovl
		      (or (not (overlay-start (car cl-ovl)))
			  (and cl-end (>= (overlay-start (car cl-ovl)) cl-end))
			  (and cl-start (<= (overlay-end (car cl-ovl))
					    cl-start))
			  (not (funcall cl-func (car cl-ovl) cl-arg))))
	    (setq cl-ovl (cdr cl-ovl)))
	  (if cl-start (set-marker cl-start nil))
	  (if cl-end (set-marker cl-end nil)))

      ;; This alternate algorithm fails to find zero-length overlays.
      (let ((cl-mark (with-current-buffer cl-buffer
		       (copy-marker (or cl-start (point-min)))))
	    (cl-mark2 (and cl-end (with-current-buffer cl-buffer
				    (copy-marker cl-end))))
	    cl-pos cl-ovl)
	(while (save-excursion
		 (and (setq cl-pos (marker-position cl-mark))
		      (< cl-pos (or cl-mark2 (point-max)))
		      (progn
			(set-buffer cl-buffer)
			(setq cl-ovl (overlays-at cl-pos))
			(set-marker cl-mark (next-overlay-change cl-pos)))))
	  (while (and cl-ovl
		      (or (/= (overlay-start (car cl-ovl)) cl-pos)
			  (not (and (funcall cl-func (car cl-ovl) cl-arg)
				    (set-marker cl-mark nil)))))
	    (setq cl-ovl (cdr cl-ovl))))
	(set-marker cl-mark nil) (if cl-mark2 (set-marker cl-mark2 nil))))))

;;; Support for `setf'.
(defun cl-set-frame-visible-p (frame val)
  (cond ((null val) (make-frame-invisible frame))
	((eq val 'icon) (iconify-frame frame))
	(t (make-frame-visible frame)))
  val)

;;; Support for `progv'.
(defvar cl-progv-save)
(defun cl-progv-before (syms values)
  (while syms
    (push (if (boundp (car syms))
		 (cons (car syms) (symbol-value (car syms)))
	       (car syms)) cl-progv-save)
    (if values
	(set (pop syms) (pop values))
      (makunbound (pop syms)))))

(defun cl-progv-after ()
  (while cl-progv-save
    (if (consp (car cl-progv-save))
	(set (car (car cl-progv-save)) (cdr (car cl-progv-save)))
      (makunbound (car cl-progv-save)))
    (pop cl-progv-save)))


;;; Numbers.

(defun gcd (&rest args)
  "Return the greatest common divisor of the arguments."
  (let ((a (abs (or (pop args) 0))))
    (while args
      (let ((b (abs (pop args))))
	(while (> b 0) (setq b (% a (setq a b))))))
    a))

(defun lcm (&rest args)
  "Return the least common multiple of the arguments."
  (if (memq 0 args)
      0
    (let ((a (abs (or (pop args) 1))))
      (while args
	(let ((b (abs (pop args))))
	  (setq a (* (/ a (gcd a b)) b))))
      a)))

(defun isqrt (a)
  "Return the integer square root of the argument."
  (if (and (integerp a) (> a 0))
      ;; XEmacs change
      (let ((g (cond ((>= a 1000000) 10000) ((>= a 10000) 1000)
		     ((>= a 100) 100) (t 10)))
	    g2)
	(while (< (setq g2 (/ (+ g (/ a g)) 2)) g)
	  (setq g g2))
	g)
    (if (eq a 0) 0 (signal 'arith-error nil))))

;; XEmacs addition
(defun cl-expt (x y)
  "Return X raised to the power of Y.  Works only for integer arguments."
  (if (<= y 0) (if (= y 0) 1 (if (memq x '(-1 1)) (cl-expt x (- y)) 0))
    (* (if (= (% y 2) 0) 1 x) (cl-expt (* x x) (/ y 2)))))
(or (and (fboundp 'expt) (subrp (symbol-function 'expt)))
    (defalias 'expt 'cl-expt))

;; We can't use macrolet in this file; whence the literal macro
;; definition-and-call:
((macro . (lambda (&rest symbols)
   "Make some old CL package truncate and round functions available.

These functions are now implemented in C; their Lisp implementations in this
XEmacs are trivial, so we provide them and mark them obsolete."
   (let (symbol result)
     (while symbols
       (setq symbol (car symbols)
	     symbols (cdr symbols))
       (push `(make-obsolete ',(intern (format "%s*" symbol))
	       ',symbol "21.5.29")
	     result)
       (push
	`(defun ,(intern (format "%s*" symbol)) (number &optional divisor)
	  ,(format "See `%s'. This returns a list, not multiple values."
		   symbol)
	  (multiple-value-list (,symbol number divisor)))
	result))
     (cons 'progn result))))
 ceiling floor round truncate)

(defun mod* (x y)
  "The remainder of X divided by Y, with the same sign as Y."
  (nth-value 1 (floor x y)))

(defun rem* (x y)
  "The remainder of X divided by Y, with the same sign as X."
  (nth-value 1 (truncate x y)))

(defun signum (a)
  "Return 1 if A is positive, -1 if negative, 0 if zero."
  (cond ((> a 0) 1) ((< a 0) -1) (t 0)))

;; Random numbers.

(defvar *random-state*)
(defun random* (lim &optional state)
  "Return a random nonnegative number less than LIM, an integer or float.
Optional second arg STATE is a random-state object."
  (or state (setq state *random-state*))
  ;; Inspired by "ran3" from Numerical Recipes.  Additive congruential method.
  (let ((vec (aref state 3)))
    (if (integerp vec)
	(let ((i 0) (j (- 1357335 (% (abs vec) 1357333))) (k 1))
	  (aset state 3 (setq vec (make-vector 55 nil)))
	  (aset vec 0 j)
	  (while (> (setq i (% (+ i 21) 55)) 0)
	    (aset vec i (setq j (prog1 k (setq k (- j k))))))
	  (while (< (setq i (1+ i)) 200) (random* 2 state))))
    (let* ((i (aset state 1 (% (1+ (aref state 1)) 55)))
	   (j (aset state 2 (% (1+ (aref state 2)) 55)))
	   (n (logand 8388607 (aset vec i (- (aref vec i) (aref vec j))))))
      (if (integerp lim)
	  (if (<= lim 512) (% n lim)
	    (if (> lim 8388607) (setq n (+ (lsh n 9) (random* 512 state))))
	    (let ((mask 1023))
	      (while (< mask (1- lim)) (setq mask (1+ (+ mask mask))))
	      (if (< (setq n (logand n mask)) lim) n (random* lim state))))
	(* (/ n '8388608e0) lim)))))

(defun make-random-state (&optional state)
  "Return a copy of random-state STATE, or of `*random-state*' if omitted.
If STATE is t, return a new state object seeded from the time of day."
  (cond ((null state) (make-random-state *random-state*))
	((vectorp state) (cl-copy-tree state t))
	((integerp state) (vector 'cl-random-state-tag -1 30 state))
	(t (make-random-state (cl-random-time)))))

(defun random-state-p (object)
  "Return t if OBJECT is a random-state object."
  (and (vectorp object) (= (length object) 4)
       (eq (aref object 0) 'cl-random-state-tag)))


;; Implementation limits.

(defun cl-finite-do (func a b)
  (condition-case nil
      (let ((res (funcall func a b)))   ; check for IEEE infinity
	(and (numberp res) (/= res (/ res 2)) res))
    (arith-error nil)))

(defvar most-positive-float)
(defvar most-negative-float)
(defvar least-positive-float)
(defvar least-negative-float)
(defvar least-positive-normalized-float)
(defvar least-negative-normalized-float)
(defvar float-epsilon)
(defvar float-negative-epsilon)

(defun cl-float-limits ()
  (or most-positive-float (not (numberp '2e1))
      (let ((x '2e0) y z)
	;; Find maximum exponent (first two loops are optimizations)
	(while (cl-finite-do '* x x) (setq x (* x x)))
	(while (cl-finite-do '* x (/ x 2)) (setq x (* x (/ x 2))))
	(while (cl-finite-do '+ x x) (setq x (+ x x)))
	(setq z x y (/ x 2))
	;; Now fill in 1's in the mantissa.
	(while (and (cl-finite-do '+ x y) (/= (+ x y) x))
	  (setq x (+ x y) y (/ y 2)))
	(setq most-positive-float x
	      most-negative-float (- x))
	;; Divide down until mantissa starts rounding.
	(setq x (/ x z) y (/ 16 z) x (* x y))
	(while (condition-case nil (and (= x (* (/ x 2) 2)) (> (/ y 2) 0))
		 (arith-error nil))
	  (setq x (/ x 2) y (/ y 2)))
	(setq least-positive-normalized-float y
	      least-negative-normalized-float (- y))
	;; Divide down until value underflows to zero.
	(setq x (/ 1 z) y x)
	(while (condition-case nil (> (/ x 2) 0) (arith-error nil))
	  (setq x (/ x 2)))
	(setq least-positive-float x
	      least-negative-float (- x))
	(setq x '1e0)
	(while (/= (+ '1e0 x) '1e0) (setq x (/ x 2)))
	(setq float-epsilon (* x 2))
	(setq x '1e0)
	(while (/= (- '1e0 x) '1e0) (setq x (/ x 2)))
	(setq float-negative-epsilon (* x 2))))
  nil)


;;; Sequence functions.

;XEmacs -- our built-in is more powerful.
;(defun subseq (seq start &optional end)
;  "Return the subsequence of SEQ from START to END.
;If END is omitted, it defaults to the length of the sequence.
;If START or END is negative, it counts from the end."
;  (if (stringp seq) (substring seq start end)
;    (let (len)
;      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
;      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
;      (cond ((listp seq)
;	     (if (> start 0) (setq seq (nthcdr start seq)))
;	     (if end
;		 (let ((res nil))
;		   (while (>= (setq end (1- end)) start)
;		     (push (pop seq) res))
;		   (nreverse res))
;	       (copy-sequence seq)))
;	    (t
;	     (or end (setq end (or len (length seq))))
;	     (let ((res (make-vector (max (- end start) 0) nil))
;		   (i 0))
;	       (while (< start end)
;		 (aset res i (aref seq start))
;		 (setq i (1+ i) start (1+ start)))
;	       res))))))

(defun concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCES."
  ;; XEmacs change: use case instead of cond for clarity
  (case type
    (vector (apply 'vconcat seqs))
    (string (apply 'concat seqs))
    (list   (apply 'append (append seqs '(nil))))
    (t (error "Not a sequence type name: %s" type))))

;;; List functions.

(defun revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (nconc (reverse x) y))

(defun nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (nconc (nreverse x) y))

(defun list-length (x)
  "Return the length of a list.  Return nil if list is circular."
  (let ((n 0) (fast x) (slow x))
    (while (and (cdr fast) (not (and (eq fast slow) (> n 0))))
      (setq n (+ n 2) fast (cdr (cdr fast)) slow (cdr slow)))
    (if fast (if (cdr fast) nil (1+ n)) n)))

(defun tailp (sublist list)
  "Return true if SUBLIST is a tail of LIST."
  (while (and (consp list) (not (eq sublist list)))
    (setq list (cdr list)))
  (if (numberp sublist) (equal sublist list) (eq sublist list)))

(defalias 'cl-copy-tree 'copy-tree)


;;; Property lists.

;; XEmacs: our `get' groks DEFAULT.
(defalias 'get* 'get)
(defalias 'getf 'plist-get)

(defun cl-set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

(defun cl-do-remf (plist tag)
  (let ((p (cdr plist)))
    (while (and (cdr p) (not (eq (car (cdr p)) tag))) (setq p (cdr (cdr p))))
    (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t))))

;; XEmacs change: we have a builtin remprop
(defalias 'cl-remprop 'remprop)

(defun get-properties (plist indicator-list)
  "Find a property from INDICATOR-LIST in PLIST.
Return 3 values:
- the first property found,
- its value,
- the tail of PLIST beginning with the found entry."
  (do ((plst plist (cddr plst)))
      ((null plst) (values nil nil nil))
    (cond ((atom (cdr plst))
	   (error "Malformed property list: %S." plist))
	  ((memq (car plst) indicator-list)
	   (return (values (car plst) (cadr plst) plst))))))

;; See our compiler macro in cl-macs.el, we will only pass back the
;; actual lambda list in interpreted code or if we've been funcalled
;; (from #'apply or #'mapcar or whatever).
(defun constantly (value &rest more-values)
  "Construct a function always returning VALUE, and possibly MORE-VALUES.

The constructed function accepts any number of arguments, and ignores them.

Members of MORE-VALUES, if provided, will be passed as multiple values; see
`multiple-value-bind' and `multiple-value-setq'."
  `(lambda (&rest ignore) (values-list ',(cons value more-values))))

;;; Hash tables.

;; The `regular' Common Lisp hash-table stuff has been moved into C.
;; Only backward compatibility stuff remains here.
(defun make-hashtable (size &optional test)
  (make-hash-table :test test :size size))
(defun make-weak-hashtable (size &optional test)
  (make-hash-table :test test :size size :weakness t))
(defun make-key-weak-hashtable (size &optional test)
  (make-hash-table :test test :size size :weakness 'key))
(defun make-value-weak-hashtable (size &optional test)
  (make-hash-table :test test :size size :weakness 'value))

(define-obsolete-function-alias 'hashtablep 'hash-table-p)
(define-obsolete-function-alias 'hashtable-fullness 'hash-table-count)
(define-obsolete-function-alias 'hashtable-test-function 'hash-table-test)
(define-obsolete-function-alias 'hashtable-type 'hash-table-type)
(define-obsolete-function-alias 'hashtable-size 'hash-table-size)
(define-obsolete-function-alias 'copy-hashtable 'copy-hash-table)

(make-obsolete 'make-hashtable            'make-hash-table)
(make-obsolete 'make-weak-hashtable       'make-hash-table)
(make-obsolete 'make-key-weak-hashtable   'make-hash-table)
(make-obsolete 'make-value-weak-hashtable 'make-hash-table)
(make-obsolete 'hash-table-type           'hash-table-weakness)

(when (fboundp 'x-keysym-hash-table)
  (make-obsolete 'x-keysym-hashtable 'x-keysym-hash-table))

;; Compatibility stuff for old kludgy cl.el hash table implementation
(defvar cl-builtin-gethash (symbol-function 'gethash))
(defvar cl-builtin-remhash (symbol-function 'remhash))
(defvar cl-builtin-clrhash (symbol-function 'clrhash))
(defvar cl-builtin-maphash (symbol-function 'maphash))

(defalias 'cl-gethash 'gethash)
(defalias 'cl-puthash 'puthash)
(defalias 'cl-remhash 'remhash)
(defalias 'cl-clrhash 'clrhash)
(defalias 'cl-maphash 'maphash)
;; These three actually didn't exist in Emacs-20.
(defalias 'cl-make-hash-table 'make-hash-table)
(defalias 'cl-hash-table-p 'hash-table-p)
(defalias 'cl-hash-table-count 'hash-table-count)

;;; Some debugging aids.

(defun cl-prettyprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  (let ((pt (point)) last)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (while (search-forward "(quote " last t)
      (delete-backward-char 7)
      (insert "'")
      (forward-sexp)
      (delete-char 1))
    (goto-char (1+ pt))
    (cl-do-prettyprint)))

(defun cl-do-prettyprint ()
  (skip-chars-forward " ")
  (if (looking-at "(")
      (let ((skip (or (looking-at "((")
		      ;; XEmacs: be selective about trailing stuff after prog
		      (looking-at "(prog[nv12\\(ress-feedback\\|n-with-message\\)]")
		      (looking-at "(unwind-protect ")
		      (looking-at "(function (")
		      (looking-at "(cl-block-wrapper ")))
	    (two (or (looking-at "(defun ") (looking-at "(defmacro ")))
	    (let (or (looking-at "(let\\*? ") (looking-at "(while ")))
	    (set (looking-at "(p?set[qf] ")))
	(if (or skip let
		(progn
		  (forward-sexp)
		  (and (>= (current-column) 78) (progn (backward-sexp) t))))
	    (let ((nl t))
	      (forward-char 1)
	      (cl-do-prettyprint)
	      (or skip (looking-at ")") (cl-do-prettyprint))
	      (or (not two) (looking-at ")") (cl-do-prettyprint))
	      (while (not (looking-at ")"))
		(if set (setq nl (not nl)))
		(if nl (insert "\n"))
		(lisp-indent-line)
		(cl-do-prettyprint))
	      (forward-char 1))))
    (forward-sexp)))

(defvar cl-macroexpand-cmacs nil)
(defvar cl-closure-vars nil)

(defun cl-macroexpand-all (form &optional env)
  "Expand all macro calls through a Lisp FORM.
This also does some trivial optimizations to make the form prettier."
  (while (or (not (eq form (setq form (macroexpand form env))))
	     (and cl-macroexpand-cmacs
		  (not (eq form (setq form (compiler-macroexpand form)))))))
  (cond ((not (consp form)) form)
	((memq (car form) '(let let*))
	 (if (null (nth 1 form))
	     (cl-macroexpand-all (cons 'progn (cddr form)) env)
	   (let ((letf nil) (res nil) (lets (cadr form)))
	     (while lets
	       (push (if (consp (car lets))
			    (let ((exp (cl-macroexpand-all (caar lets) env)))
			      (or (symbolp exp) (setq letf t))
			      (cons exp (cl-macroexpand-body (cdar lets) env)))
			  (let ((exp (cl-macroexpand-all (car lets) env)))
			    (if (symbolp exp) exp
			      (setq letf t) (list exp nil)))) res)
	       (setq lets (cdr lets)))
	     (list* (if letf (if (eq (car form) 'let) 'letf 'letf*) (car form))
		    (nreverse res) (cl-macroexpand-body (cddr form) env)))))
	((eq (car form) 'cond)
	 (cons (car form)
	       (mapcar (function (lambda (x) (cl-macroexpand-body x env)))
		       (cdr form))))
	((eq (car form) 'condition-case)
	 (list* (car form) (nth 1 form) (cl-macroexpand-all (nth 2 form) env)
		(mapcar (function
			 (lambda (x)
			   (cons (car x) (cl-macroexpand-body (cdr x) env))))
			(cdddr form))))
	((memq (car form) '(quote function))
	 (if (eq (car-safe (nth 1 form)) 'lambda)
	     (let ((body (cl-macroexpand-body (cddadr form) env)))
	       (if (and cl-closure-vars (eq (car form) 'function)
			(cl-expr-contains-any body cl-closure-vars))
		   (let* ((new (mapcar 'gensym cl-closure-vars))
			  (sub (pairlis cl-closure-vars new)) (decls nil))
		     (while (or (stringp (car body))
				(eq (car-safe (car body)) 'interactive))
		       (push (list 'quote (pop body)) decls))
		     (put (car (last cl-closure-vars)) 'used t)
		     (append
		      (list 'list '(quote lambda) '(quote (&rest --cl-rest--)))
		      (sublis sub (nreverse decls))
		      (list
		       (list* 'list '(quote apply)
			      ;; XEmacs: put a quote before the function
			      (list 'list '(quote quote)
				    (list 'function
					  (list* 'lambda
						 (append new (cadadr form))
						 (sublis sub body))))
			      (nconc (mapcar (function
					      (lambda (x)
						(list 'list '(quote quote) x)))
					     cl-closure-vars)
				     '((quote --cl-rest--)))))))
		 (list (car form) (list* 'lambda (cadadr form) body))))
	   (let ((found (assq (cadr form) env)))
	     ;; XEmacs: cadr/caddr operate on nil without errors
	     (if (eq (cadr (caddr found)) 'cl-labels-args)
		 (cl-macroexpand-all (cadr (caddr (cadddr found))) env)
	       form))))
	((memq (car form) '(defun defmacro))
	 (list* (car form) (nth 1 form) (cl-macroexpand-body (cddr form) env)))
	((and (eq (car form) 'progn) (not (cddr form)))
	 (cl-macroexpand-all (nth 1 form) env))
	((eq (car form) 'setq)
	 (let* ((args (cl-macroexpand-body (cdr form) env)) (p args))
	   (while (and p (symbolp (car p))) (setq p (cddr p)))
	   (if p (cl-macroexpand-all (cons 'setf args)) (cons 'setq args))))
	(t (cons (car form) (cl-macroexpand-body (cdr form) env)))))

(defun cl-macroexpand-body (body &optional env)
  (mapcar (function (lambda (x) (cl-macroexpand-all x env))) body))

(defun cl-prettyexpand (form &optional full)
  (message "Expanding...")
  (let ((cl-macroexpand-cmacs full) (cl-compiling-file full)
	(byte-compile-macro-environment nil))
    (setq form (cl-macroexpand-all form
				   (and (not full) '((block) (eval-when)))))
    (message "Formatting...")
    (prog1 (cl-prettyprint form)
      (message ""))))



(run-hooks 'cl-extra-load-hook)

;; XEmacs addition
(provide 'cl-extra)

;;; arch-tag: bcd03437-0871-43fb-a8f1-ad0e0b5427ed
;;; cl-extra.el ends here
