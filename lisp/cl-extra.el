;;; cl-extra.el --- Common Lisp extensions for XEmacs Lisp (part two)

;; Copyright (C) 1993,2000,2003  Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Maintainer: XEmacs Development Team
;; Version: 2.02
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

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

;;; Type coercion.

(defun coerce (object type)
  "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier."
  (cond ((eq type 'list) (if (listp object) object (append object nil)))
	((eq type 'vector) (if (vectorp object) object (vconcat object)))
	((eq type 'string) (if (stringp object) object (concat object)))
	((eq type 'array) (if (arrayp object) object (vconcat object)))
	((and (eq type 'character) (stringp object)
	      (eql (length object) 1)) (aref object 0))
	((and (eq type 'character) (symbolp object))
	 (coerce (symbol-name object) type))
	;; XEmacs addition character <-> integer coercions
	((and (eq type 'character) (char-int-p object)) (int-char object))
	((and (memq type '(integer fixnum)) (characterp object))
	 (char-int object))
	((eq type 'float) (float object))
	;; XEmacs addition: enhanced numeric type coercions
	((and-fboundp 'coerce-number
	   (memq type '(integer ratio bigfloat fixnum))
	   (coerce-number object type)))
	;; XEmacs addition: bit-vector coercion
	((or (eq type 'bit-vector)
	     (eq type 'simple-bit-vector))
	 (if (bit-vector-p object)
	     object
	   (apply 'bit-vector (append object nil))))
	;; XEmacs addition: weak-list coercion
	((eq type 'weak-list)
	 (if (weak-list-p object) object
	   (let ((wl (make-weak-list)))
	     (set-weak-list-list wl (if (listp object)
					object
				      (append object nil)))
	     wl)))
	((and
	  (memq (car-safe type) '(vector simple-array))
	  (loop
	    for (ignore elements length) = type
	    initially (declare (special ignore))
	    return (if (or (memq length '(* nil)) (eql length (length object)))
		       (cond
			((memq elements '(t * nil))
			 (coerce object 'vector))
			((memq elements '(string-char character))
			 (coerce object 'string))
			((eq elements 'bit)
			 (coerce object 'bit-vector)))
		     (error 
		      'wrong-type-argument
		      "Type specifier length must equal sequence length"
		      type)))))
	((eq (car-safe type) 'simple-vector)
	 (coerce object (list* 'vector t (cdr type))))
	((memq (car-safe type)
	       '(string simple-string base-string simple-base-string))
	 (coerce object (list* 'vector 'character (cdr type))))
	((eq (car-safe type) 'bit-vector)
	 (coerce object (list* 'vector 'bit (cdr type))))
	((typep object type) object)
	(t (error 'invalid-operation
		  "Can't coerce object to type" object type))))

;; XEmacs; #'equalp is in C.

;; XEmacs; #'map, #'mapc, #'mapl, #'maplist, #'mapcon, #'some and #'every
;; are now in C, together with #'map-into, which was never in this file.

;; The compiler macro for this in cl-macs.el means if #'complement is handed
;; a constant expression, byte-compiled code will see a byte-compiled
;; function.
(defun complement (function &optional documentation)
  "Return a function which gives the logical inverse of what FUNCTION would."
  `(lambda (&rest arguments) ,@(if documentation (list documentation))
     (not (apply ',function arguments))))

(defun notany (cl-predicate cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of every element of SEQUENCE.

With optional SEQUENCES, call PREDICATE each time with as many arguments as
there are SEQUENCES (plus one for the element from SEQUENCE).

arguments: (PREDICATE SEQUENCES &rest SEQUENCES)"
  (not (apply 'some cl-predicate cl-seq cl-rest)))

(defun notevery (cl-predicate cl-seq &rest cl-rest)
  "Return true if PREDICATE is false of some element of SEQUENCE.

With optional SEQUENCES, call PREDICATE each time with as many arguments as
there are SEQUENCES (plus one for the element from SEQUENCE).

arguments: (PREDICATE SEQUENCES &rest SEQUENCES)"
  (not (apply 'every cl-predicate cl-seq cl-rest)))

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

;; XEmacs; call cl-float-limits at dump time.
(cl-float-limits)

;;; Sequence functions.

;; XEmacs; #'subseq is in C.

(defun concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCES."
  ;; XEmacs change: use case instead of cond for clarity
  (case type
    (vector (apply 'vconcat seqs))
    (string (apply 'concat seqs))
    (list   (reduce 'append seqs :from-end t :initial-value nil))
    (bit-vector (apply 'bvconcat seqs))
    (t (coerce (reduce 'append seqs :from-end t :initial-value nil) type))))

;;; List functions.

(defun revappend (x y)
  "Equivalent to (append (reverse X) Y)."
  (nconc (reverse x) y))

(defun nreconc (x y)
  "Equivalent to (nconc (nreverse X) Y)."
  (nconc (nreverse x) y))

;; XEmacs; check LIST for type and circularity.
(defun tailp (sublist list)
  "Return true if SUBLIST is a tail of LIST."
  (check-argument-type #'listp list)
  (let ((before list) (evenp t))
    (while (and (consp list) (not (eq sublist list)))
      (setq list (cdr list)
	    evenp (not evenp))
      (if evenp (setq before (cdr before)))
      (if (eq before list) (error 'circular-list list)))
    (eql sublist list)))

(defalias 'cl-copy-tree 'copy-tree)

;;; Property lists.

;; XEmacs: our `get' groks DEFAULT.
(defalias 'get* 'get)
(defalias 'getf 'plist-get)

;; XEmacs; these are built-in.
(defalias 'cl-set-getf 'plist-put)
(defalias 'cl-do-remf 'plist-remprop)
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

;; See also the compiler macro in cl-macs.el.
(defun constantly (value &rest more-values)
  "Construct a function always returning VALUE, and possibly MORE-VALUES.

The constructed function accepts any number of arguments, and ignores them.

Members of MORE-VALUES, if provided, will be passed as multiple values; see
`multiple-value-bind' and `multiple-value-setq'."
  (symbol-macrolet
      ((arglist '(&rest ignore)))
    (if (or more-values (eval-when-compile (not (cl-compiling-file))))
        `(lambda ,arglist (values-list ',(cons value more-values)))
      (make-byte-code
       arglist
       (eval-when-compile
         (let ((compiled (byte-compile-sexp #'(lambda (&rest ignore)
                                                (declare (ignore ignore))
                                                'placeholder))))
           (assert (and
                    (equal [placeholder]
                           (compiled-function-constants compiled))
                    (= 1 (compiled-function-stack-depth compiled)))
		   t
		   "Our assumptions about compiled code appear not to hold.")
           (compiled-function-instructions compiled)))
       (vector value) 1))))

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
  (let ((pt (point)) last just)
    (insert "\n" (prin1-to-string form) "\n")
    (setq last (point))
    (goto-char (1+ pt))
    (while (re-search-forward "(\\(?:\\(?:function\\|quote\\) \\)" last t)
      (delete-region (match-beginning 0) (match-end 0))
      (if (= (length "(function ") (- (match-end 0) (match-beginning 0)))
	  (insert "#'")
	(insert "'"))
      (setq just (point))
      (forward-sexp)
      (delete-char 1)
      (goto-char just))
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

;; XEmacs addition; force cl-macs to be available from here on when
;; compiling files to be dumped.  This is more reasonable than forcing other
;; files to do the same, multiple times.
(eval-when-compile (or (cl-compiling-file) (load "cl-macs")))

(run-hooks 'cl-extra-load-hook)

;; XEmacs addition
(provide 'cl-extra)

;;; arch-tag: bcd03437-0871-43fb-a8f1-ad0e0b5427ed
;;; cl-extra.el ends here
