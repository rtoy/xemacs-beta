;;; cl-macs.el --- Common Lisp extensions for XEmacs Lisp (part four)

;; Copyright (C) 1993, 2003, 2004 Free Software Foundation, Inc.
;; Copyright (C) 2002, 2010 Ben Wing.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
;; Keywords: extensions

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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the portions of the Common Lisp extensions
;; package which should be autoloaded, but need only be present
;; if the compiler or interpreter is used---this file is not
;; necessary for executing compiled code.

;; See cl.el for Change Log.


;;; Code:

(defmacro cl-pop2 (place)
  (list 'prog1 (list 'car-safe (list 'cdr-safe place))
	(list 'setq place (list 'cdr (list 'cdr place)))))
(put 'cl-pop2 'edebug-form-spec 'edebug-sexps)

(defvar cl-optimize-safety)
(defvar cl-optimize-speed)

;;; Initialization.

(defvar cl-old-bc-file-form nil)

;;;###autoload
(defun cl-compile-time-init ()
  (run-hooks 'cl-hack-bytecomp-hook))


;;; Some predicates for analyzing Lisp forms.  These are used by various
;;; macro expanders to optimize the results in certain common cases.

(defconst cl-simple-funcs '(car cdr nth aref elt if and or + - 1+ 1- min max
			    car-safe cdr-safe progn prog1 prog2))
(defconst cl-safe-funcs '(* / % length memq list vector vectorp
			  < > <= >= = error))

;;; Check if no side effects, and executes quickly.
(defun cl-simple-expr-p (x &optional size)
  (or size (setq size 10))
  (if (and (consp x) (not (memq (car x) '(quote function function*))))
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (setq size (1- size))
	     (while (and (setq x (cdr x))
			 (setq size (cl-simple-expr-p (car x) size))))
	     (and (null x) (>= size 0) size)))
    (and (> size 0) (1- size))))

(defun cl-simple-exprs-p (xs)
  (while (and xs (cl-simple-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

;;; Check if no side effects.
(defun cl-safe-expr-p (x)
  (or (not (and (consp x) (not (memq (car x)
                                     '(quote function function* lambda)))))
      (and (symbolp (car x))
	   (or (memq (car x) cl-simple-funcs)
	       (memq (car x) cl-safe-funcs)
	       (get (car x) 'side-effect-free))
	   (progn
	     (while (and (setq x (cdr x)) (cl-safe-expr-p (car x))))
	     (null x)))))

;;; Check if constant (i.e., no side effects or dependencies).
(defun cl-const-expr-p (x)
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function function*))
		  (or (symbolp (nth 1 x))
		      (and (eq (car-safe (nth 1 x)) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

(defun cl-const-exprs-p (xs)
  (while (and xs (cl-const-expr-p (car xs)))
    (setq xs (cdr xs)))
  (not xs))

(defun cl-const-expr-val (x &optional cl-not-constant)
  (let ((cl-const-expr-p (cl-const-expr-p x)))
    (cond ((eq cl-const-expr-p t) (if (consp x) (nth 1 x) x))
	  ((eq cl-const-expr-p 'func) (nth 1 x))
	  (cl-not-constant))))

(defun cl-expr-access-order (x v)
  (if (cl-const-expr-p x) v
    (if (consp x)
	(progn
	  (while (setq x (cdr x)) (setq v (cl-expr-access-order (car x) v)))
	  v)
      (if (eq x (car v)) (cdr v) '(t)))))

;;; Count number of times X refers to Y.  Return nil for 0 times.
(defun cl-expr-contains (x y)
  (cond ((equal y x) 1)
	((and (consp x) (not (memq (car-safe x) '(quote function function*))))
	 (let ((sum 0))
	   (while x
	     (setq sum (+ sum (or (cl-expr-contains (pop x) y) 0))))
	   (and (> sum 0) sum)))
	(t nil)))

(defun cl-expr-contains-any (x y)
  (while (and y (not (cl-expr-contains x (car y)))) (pop y))
  y)

;;; Check whether X may depend on any of the symbols in Y.
(defun cl-expr-depends-p (x y)
  (and (not (cl-const-expr-p x))
       (or (not (cl-safe-expr-p x)) (cl-expr-contains-any x y))))

;;; Symbols.

(defvar *gensym-counter*)

;; XEmacs change: gensym and gentemp have been moved to cl.el.


;;; Program structure.

;;;###autoload
(defmacro defun* (name arglist &optional docstring &rest body)
  "Define NAME as a function.
Like normal `defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...).

\"Full Common Lisp conventions\" means that:

-- In addition to &optional and &rest, the lambda-list keywords &key,
   &allow-other-keys, and &aux are allowed.

-- The format of the arguments to &optional is expanded: As well as simple
   variables, they can be lists of the form (VAR [INITFORM [SVAR]]); when
   no argument is available for VAR, INITFORM is evaluated (or nil, if
   INITFORM is omitted) and stored as VAR's value, and SVAR is bound to t.
   If an argument is available for VAR, and INITFORM is unused, SVAR is
   bound to nil.

-- &key specifies keyword arguments.  The format of each argument is
   VAR || ( { VAR || (KEYWORD VAR) } [INITFORM [SVAR]]).  

   If VAR is specified on its own, VAR is bound within BODY to the value
   supplied by the caller for the corresponding keyword; for example, &key
   my-value means callers write :my-value RUNTIME-EXPRESSION.

   If (VAR INITFORM) is specified, INITFORM is an expression evaluated at
   runtime to determine a default value for VAR.

   If (VAR INITFORM SVAR) is specified, SVAR is variable available within
   BODY that is non-nil if VAR was explicitly specified in the calling
   expression.

   If ((KEYWORD VAR)) is specified, KEYWORD is the keyword to be used by
   callers, and VAR is the corresponding variable binding within BODY.

   In calls to NAME, values for a given keyword may be supplied multiple
   times.  The first value is the only one used.

-- &allow-other-keys means that if other keyword arguments are given that are
   not specifically list in the arg list, they are allowed, rather than an
   error being signalled.  They can be retrieved with an &rest form.

-- &aux specifies extra bindings, exactly like a `let*' enclosing the body.
   The format of each binding is VAR || (VAR [INITFORM]) -- exactly like the
   format of `let'/`let*' bindings.
"
  (list* 'defun name (cdr (cl-transform-lambda (list* arglist docstring body)
                                               name))))

;;;###autoload
(defmacro defmacro* (name arglist &optional docstring &rest body)
  "Define NAME as a macro.
Like normal `defmacro', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...).

\"Full Common Lisp conventions\" means that:

-- The lambda-list keywords &optional, &rest, &key, &allow-other-keys, and
   &aux are allowed, as in `defun*'.

-- Three additional lambda-list keywords are allowed: &body, &whole, and
   &environment:

   &body is equivalent to &rest, but is intended to indicate that the
   following arguments are the body of some piece of code, and should be
   indented as such.

   &whole must come first; it is followed by a single variable that, at
   macro expansion time, reflects all the arguments supplied to the macro,
   as if it had been declared with a single &rest argument.

   &environment allows access to the macro environment at the time of
   expansion; it is most relevant when it's necessary to force macro expansion
   of the body of a form at the time of macro expansion of its top level.
   &environment is followed by variable name, and this variable will be bound
   to the value of the macro environment within BODY. See the ENVIRONMENT
   argument to `macroexpand'.

-- The macro arg list syntax allows for \"destructuring\" -- see also
   `destructuring-bind', which destructures exactly like `defmacro*', and
   `loop', which does a rather different way of destructuring.  Anywhere
   that a simple argument may appear, and (if following a lambda-list
   keyword) a list may not normally appear, an embedded lambda list can be
   substituted.  (The format of the embedded lambda list is exactly the
   same as for a top-level list except that &environment is not allowed.)
   When matching this lambda list against a caller-specified argument, that
   argument is treated as a list and normal lambda-list processing occurs,
   just as if the entire operation were happening at top level.
   Furthermore, any lambda list, embedded or top-level, can be dotted at its
   end, and this will cause matching with an appropriate dotted list given
   as an argument.

   See `loop' for practical examples of destructuring, but
   keep in mind that `loop' destructuring is somewhat different from macro
   destructuring in that

   (a) Macro destructuring has extra features in the various lambda-list
       keywords, allowing for special processing of a list other than just
       simple matching.
   (b) Macro destructuring is strict, in that an error is signalled if the
       actual structure does not match the expected structure.  On the
       other hand, loop destructuring is lax -- extra arguments in a list
       are ignored, not enough arguments cause the remaining parameters to
       receive a value of nil, etc.
"
  (list* 'defmacro name (cdr (cl-transform-lambda (list* arglist docstring body)
                                                  name))))

;;;###autoload
(defmacro function* (symbol-or-lambda)
  "Introduce a function.
Like normal `function', except that if argument is a lambda form, its
ARGLIST allows full Common Lisp conventions."
  `(function
    ,(if (eq (car-safe symbol-or-lambda) 'lambda)
         (cons 'lambda (cdr (cl-transform-lambda (cdr symbol-or-lambda)
                                                 'cl-none)))
       symbol-or-lambda)))

(defun cl-transform-function-property (func prop form)
  (cl-macroexpand-all
  `(put ',func ',prop #'(lambda ,@(cdr (cl-transform-lambda form func))))
  byte-compile-macro-environment))

(defconst lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &whole &body &environment))

(defvar bind-block) (defvar bind-defs) (defvar bind-enquote)
(defvar bind-lets) (defvar bind-forms)

;; npak@ispras.ru
(defun cl-upcase-arg (arg)
  ;; Changes all non-keyword symbols in `ARG' to symbols
  ;; with name in upper case.
  ;; ARG is either symbol or list of symbols or lists
  (cond ((symbolp arg)
	 ;; Do not upcase &optional, &key etc.
	 (if (memq arg lambda-list-keywords)
             arg
	   (make-symbol (upcase (symbol-name arg)))))
	((listp arg)
	 (let ((arg (copy-list arg)) junk)
	   ;; Clean the list
	   (let ((p (last arg))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
	   (if (setq junk (cadr (memq '&cl-defs arg)))
	       (setq arg (delete* '&cl-defs (delete* junk arg))))
	   (if (memq '&cl-quote arg)
	       (setq arg (delete* '&cl-quote arg)))
	   (mapcar 'cl-upcase-arg arg)))
	(t arg)))                         ; Maybe we are in initializer

;; npak@ispras.ru, modified by ben@666.com
;;;###autoload
(defun cl-function-arglist (arglist)
  "Returns string with printed representation of arguments list.
Supports Common Lisp lambda lists."
  (if (not (or (listp arglist) (symbolp arglist)))
      "Not available"
    (check-argument-type #'true-list-p arglist)
    (let ((print-gensym nil))
      (condition-case nil
	  (let ((args (cond ((null arglist) nil)
			    ((listp arglist) (cl-upcase-arg arglist))
			    ((symbolp arglist)
			     (cl-upcase-arg (list '&rest arglist)))
			    (t (wrong-type-argument 'listp arglist)))))
	    (if args (prin1-to-string args) "()"))
	(t "Not available")))))

(defun cl-transform-lambda (form bind-block)
  "Transform a lambda expression to support Common Lisp conventions.

FORM is the cdr of the lambda expression.  BIND-BLOCK is the implicit block
name that's added, typically the name of the associated function. It can be
the symbol `cl-none', to indicate no implicit block is needed.

The Common Lisp conventions described are those detailed in the `defun*' and
`defmacro*' docstrings.  This function returns a list with the first element
nil, to be ignored. The rest of the list represents a transformed lambda
expression, with any argument list parsing code necessary, and a surrounding
block."
  (let* ((args (car form)) (body (cdr form))
	 (bind-defs nil) (bind-enquote nil)
         (bind-lets nil) (bind-forms nil)
	 (header nil) (simple-args nil)
         (complex-arglist (cl-function-arglist args))
         (doc ""))
    (while (or (stringp (car body)) (eq (car-safe (car body)) 'interactive))
      (push (pop body) header))
    (setq args (if (listp args) (copy-list args) (list '&rest args)))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (if (setq bind-defs (cadr (memq '&cl-defs args)))
	(setq args (delete* '&cl-defs (delete* bind-defs args))
	      bind-defs (cadr bind-defs)))
    (if (setq bind-enquote (memq '&cl-quote args))
	(setq args (delete* '&cl-quote args)))
    (if (memq '&whole args) (error "&whole not currently implemented"))
    (let* ((p (memq '&environment args)) (v (cadr p)))
      (if p (setq args (nconc (delete* (car p) (delete* v args))
                              `(&aux (,v byte-compile-macro-environment))))))
    (while (and args (symbolp (car args))
		(not (memq (car args) '(nil &rest &body &key &aux)))
		(not (and (eq (car args) '&optional)
			  (or bind-defs (consp (cadr args))))))
      (push (pop args) simple-args))
    (or (eq bind-block 'cl-none)
	(setq body (list (list* 'block bind-block body))))
    (setq simple-args (nreverse simple-args)
          header (nreverse header))
    (if (null args)
	(list* nil simple-args (nconc header body))
      (if (memq '&optional simple-args) (push '&optional args))
      (cl-do-arglist args nil (- (length simple-args)
				 (if (memq '&optional simple-args) 1 0)))
      (setq bind-lets (nreverse bind-lets))
      ;; This code originally needed to create the keywords itself, that
      ;; wasn't done by the Lisp reader; the first element of the result
      ;; list comprised code to do this. It's not used any more.
      (list* nil (prog1
                     (setq simple-args
                           (nconc simple-args
                                  (list '&rest (car (pop bind-lets)))))
                   ;; Add CL lambda list to documentation, if the CL lambda
                   ;; list differs from the non-CL lambda
                   ;; list. npak@ispras.ru
                   (unless (equal complex-arglist
                                  (cl-function-arglist simple-args))
                     (and (stringp (car header)) (setq doc (pop header)))
                     ;; Stick the arguments onto the end of the doc string
                     ;; in a way that will be recognized specially by
                     ;; `function-arglist'.
                     (push (concat doc "\n\narguments: " complex-arglist "\n")
                           header)))
	     ;; XEmacs change: we add usage information using Nickolay's
	     ;; approach above
	     (nconc header
		    (list (nconc (list 'let* bind-lets)
				 (nreverse bind-forms) body)))))))

(defun cl-do-arglist (args expr &optional num)   ; uses bind-*
  (if (nlistp args)
      (if (or (memq args lambda-list-keywords) (not (symbolp args)))
	  (error "Invalid argument name: %s" args)
	(push (list args expr) bind-lets))
    (setq args (copy-list args))
    (let ((p (last args))) (if (cdr p) (setcdr p (list '&rest (cdr p)))))
    (let ((p (memq '&body args))) (if p (setcar p '&rest)))
    (if (memq '&environment args) (error "&environment used incorrectly"))
    (let ((save-args args)
	  (restarg (memq '&rest args))
	  (safety (if (cl-compiling-file) cl-optimize-safety 3))
	  (keys nil)
	  (laterarg nil) (exactarg nil) minarg)
      (or num (setq num 0))
      (if (listp (cadr restarg))
	  (setq restarg (gensym "--rest--"))
	(setq restarg (cadr restarg)))
      (push (list restarg expr) bind-lets)
      (if (eq (car args) '&whole)
	  (push (list (cl-pop2 args) restarg) bind-lets))
      (let ((p args))
	(setq minarg restarg)
	(while (and p (not (memq (car p) lambda-list-keywords)))
	  (or (eq p args) (setq minarg (list 'cdr minarg)))
	  (setq p (cdr p)))
	(if (memq (car p) '(nil &aux))
	    (setq minarg (list 'eql (list 'length restarg)
			       (length (ldiff args p)))
		  exactarg (not (eq args p)))))
      (while (and args (not (memq (car args) lambda-list-keywords)))
	(let ((poparg (list (if (or (cdr args) (not exactarg)) 'pop 'car)
			    restarg)))
	  (cl-do-arglist
	   (pop args)
	   (if (or laterarg (= safety 0)) poparg
	     (list 'if minarg poparg
		   (list 'signal '(quote wrong-number-of-arguments)
			 (list 'list (and (not (eq bind-block 'cl-none))
					  (list 'quote bind-block))
			       (list 'length restarg)))))))
	(setq num (1+ num) laterarg t))
      (while (and (eq (car args) '&optional) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (if (cddr arg) (cl-do-arglist (nth 2 arg) (list 'and restarg t)))
	    (let ((def (if (cdr arg) (nth 1 arg)
			 (or (car bind-defs)
			     (nth 1 (assq (car arg) bind-defs)))))
		  (poparg (list 'pop restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (cl-do-arglist (car arg)
			     (if def (list 'if restarg poparg def) poparg))
	      (setq num (1+ num))))))
      (if (eq (car args) '&rest)
	  (let ((arg (cl-pop2 args)))
	    (if (consp arg) (cl-do-arglist arg restarg)))
	(or (eq (car args) '&key) (= safety 0) exactarg
	    (push (list 'if restarg
			   (list 'signal '(quote wrong-number-of-arguments)
				 (list 'list
				       (and (not (eq bind-block 'cl-none))
					    (list 'quote bind-block))
				       (list '+ num (list 'length restarg)))))
		     bind-forms)))
      (while (and (eq (car args) '&key) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (let ((arg (pop args)))
	    (or (consp arg) (setq arg (list arg)))
	    (let* ((karg (if (consp (car arg))
			     ;; It's possible to use non-keywords here, as
			     ;; in the KEYWORD-ARGUMENT-NAME-PACKAGE Common
			     ;; Lisp issue:
			     (caar arg)
			   ;; Use read instead of intern in case we ever
			   ;; actually get packages and keywords are no
			   ;; longer in obarray:
			   (read (concat ":" (symbol-name (car arg))))))
		   (varg (if (consp (car arg)) (cadar arg) (car arg)))
		   (def (if (cdr arg) (cadr arg)
			  (or (car bind-defs) (cadr (assq varg bind-defs)))))
		   (look (list 'memq (quote-maybe karg) restarg)))
	      (and def bind-enquote (setq def (list 'quote def)))
	      (if (cddr arg)
		  (let* ((temp (or (nth 2 arg) (gensym)))
			 (val (list 'car (list 'cdr temp))))
		    (cl-do-arglist temp look)
		    (cl-do-arglist varg
				   (list 'if temp
					 (list 'prog1 val (list 'setq temp t))
					 def)))
		(cl-do-arglist
		 varg
		 (list 'car
		       (list 'cdr
			     (if (null def)
				 look
			       (list 'or look
				     (if (eq (cl-const-expr-p def) t)
					 (list
					  'quote
					  (list nil (cl-const-expr-val def)))
				       (list 'list nil def))))))))
	      (push karg keys)))))
      (setq keys (nreverse keys))
      (or (and (eq (car args) '&allow-other-keys) (pop args))
	  (null keys) (= safety 0)
	  (let* ((var (gensym "--keys--"))
		 (allow '(:allow-other-keys))
		 (check (list
			 'while var
			 (list
			  'cond
			  (list (list 'memq (list 'car var)
				      (list 'quote (append keys allow)))
				(list 'setq var (list 'cdr (list 'cdr var))))
			  (list (list 'car
				      (list 'cdr
					    (list 'memq (car allow)
						  restarg)))
				(list 'setq var nil))
			  (list t
				(list
				 'error
                                 ''invalid-keyword-argument
				 (list 'car var)))))))
	    (push (list 'let (list (list var restarg)) check) bind-forms)))
      (while (and (eq (car args) '&aux) (pop args))
	(while (and args (not (memq (car args) lambda-list-keywords)))
	  (if (consp (car args))
	      (if (and bind-enquote (cadar args))
		  (cl-do-arglist (caar args)
				 (list 'quote (cadr (pop args))))
		(cl-do-arglist (caar args) (cadr (pop args))))
	    (cl-do-arglist (pop args) nil))))
      (if args (error "Malformed argument list %s" save-args)))))

(defun cl-arglist-args (args)
  (if (nlistp args) (list args)
    (let ((res nil) (kind nil) arg)
      (while (consp args)
	(setq arg (pop args))
	(if (memq arg lambda-list-keywords) (setq kind arg)
	  (if (eq arg '&cl-defs) (pop args)
	    (and (consp arg) kind (setq arg (car arg)))
	    (and (consp arg) (cdr arg) (eq kind '&key) (setq arg (cadr arg)))
	    (setq res (nconc res (cl-arglist-args arg))))))
      (nconc res (and args (list args))))))

;;;###autoload
(defmacro destructuring-bind (args expr &rest body)
  "Bind the arguments in ARGS to EXPR then eval BODY.
This is similar to `let' but it does \"destructuring\", in that it matches
the structure of ARGS to the structure of EXPR and binds corresponding
arguments in ARGS to their values in EXPR.  The format of ARGS, and the
way the destructuring works, is exactly like the destructuring that occurs
in `defmacro*'; see that for more information.

An alternative means of destructuring is using the `loop' macro. `loop'
gives practical examples of destructuring.  `defmacro*' describes the
differences between loop and macro-style destructuring.

You can rewrite a call to (destructuring-bind ARGS EXPR &rest BODY) using
`loop', approximately like this:

  (loop for ARGS = EXPR
    return (progn BODY))

I say \"approximately\" because the destructuring works in a somewhat
different fashion, although for most reasonably simple constructs the
results will be the same."
  (let ((bind-block 'cl-none) bind-lets bind-forms bind-defs)
    (cl-do-arglist (or args '(&aux)) expr)
    (nconc (list 'let* (nreverse bind-lets)) (nreverse bind-forms) body)))

;;; The `eval-when' form.

(defvar cl-not-toplevel nil)

;;;###autoload
(defmacro eval-when (when &rest body)
  "Control when BODY is evaluated.
If `compile' is in WHEN, BODY is evaluated when compiled at top-level.
If `load' is in WHEN, BODY is evaluated when loaded after top-level compile.
If `eval' is in WHEN, BODY is evaluated when interpreted or at non-top-level.

arguments: ((&rest WHEN) &body BODY)"
  (if (and (fboundp 'cl-compiling-file) (cl-compiling-file)
	   (not cl-not-toplevel) (not (boundp 'for-effect)))  ; horrible kludge
      (let ((comp (or (memq 'compile when) (memq :compile-toplevel when)))
	    (cl-not-toplevel t))
	(if (or (memq 'load when) (memq :load-toplevel when))
	    (if comp (cons 'progn (mapcar 'cl-compile-time-too body))
	      (list* 'if nil nil body))
	  (progn (if comp (eval (cons 'progn body))) nil)))
    (and (or (memq 'eval when) (memq :execute when))
	 (cons 'progn body))))

(defun cl-compile-time-too (form)
  (or (and (symbolp (car-safe form)) (get (car-safe form) 'byte-hunk-handler))
      (setq form (macroexpand
		  form (cons '(eval-when) byte-compile-macro-environment))))
  (cond ((eq (car-safe form) 'progn)
	 (cons 'progn (mapcar 'cl-compile-time-too (cdr form))))
	((eq (car-safe form) 'eval-when)
	 (let ((when (nth 1 form)))
	   (if (or (memq 'eval when) (memq :execute when))
	       (list* 'eval-when (cons 'compile when) (cddr form))
	     form)))
	(t (eval form) form)))

;;; Conditional control structures.

;;;###autoload
(defmacro case (expr &rest clauses)
  "Evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and compared
against each key in each KEYLIST; the corresponding BODY is evaluated.
If no clause succeeds, case returns nil.  A single atom may be used in
place of a KEYLIST of one atom.  A KEYLIST of `t' or `otherwise' is
allowed only in the final clause, and matches if no other keys match.
Key values are compared by `eql'."
  (let* ((temp (if (cl-simple-expr-p expr 3) expr (gensym)))
	 (head-list nil)
	 (last-clause (car (last clauses)))
	 (body (cons
		'cond
		(mapcar
		 #'(lambda (c)
		     (cons (cond ((memq (car c) '(t otherwise))
				  ;; XEmacs addition: check for last clause
				  (or (eq c last-clause)
				      (error
				       "`%s' is allowed only as the last case clause"
				       (car c)))
				  t)
				 ((eq (car c) 'ecase-error-flag)
				  (list 'error "ecase failed: %s, %s"
					temp (list 'quote (reverse head-list))))
				 ((listp (car c))
				  (setq head-list (append (car c) head-list))
				  (list 'member* temp (list 'quote (car c))))
				 (t
				  (if (memq (car c) head-list)
				      (error "Duplicate key in case: %s"
					     (car c)))
				  (push (car c) head-list)
				  (list 'eql temp (list 'quote (car c)))))
			   (or (cdr c) '(nil))))
		 clauses))))
    (if (eq temp expr) body
      (list 'let (list (list temp expr)) body))))

;; #### CL standard also requires `ccase', which signals a continuable
;; error (`cerror' in XEmacs).  However, I don't think it buys us
;; anything to introduce it, as there is probably much more CL stuff
;; missing, and the feature is not essential.  --hniksic

;;;###autoload
(defmacro ecase (expr &rest clauses)
  "Like `case', but error if no case fits.
`otherwise'-clauses are not allowed."
  ;; XEmacs addition: disallow t and otherwise
  (let ((disallowed (or (assq t clauses)
			(assq 'otherwise clauses))))
    (if disallowed
	(error "`%s' is not allowed in ecase" (car disallowed))))
  (list* 'case expr (append clauses '((ecase-error-flag)))))

;;;###autoload
(defmacro typecase (expr &rest clauses)
  "Evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (TYPE BODY...).  EXPR is evaluated and, if it
satisfies TYPE, the corresponding BODY is evaluated.  If no clause succeeds,
typecase returns nil.  A TYPE of `t' or `otherwise' is allowed only in the
final clause, and matches if no other keys match."
  (let* ((temp (if (cl-simple-expr-p expr 3) expr (gensym)))
	 (type-list nil)
	 (body (cons
		'cond
		(mapcar
		 #'(lambda (c)
		     (cons (cond ((eq (car c) 'otherwise) t)
				 ((eq (car c) 'ecase-error-flag)
				  (list 'error "etypecase failed: %s, %s"
					temp (list 'quote (reverse type-list))))
				 (t
				  (push (car c) type-list)
				  (cl-make-type-test temp (car c))))
			   (or (cdr c) '(nil))))
		 clauses))))
    (if (eq temp expr) body
      (list 'let (list (list temp expr)) body))))

;;;###autoload
(defmacro etypecase (expr &rest clauses)
  "Like `typecase', but error if no case fits.
`otherwise'-clauses are not allowed."
  (list* 'typecase expr (append clauses '((ecase-error-flag)))))


;;; Blocks and exits.
(defvar cl-active-block-names nil)

;;;###autoload
(defmacro block (name &rest body)
  "Define a lexically-scoped block named NAME.
NAME may be any symbol.  Code inside the BODY forms can call `return-from'
to jump prematurely out of the block.  This differs from `catch' and `throw'
in two respects:  First, the NAME is an unevaluated symbol rather than a
quoted symbol or other form; and second, NAME is lexically rather than
dynamically scoped:  Only references to it within BODY will work.  These
references may appear inside macro expansions and in lambda expressions, but
not inside other functions called from BODY."
  (let ((cl-active-block-names (acons name (copy-symbol name)
				      cl-active-block-names))
	(body (cons 'progn body)))
    ;; Tell the byte-compiler this is a block, not a normal catch call, and
    ;; as such it can eliminate it if that's appropriate:
    (put (cdar cl-active-block-names) 'cl-block-name name)
    `(catch ',(cdar cl-active-block-names)
      ;; Can't use &environment, since #'block is used in
      ;; #'cl-transform-lambda.
      ,(cl-macroexpand-all body byte-compile-macro-environment))))

;;;###autoload
(defmacro return (&optional result)
  "Return from the block named nil.
This is equivalent to `(return-from nil RESULT)'."
  `(return-from nil ,result))

;;;###autoload
(defmacro return-from (name &optional result)
  "Return from the block named NAME.
This jumps out to the innermost enclosing `(block NAME ...)' form,
returning RESULT from that form (or nil if RESULT is omitted).
This is compatible with Common Lisp, but note that `defun' and
`defmacro' do not create implicit blocks as they do in Common Lisp."
  `(throw ',(or (cdr (assq name cl-active-block-names))
                ;; Tell the byte-compiler the original name of the block,
                ;; leave any warning to it.
                (let ((copy-symbol (copy-symbol name)))
                  (put copy-symbol 'cl-block-name name)
                  copy-symbol))
           ,result))

;;; The "loop" macro.

(defvar args) (defvar loop-accum-var) (defvar loop-accum-vars)
(defvar loop-bindings) (defvar loop-body) (defvar loop-destr-temps)
(defvar loop-finally) (defvar loop-finish-flag) (defvar loop-first-flag)
(defvar loop-initially) (defvar loop-map-form) (defvar loop-name)
(defvar loop-result) (defvar loop-result-explicit)
(defvar loop-result-var) (defvar loop-steps) (defvar loop-symbol-macs)

;;;###autoload
(defmacro loop (&rest clauses)
  "The Common Lisp `loop' macro.

The loop macro consists of a series of clauses, which do things like
iterate variables, set conditions for exiting the loop, accumulating values
to be returned as the return value of the loop, and executing arbitrary
blocks of code.  Each clause is processed in turn, and the loop executes its
body repeatedly until an exit condition is hit.

It's important to understand that loop clauses such as `for' and `while',
which look like loop-establishing constructs, don't actually *establish* a
loop\; the looping is established by the `loop' clause itself, which will
repeatedly process its body until told to stop.  `while' merely establishes
a condition which, when true, causes the loop to finish, and `for' sets a
variable to different values on each iteration (e.g. successive elements of
a list) and sets an exit condition when there are no more values.  This
means, for example, that if two `for' clauses appear, you don't get two
nested loops, but instead two variables that are stepped in parallel, and
two exit conditions, either of which, if triggered, will cause the loop to
end.  Similarly for a loop with a `for' and a `while' clause.  For example:

\(loop
  for x in list
  while x
  do ...)

In each successive iteration, X is set to the next element of the list.  If
there are no more elements, or if any element is nil (the `while' clause),
the loop exits.  Otherwise, the block of code following `do' is executed.)

This example also shows that some clauses establish variable bindings --
essentially like a `let' binding -- and that following clauses can
reference these variables.  Furthermore, the entire loop is surrounded by a
block named nil (unless the `named' clause is given), so you can return
from the loop using the macro `return'. (The other way to exit the loop is
through the macro `loop-finish'.  The difference is that some loop clauses
establish or accumulate a value to be returned, and `loop-finish' returns
this. `return', however, can only return an explicitly-specified value.
NOTE CAREFULLY: There is a loop clause called `return' as well as a
standard Lisp macro called `return'.  Normally they work similarly\; but if
you give the loop a name with `named', you will need to use the macro
`return-from'.)

Another extremely useful feature of loops is called \"destructuring\".  If,
in place of VAR, a list (possibly dotted, possibly a tree of arbitrary
complexity) is given, the value to be assigned is assumed to have a similar
structure to the list given, and variables in the list will be matched up
with corresponding elements in the structure.  For example:

\(loop
  for (x y) in '((foo 1) (bar 2) (baz 3))
  do (puthash x y some-hash-table))

will add three elements to a hash table, mapping foo -> 1, bar -> 2, and
baz -> 3.  As other examples, you can conveniently process alists using

\(loop for (x . y) in alist do ...)

and plists using

\(loop for (x y) on plist by #'cddr do ...)

Destructuring is forgiving in that mismatches in the number of elements on
either size will be handled gracefully, either by ignoring or initializing
to nil.  Destructuring is extremely powerful, and is probably the single
most useful feature of `loop'.

Other useful features of loops are iterating over hash-tables, collecting values into lists, and being able to modify lists in-place as you iterate over them.  As an example of the first two,

\(loop for x being the hash-key in table using (hash-value y)
  collect (cons x y))

converts hash-table TABLE to an alist. (What `collect' actually does is
push its value onto the end of an internal list and establish this list as
the default return value of the loop.  See below for more information.)

An example of in-place modification is

\(setq foo '(1 3 5))
\(loop for x in-ref foo do
  (setf x (* x x)))

after which foo will contain '(1 9 25).

If you don't understand how a particular loop clause works, create an
example and use `macroexpand-sexp' to expand the macro.

Valid clauses are:

\(NOTE: Keywords in lowercase\; slashes separate different possibilities
for keywords, some of which are synonymous\; brackets indicate optional
parts of the clause.  In all of the clauses with `being', the word `being',
the words `each' or `the', and the difference between singular and plural
keywords are all just syntactic sugar.  Stylistically, you should write
either `being each foo' or `being the foos'.)

  for VAR from/upfrom/downfrom NUM1 to/upto/downto/above/below NUM2 [by NUMSTEP]
    Step VAR across numbers.  `upfrom', `upto', and `below' explicitly
    indicate upward stepping\; `downfrom', `downto', and `above' explicitly
    indicate downward stepping. (If none of these is given, the default is
    upward.) `to', `upto', and `downto' cause stepping to include NUM2 as
    the last iteration, while `above' and `below' stop just before reaching
    NUM2.  `by' can be given to indicate a stepping increment other than 1.

  for VAR in LIST [by FUNC]
    Step VAR over elements of a LIST.  FUNC specifies how to get successive
    sublists and defaults to `cdr'.

  for VAR on LIST [by FUNC]
    Step VAR over tails of a LIST.  FUNC specifies how to get successive
    sublists and defaults to `cdr'.

  for VAR in-ref LIST [by FUNC]
    Step VAR over elements of a LIST, like `for ... in', except the VAR is
    bound using `symbol-macrolet' instead of `let'.  In essence, VAR is set
    to a \"reference\" to the list element instead of the element itself\;
    this us, you can destructively modify the list using `setf' on VAR, and
    any changes to the list will \"magically\" reflect themselves in
    subsequent uses of VAR.

  for VAR = INIT [then EXPR]
    Set VAR on each iteration of the loop.  If only INIT is given, use it
    on each iteration.  Otherwise, use INIT on the first iteration and EXPR
    on subsequent ones.

  for VAR across/across-ref ARRAY
    Step VAR across a sequence other than a list (string, vector, bit
    vector).  If `across-ref' is given, VAR is bound using
    `symbol-macrolet' instead of `let' -- see above.

  for VAR being each/the element/elements in/of/in-ref/of-ref SEQUENCE [using (index INDEX-VAR)]
    Step VAR across any sequence.  A variable can be specified with a
    `using' phrase to receive the index, starting at 0.  If `in-ref' or
    `of-ref' is given, VAR is bound using `symbol-macrolet' instead of
    `let' -- see above.

  for VAR being each/the hash-key/hash-keys/hash-value/hash-values in/of HASH-TABLE [using (hash-value/hash-key OTHER-VAR)]

  for VAR being each/the hash-key/hash-keys/hash-value/hash-values in/of HASH-TABLE [using (hash-value/hash-key OTHER-VAR)]
    Map VAR over a hash table.  The various keywords are synonymous except
    those that distinguish between keys and values.  The `using' phrase is
    optional and allows both key and value to be bound.

  for VAR being each/the symbol/present-symbol/external-symbol/symbols/present-symbols/external-symbols in/of OBARRAY
    Map VAR over the symbols in an obarray.  All symbol keywords are
    currently synonymous.

  for VAR being each/the extent/extents [in/of BUFFER-OR-STRING] [from POS] [to POS]
    Map VAR over the extents in a buffer or string, defaulting to the
    current buffer, the beginning and the end, respectively.

  for VAR being each/the interval/intervals [in/of BUFFER-OR-STRING] [property PROPERTY] [from POS] [to POS]
    Map VAR over the intervals without property change in a buffer or
    string, defaulting to the current buffer, the beginning and the end,
    respectively.  If PROPERTY is given, iteration occurs using
    `next-single-property-change'\; otherwise, using
    `next-property-change'.

  for VAR being each/the window/windows [in/of FRAME]
    Step VAR over the windows in FRAME, defaulting to the selected frame.

  for VAR being each/the frame/frames
    Step VAR over all frames.

  for VAR being each/the buffer/buffers [by FUNC]
    Step VAR over all buffers.  This is actually equivalent to
    `for VAR in (buffer-list) [by FUNC]'.

  for VAR being each/the key-code/key-codes/key-seq/key-seqs/key-binding/key-bindings in KEYMAP [using (key-code/key-codes/key-seq/key-seqs/key-binding/key-bindings OTHER-VAR)]
    Map VAR over the entries in a keymap.  Keyword `key-seq' causes
    recursive mapping over prefix keymaps occurring in the keymap, with VAR
    getting the built-up sequence (a vector).  Otherwise, mapping does not
    occur recursively.  `key-code' and `key-seq' refer to what is bound
    (second argument of `define-key'), and `key-binding' what it's bound to
    (third argument of `define-key').

  as VAR ...
    `as' is a synonym for `for'.

  and VAR ...
    `and' clauses have the same syntax as `for' clauses except that the
    variables in the clause are bound in parallel with a preceding
    `and'/`for' clause instead of in series.

  with VAR = INIT
    Set VAR to INIT once, before doing any iterations.

  repeat NUM
    Exit the loop if more than NUM iterations have occurred.

  while COND
    Exit the loop if COND isn't true.

  until COND
    Exit the loop if COND is true.

  collect EXPR [into VAR]
    Push EXPR onto the end of a list of values -- stored either in VAR or a
    temporary variable that will be returned as the return value of the
    loop if it terminates through an exit condition or a call to
    `loop-finish'.

  append EXPR [into VAR]
    Append EXPR (a list) onto the end of a list of values, like `collect'.

  nconc EXPR [into VAR]
    Nconc EXPR (a list) onto the end of a list of values, like `collect'.

  concat EXPR [into VAR]
    Concatenate EXPR (a string) onto the end of a string of values, like
    `collect'.

  vconcat EXPR [into VAR]
    Concatenate EXPR (a vector) onto the end of a vector of values, like
    `collect'.

  bvconcat EXPR [into VAR]
    Concatenate EXPR (a bit vector) onto the end of a bit vector of values,
    like `collect'.

  sum EXPR [into VAR]
    Add EXPR to a value, like `collect'.

  count EXPR [into VAR]
    If EXPR is true, increment a value by 1, like `collect'.

  maximize EXPR [into VAR]
    IF EXPR is greater than a value, replace the value with EXPR, like
    `collect'.

  minimize EXPR [into VAR]
    IF EXPR is less than a value, replace the value with EXPR, like
    `collect'.

  always COND
    If COND is true, continue the loop and set the loop return value (the
    same value that's manipulated by `collect' and friends and is returned
    by a normal loop exit or an exit using `loop-finish') to t\; otherwise,
    exit the loop and return nil.  The effect is to determine and return
    whether a condition is true \"always\" (all iterations of the loop).

  never COND
    If COND is false, continue the loop and set the loop return value (like
    `always') to t\; otherwise, exit the loop and return nil.  The effect
    is to determine and return whether a condition is \"never\" true (all
    iterations of the loop).

  thereis COND
    If COND is true, exit the loop and return COND.

  if/when COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    If COND is true, execute the directly following clause(s)\; otherwise,
    execute the clauses following `else'.

  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    If COND is false, execute the directly following clause(s)\; otherwise, execute the clauses following `else'.

  do EXPRS...
    Execute the expressions (any Lisp forms).

  initially EXPRS...
    Execute EXPR once, before doing any iterations, and after values have
    been set using `with'.

  finally EXPRS...
    Execute EXPR once, directly before the loop terminates.  This will not
    be executed if the loop terminates prematurely as a result of `always',
    `never', `thereis', or `return'.

  return EXPR
    Exit from the loop and return EXPR.

  finally return EXPR
    Specify the value to be returned when the loop exits. (Unlike `return',
    this doesn't cause the loop to immediately exit\; it will exit whenever
    it normally would have.) This takes precedence over a return value
    specified with `collect' and friends or `always' and friends.

  named NAME
    Specify the name for block surrounding the loop, in place of nil.
    (See `block'.)
"
  (if (notany #'symbolp (set-difference clauses '(nil t)))
      (list 'block nil (list* 'while t clauses))
    (let ((loop-name nil)	(loop-bindings nil)
	  (loop-body nil)	(loop-steps nil)
	  (loop-result nil)	(loop-result-explicit nil)
	  (loop-result-var nil) (loop-finish-flag nil)
	  (loop-accum-var nil)	(loop-accum-vars nil)
	  (loop-initially nil)	(loop-finally nil)
	  (loop-map-form nil)   (loop-first-flag nil)
	  (loop-destr-temps nil) (loop-symbol-macs nil)
          (args (append clauses '(cl-end-loop))))
      (while (not (eq (car args) 'cl-end-loop)) (cl-parse-loop-clause))
      (if loop-finish-flag
	  (push (list (list loop-finish-flag t)) loop-bindings))
      (if loop-first-flag
	  (progn (push (list (list loop-first-flag t)) loop-bindings)
		 (push (list 'setq loop-first-flag nil) loop-steps)))
      (let* ((epilogue (nconc (nreverse loop-finally)
			      (list (or loop-result-explicit loop-result))))
	     (ands (cl-loop-build-ands (nreverse loop-body)))
	     (while-body (nconc (cadr ands) (nreverse loop-steps)))
	     (body (append
		    (nreverse loop-initially)
		    (list (if loop-map-form
			      (list 'block '--cl-finish--
				    (subst
				     (if (eq (car ands) t) while-body
				       (cons (list 'or (car ands)
						   '(return-from --cl-finish--
						      nil))
					     while-body))
				     '--cl-map loop-map-form))
			    (list* 'while (car ands) while-body)))
		    (if loop-finish-flag
			(if (equal epilogue '(nil))
			    ;; XEmacs change: When epilogue is nil and
			    ;; loop-finish-flag exists, you get a byte-compiler
			    ;; warning using the original (commented-out)
			    ;; code below.  So instead we create a form that
			    ;; gives the same result but uses loop-finish-flag.
			    ;; --ben
			    ;(list loop-result-var)
			    (list (list 'if loop-finish-flag
					loop-result-var loop-result-var))
			  (list (list 'if loop-finish-flag
				      (cons 'progn epilogue) loop-result-var)))
		      epilogue))))
	(if loop-result-var (push (list loop-result-var) loop-bindings))
	(while loop-bindings
	  (if (cdar loop-bindings)
	      (setq body (list (cl-loop-let (pop loop-bindings) body t)))
	    (let ((lets nil))
	      (while (and loop-bindings
			  (not (cdar loop-bindings)))
		(push (car (pop loop-bindings)) lets))
	      (setq body (list (cl-loop-let lets body nil))))))
	(if loop-symbol-macs
	    (setq body (list (list* 'symbol-macrolet loop-symbol-macs body))))
	(list* 'block loop-name body)))))

(defun cl-parse-loop-clause ()   ; uses args, loop-*
  (let ((word (pop args))
	(hash-types '(hash-key hash-keys hash-value hash-values))
	(key-types '(key-code key-codes key-seq key-seqs
		     key-binding key-bindings)))
    (cond

     ((null args)
      (error "Malformed `loop' macro"))

     ((eq word 'named)
      (setq loop-name (pop args)))

     ((eq word 'initially)
      (if (memq (car args) '(do doing)) (pop args))
      (or (consp (car args)) (error "Syntax error on `initially' clause"))
      (while (consp (car args))
	(push (pop args) loop-initially)))

     ((eq word 'finally)
      (if (eq (car args) 'return)
	  (setq loop-result-explicit (or (cl-pop2 args) '(quote nil)))
	(if (memq (car args) '(do doing)) (pop args))
	(or (consp (car args)) (error "Syntax error on `finally' clause"))
	(if (and (eq (caar args) 'return) (null loop-name))
	    (setq loop-result-explicit (or (nth 1 (pop args)) '(quote nil)))
	  (while (consp (car args))
	    (push (pop args) loop-finally)))))

     ((memq word '(for as))
      (let ((loop-for-bindings nil) (loop-for-sets nil) (loop-for-steps nil)
	    (ands nil))
	(while
	    (let ((var (or (pop args) (gensym))))
	      (setq word (pop args))
	      (if (eq word 'being) (setq word (pop args)))
	      (if (memq word '(the each)) (setq word (pop args)))
	      (if (memq word '(buffer buffers))
		  (setq word 'in args (cons '(buffer-list) args)))
	      (cond

	       ((memq word '(from downfrom upfrom to downto upto
			     above below by))
		(push word args)
		(if (memq (car args) '(downto above))
		    (error "Must specify `from' value for downward loop"))
		(let* ((down (or (eq (car args) 'downfrom)
				 (memq (caddr args) '(downto above))))
		       (excl (or (memq (car args) '(above below))
				 (memq (caddr args) '(above below))))
		       (start (and (memq (car args) '(from upfrom downfrom))
				   (cl-pop2 args)))
		       (end (and (memq (car args)
				       '(to upto downto above below))
				 (cl-pop2 args)))
		       (step (and (eq (car args) 'by) (cl-pop2 args)))
		       (end-var (and (not (cl-const-expr-p end)) (gensym)))
		       (step-var (and (not (cl-const-expr-p step))
				      (gensym))))
		  (and step (numberp step) (<= step 0)
		       (error "Loop `by' value is not positive: %s" step))
		  (push (list var (or start 0)) loop-for-bindings)
		  (if end-var (push (list end-var end) loop-for-bindings))
		  (if step-var (push (list step-var step)
					loop-for-bindings))
		  (if end
		      (push (list
				(if down (if excl '> '>=) (if excl '< '<=))
				var (or end-var end)) loop-body))
		  (push (list var (list (if down '- '+) var
					   (or step-var step 1)))
			   loop-for-steps)))

	       ((memq word '(in in-ref on))
		(let* ((on (eq word 'on))
		       (temp (if (and on (symbolp var)) var (gensym))))
		  (push (list temp (pop args)) loop-for-bindings)
		  (push (list 'consp temp) loop-body)
		  (if (eq word 'in-ref)
		      (push (list var (list 'car temp)) loop-symbol-macs)
		    (or (eq temp var)
			(progn
			  (push (list var nil) loop-for-bindings)
			  (push (list var (if on temp (list 'car temp)))
				   loop-for-sets))))
		  (push (list temp
				 (if (eq (car args) 'by)
				     (let ((step (cl-pop2 args)))
				       (if (and (memq (car-safe step)
						      '(quote function
							      function*))
						(symbolp (nth 1 step)))
					   (list (nth 1 step) temp)
					 (list 'funcall step temp)))
				   (list 'cdr temp)))
			   loop-for-steps)))

	       ((eq word '=)
		(let* ((start (pop args))
		       (then (if (eq (car args) 'then) (cl-pop2 args) start)))
		  (push (list var nil) loop-for-bindings)
		  (if (or ands (eq (car args) 'and))
		      (progn
			(push (list var
				       (list 'if
					     (or loop-first-flag
						 (setq loop-first-flag
						       (gensym)))
					     start var))
				 loop-for-sets)
			(push (list var then) loop-for-steps))
		    (push (list var
				   (if (eq start then) start
				     (list 'if
					   (or loop-first-flag
					       (setq loop-first-flag (gensym)))
					   start then)))
			     loop-for-sets))))

	       ((memq word '(across across-ref))
		(let ((temp-vec (gensym)) (temp-idx (gensym)))
		  (push (list temp-vec (pop args)) loop-for-bindings)
		  (push (list temp-idx -1) loop-for-bindings)
		  (push (list '< (list 'setq temp-idx (list '1+ temp-idx))
				 (list 'length temp-vec)) loop-body)
		  (if (eq word 'across-ref)
		      (push (list var (list 'aref temp-vec temp-idx))
			       loop-symbol-macs)
		    (push (list var nil) loop-for-bindings)
		    (push (list var (list 'aref temp-vec temp-idx))
			     loop-for-sets))))

	       ((memq word '(element elements))
		(let ((ref (or (memq (car args) '(in-ref of-ref))
			       (and (not (memq (car args) '(in of)))
				    (error "Expected `of'"))))
		      (seq (cl-pop2 args))
		      (temp-seq (gensym))
		      (temp-idx (if (eq (car args) 'using)
				    (if (and (eql (length (cadr args)) 2)
					     (eq (caadr args) 'index))
					(cadr (cl-pop2 args))
				      (error "Bad `using' clause"))
				  (gensym))))
		  (push (list temp-seq seq) loop-for-bindings)
		  (push (list temp-idx 0) loop-for-bindings)
		  (if ref
		      (let ((temp-len (gensym)))
			(push (list temp-len (list 'length temp-seq))
				 loop-for-bindings)
			(push (list var (list 'elt temp-seq temp-idx))
				 loop-symbol-macs)
			(push (list '< temp-idx temp-len) loop-body))
		    (push (list var nil) loop-for-bindings)
		    (push (list 'and temp-seq
				   (list 'or (list 'consp temp-seq)
					 (list '< temp-idx
					       (list 'length temp-seq))))
			     loop-body)
		    (push (list var (list 'if (list 'consp temp-seq)
					     (list 'pop temp-seq)
					     (list 'aref temp-seq temp-idx)))
			     loop-for-sets))
		  (push (list temp-idx (list '1+ temp-idx))
			   loop-for-steps)))

	       ((memq word hash-types)
		(or (memq (car args) '(in of)) (error "Expected `of'"))
		(let* ((table (cl-pop2 args))
		       (other (if (eq (car args) 'using)
				  (if (and (eql (length (cadr args)) 2)
					   (memq (caadr args) hash-types)
					   (not (eq (caadr args) word)))
				      (cadr (cl-pop2 args))
				    (error "Bad `using' clause"))
				(gensym))))
		  (if (memq word '(hash-value hash-values))
		      (setq var (prog1 other (setq other var))))
		  (setq loop-map-form
			(list 'maphash (list 'function
					     (list* 'lambda (list var other)
						    '--cl-map)) table))))

	       ((memq word '(symbol present-symbol external-symbol
			     symbols present-symbols external-symbols))
		(let ((ob (and (memq (car args) '(in of)) (cl-pop2 args))))
		  (setq loop-map-form
			(list 'mapatoms (list 'function
					      (list* 'lambda (list var)
						     '--cl-map)) ob))))

	       ((memq word '(overlay overlays extent extents))
		(let ((buf nil) (from nil) (to nil))
		  (while (memq (car args) '(in of from to))
		    (cond ((eq (car args) 'from) (setq from (cl-pop2 args)))
			  ((eq (car args) 'to) (setq to (cl-pop2 args)))
			  (t (setq buf (cl-pop2 args)))))
		  (setq loop-map-form
			(list 'map-extents
			      (list 'function (list 'lambda (list var (gensym))
						    '(progn . --cl-map) nil))
			      buf from to))))

	       ((memq word '(interval intervals))
		(let ((buf nil) (prop nil) (from nil) (to nil)
		      (var1 (gensym)) (var2 (gensym)))
		  (while (memq (car args) '(in of property from to))
		    (cond ((eq (car args) 'from) (setq from (cl-pop2 args)))
			  ((eq (car args) 'to) (setq to (cl-pop2 args)))
			  ((eq (car args) 'property)
			   (setq prop (cl-pop2 args)))
			  (t (setq buf (cl-pop2 args)))))
		  (if (and (consp var) (symbolp (car var)) (symbolp (cdr var)))
		      (setq var1 (car var) var2 (cdr var))
		    (push (list var (list 'cons var1 var2)) loop-for-sets))
		  (setq loop-map-form
			(list 'cl-map-intervals
			      (list 'function (list 'lambda (list var1 var2)
						    '(progn . --cl-map)))
			      buf prop from to))))

	       ((memq word key-types)
		(or (memq (car args) '(in of)) (error "Expected `of'"))
		(let* ((map (cl-pop2 args))
		       other-word
		       (other (if (eq (car args) 'using)
				  (if (and (eql (length (cadr args)) 2)
					   (memq (setq other-word (caadr args))
						 key-types)
					   (not (eq (caadr args) word)))
				      (cadr (cl-pop2 args))
				    (error "Bad `using' clause"))
			       (gensym))))
		  ;; XEmacs addition: track other-word
		  (when (memq word '(key-binding key-bindings))
		    (setq var (prog1 other (setq other var)))
		    (and other-word (setq word other-word)))
		  (setq loop-map-form
			(list (if (memq word '(key-seq key-seqs))
				  'cl-map-keymap-recursively 'map-keymap)
			      (list 'function (list* 'lambda (list var other)
						     '--cl-map)) map))))

	       ((memq word '(frame frames screen screens))
		(let ((temp (gensym)))
		  (push (list var  '(selected-frame))
			   loop-for-bindings)
		  (push (list temp nil) loop-for-bindings)
		  (push (list 'prog1 (list 'not (list 'eq var temp))
				 (list 'or temp (list 'setq temp var)))
			   loop-body)
		  (push (list var (list 'next-frame var))
			   loop-for-steps)))

	       ((memq word '(window windows))
		(let ((scr (and (memq (car args) '(in of)) (cl-pop2 args)))
		      (temp (gensym)))
		  (push (list var (if scr
					 (list 'frame-selected-window scr)
				       '(selected-window)))
			   loop-for-bindings)
		  (push (list temp nil) loop-for-bindings)
		  (push (list 'prog1 (list 'not (list 'eq var temp))
				 (list 'or temp (list 'setq temp var)))
			   loop-body)
		  (push (list var (list 'next-window var)) loop-for-steps)))

	       (t
		(let ((handler (and (symbolp word)
				    (get word 'cl-loop-for-handler))))
		  (if handler
		      (funcall handler var)
		    (error "Expected a `for' preposition, found %s" word)))))
	      (eq (car args) 'and))
	  (setq ands t)
	  (pop args))
	(if (and ands loop-for-bindings)
	    (push (nreverse loop-for-bindings) loop-bindings)
	  (setq loop-bindings (nconc (mapcar 'list loop-for-bindings)
				     loop-bindings)))
	(if loop-for-sets
	    (push (list 'progn
			   (cl-loop-let (nreverse loop-for-sets) 'setq ands)
			   t) loop-body))
	(if loop-for-steps
	    (push (cons (if ands 'psetq 'setq)
			   (apply 'append (nreverse loop-for-steps)))
		     loop-steps))))

     ((eq word 'repeat)
      (let ((temp (gensym)))
	(push (list (list temp (pop args))) loop-bindings)
	(push (list '>= (list 'setq temp (list '1- temp)) 0) loop-body)))

     ((memq word '(collect collecting))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum nil 'nreverse)))
	(if (eq var loop-accum-var)
	    (push (list 'progn (list 'push what var) t) loop-body)
	  (push (list 'progn
			 (list 'setq var (list 'nconc var (list 'list what)))
			 t) loop-body))))

     ((memq word '(nconc nconcing append appending))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum nil 'nreverse)))
	(push (list 'progn
		       (list 'setq var
			     (if (eq var loop-accum-var)
				 (list 'nconc
				       (list (if (memq word '(nconc nconcing))
						 'nreverse 'reverse)
					     what)
				       var)
			       (list (if (memq word '(nconc nconcing))
					 'nconc 'append)
				     var what))) t) loop-body)))

     ((memq word '(concat concating))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum "")))
	(push (list 'progn (list 'callf 'concat var what) t) loop-body)))

     ((memq word '(vconcat vconcating))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum [])))
	(push (list 'progn (list 'callf 'vconcat var what) t) loop-body)))

     ;; XEmacs addition: handle bit-vectors
     ((memq word '(bvconcat bvconcating))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum #*)))
	(push (list 'progn (list 'callf 'bvconcat var what) t) loop-body)))

     ((memq word '(sum summing))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum 0)))
	(push (list 'progn (list 'incf var what) t) loop-body)))

     ((memq word '(count counting))
      (let ((what (pop args))
	    (var (cl-loop-handle-accum 0)))
	(push (list 'progn (list 'if what (list 'incf var)) t) loop-body)))

     ((memq word '(minimize minimizing maximize maximizing))
      (let* ((what (pop args))
	     (temp (if (cl-simple-expr-p what) what (gensym)))
	     (var (cl-loop-handle-accum nil))
	     (func (intern (substring (symbol-name word) 0 3)))
	     (set (list 'setq var (list 'if var (list func var temp) temp))))
	(push (list 'progn (if (eq temp what) set
				(list 'let (list (list temp what)) set))
		       t) loop-body)))

     ((eq word 'with)
      (let ((bindings nil))
	(while (progn (push (list (pop args)
				     (and (eq (car args) '=) (cl-pop2 args)))
			       bindings)
		      (eq (car args) 'and))
	  (pop args))
	(push (nreverse bindings) loop-bindings)))

     ((eq word 'while)
      (push (pop args) loop-body))

     ((eq word 'until)
      (push (list 'not (pop args)) loop-body))

     ((eq word 'always)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (push (list 'setq loop-finish-flag (pop args)) loop-body)
      (setq loop-result t))

     ((eq word 'never)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (push (list 'setq loop-finish-flag (list 'not (pop args)))
	       loop-body)
      (setq loop-result t))

     ((eq word 'thereis)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (or loop-result-var (setq loop-result-var (gensym)))
      (push (list 'setq loop-finish-flag
		     (list 'not (list 'setq loop-result-var (pop args))))
	       loop-body))

     ((memq word '(if when unless))
      (let* ((cond (pop args))
	     (then (let ((loop-body nil))
		     (cl-parse-loop-clause)
		     (cl-loop-build-ands (nreverse loop-body))))
	     (else (let ((loop-body nil))
		     (if (eq (car args) 'else)
			 (progn (pop args) (cl-parse-loop-clause)))
		     (cl-loop-build-ands (nreverse loop-body))))
	     (simple (and (eq (car then) t) (eq (car else) t))))
	(if (eq (car args) 'end) (pop args))
	(if (eq word 'unless) (setq then (prog1 else (setq else then))))
	(let ((form (cons (if simple (cons 'progn (nth 1 then)) (nth 2 then))
			  (if simple (nth 1 else) (list (nth 2 else))))))
	  (if (cl-expr-contains form 'it)
	      (let ((temp (gensym)))
		(push (list temp) loop-bindings)
		(setq form (list* 'if (list 'setq temp cond)
				  (subst temp 'it form))))
	    (setq form (list* 'if cond form)))
	  (push (if simple (list 'progn form t) form) loop-body))))

     ((memq word '(do doing))
      (let ((body nil))
	(or (consp (car args)) (error "Syntax error on `do' clause"))
	(while (consp (car args)) (push (pop args) body))
	(push (cons 'progn (nreverse (cons t body))) loop-body)))

     ((eq word 'return)
      (or loop-finish-flag (setq loop-finish-flag (gensym)))
      (or loop-result-var (setq loop-result-var (gensym)))
      (push (list 'setq loop-result-var (pop args)
		     loop-finish-flag nil) loop-body))

     (t
      (let ((handler (and (symbolp word) (get word 'cl-loop-handler))))
	(or handler (error "Expected a loop keyword, found %s" word))
	(funcall handler))))
    (if (eq (car args) 'and)
	(progn (pop args) (cl-parse-loop-clause)))))

(defun cl-loop-let (specs body par)   ; uses loop-*
  (let ((p specs) (temps nil) (new nil))
    (while (and p (or (symbolp (car-safe (car p))) (null (cadar p))))
      (setq p (cdr p)))
    (and par p
	 (progn
	   (setq par nil p specs)
	   (while p
	     (or (cl-const-expr-p (cadar p))
		 (let ((temp (gensym)))
		   (push (list temp (cadar p)) temps)
		   (setcar (cdar p) temp)))
	     (setq p (cdr p)))))
    (while specs
      (if (and (consp (car specs)) (listp (caar specs)))
	  (let* ((spec (caar specs)) (nspecs nil)
		 (expr (cadr (pop specs)))
		 (temp (cdr (or (assq spec loop-destr-temps)
				(car (push (cons spec (or (last spec 0)
							     (gensym)))
					      loop-destr-temps))))))
	    (push (list temp expr) new)
	    (while (consp spec)
	      (push (list (pop spec)
			     (and expr (list (if spec 'pop 'car) temp)))
		       nspecs))
	    (setq specs (nconc (nreverse nspecs) specs)))
	(push (pop specs) new)))
    (if (eq body 'setq)
	(let ((set (cons (if par 'psetq 'setq) (apply 'nconc (nreverse new)))))
	  (if temps (list 'let* (nreverse temps) set) set))
      (list* (if par 'let 'let*)
	     (nconc (nreverse temps) (nreverse new)) body))))

(defun cl-loop-handle-accum (def &optional func)   ; uses args, loop-*
  (if (eq (car args) 'into)
      (let ((var (cl-pop2 args)))
	(or (memq var loop-accum-vars)
	    (progn (push (list (list var def)) loop-bindings)
		   (push var loop-accum-vars)))
	var)
    (or loop-accum-var
	(progn
	  (push (list (list (setq loop-accum-var (gensym)) def))
		   loop-bindings)
	  (setq loop-result (if func (list func loop-accum-var)
			      loop-accum-var))
	  loop-accum-var))))

(defun cl-loop-build-ands (clauses)
  (let ((ands nil)
	(body nil))
    (while clauses
      (if (and (eq (car-safe (car clauses)) 'progn)
	       (eq (car (last (car clauses))) t))
	  (if (cdr clauses)
	      (setq clauses (cons (nconc (butlast (car clauses))
					 (if (eq (car-safe (cadr clauses))
						 'progn)
					     (cdadr clauses)
					   (list (cadr clauses))))
				  (cddr clauses)))
	    (setq body (cdr (butlast (pop clauses)))))
	(push (pop clauses) ands)))
    (setq ands (or (nreverse ands) (list t)))
    (list (if (cdr ands) (cons 'and ands) (car ands))
	  body
	  (let ((full (if body
			  (append ands (list (cons 'progn (append body '(t)))))
			ands)))
	    (if (cdr full) (cons 'and full) (car full))))))


;;; Other iteration control structures.

;;;###autoload
(defmacro do (steps endtest &rest body)
  "The Common Lisp `do' loop.
Format is: (do ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (cl-expand-do-loop steps endtest body nil))

;;;###autoload
(defmacro do* (steps endtest &rest body)
  "The Common Lisp `do*' loop.
Format is: (do* ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)"
  (cl-expand-do-loop steps endtest body t))

(defun cl-expand-do-loop (steps endtest body star)
  (list 'block nil
	(list* (if star 'let* 'let)
	       (mapcar #'(lambda (c) (if (consp c) (list (car c) (nth 1 c)) c))
		       steps)
	       (list* 'while (list 'not (car endtest))
		      (append body
			      (let ((sets (mapcan
					   #'(lambda (c)
					       (and (consp c) (cdr (cdr c))
						    (list
						     (list (car c) (nth 2 c)))))
					   steps)))
				(and sets
				     (list (cons (if (or star (not (cdr sets)))
						     'setq 'psetq)
						 (apply 'append sets)))))))
	       (or (cdr endtest) '(nil)))))

;;;###autoload
(defmacro* dolist ((var list &optional result) &body body)
  "Loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
  (let ((gensym (gensym)))
    `(block nil
      (let ((,gensym ,list) ,var)
        (while ,gensym
          (setq ,var (car ,gensym))
          ,@body
          (setq ,gensym (cdr ,gensym)))
        ,@(if result `((setq ,var nil) ,result))))))

;;;###autoload
(defmacro* dotimes ((var count &optional result) &body body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers from 0, inclusive,
to COUNT, exclusive.  Then evaluate RESULT to get return value, default
nil."
  (let* ((limit (if (cl-const-expr-p count) count (gensym)))
         (bind (if (cl-const-expr-p count) nil `((,limit ,count)))))
    `(block nil
      (let ((,var 0) ,@bind)
        (while (< ,var ,limit)
          ,@body
          (setq ,var (1+ ,var)))
        ,@(if result (list result))))))

;;;###autoload
(defmacro* do-symbols ((var &optional obarray result) &rest body)
  "Loop over all interned symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY."
  `(block nil
    (mapatoms #'(lambda (,var) ,@body) ,@(and obarray (list obarray)))
    ,@(if result `((let (,var) ,result)))))

;;;###autoload
(defmacro do-all-symbols (spec &rest body)
  (list* 'do-symbols (list (car spec) nil (cadr spec)) body))


;;; Assignments.

;;;###autoload
(defmacro psetq (&rest args)
  "(psetq SYM VAL SYM VAL ...): set SYMs to the values VALs in parallel.
This is like `setq', except that all VAL forms are evaluated (in order)
before assigning any symbols SYM to the corresponding values."
  (cons 'psetf args))


;;; Binding control structures.

;;;###autoload
(defmacro progv (symbols values &rest body)
  "Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each SYMBOL in the first list is bound to the corresponding VALUE in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time."
  (list 'let '((cl-progv-save nil))
	(list 'unwind-protect
	      (list* 'progn (list 'cl-progv-before symbols values) body)
	      '(cl-progv-after))))

;;;###autoload
(defmacro* macrolet ((&rest macros) &body form &environment env)
  "Make temporary macro definitions.
This is like `flet', but for macros instead of functions."
  (cl-macroexpand-all (cons 'progn form)
                      (nconc
                       (loop
                         for (name . details)
                         in macros
                         collect
                         (list* name 'lambda (cdr (cl-transform-lambda details
                                                                       name))))
                       env)))

;;;###autoload
(defmacro* symbol-macrolet ((&rest symbol-macros) &body body &environment env)
  "Make temporary symbol macro definitions.

Elements in SYMBOL-MACROS look like (NAME EXPANSION &optional SHADOW).
Within BODY, a series of Lisp forms, a reference to NAME is replaced with its
EXPANSION, and (setq NAME ...) acts like (setf EXPANSION ...).

If NAME is encountered in a lambda argument list within BODY, then the
corresponding symbol macro will be shadowed within the lambda body, and NAME
will be treated as normal.

If NAME is encountered as a symbol within the VARLIST of a `let', `let*',
`lexical-let', or `lexical-let*' form, then the binding acts as it would with
`letf' or `letf*', depending on the specific form encountered.  This is in
contravention of Common Lisp, where such bindings shadow any enclosing symbol
macros. To specify the Common Lisp behavior for an individual symbol macro,
supply a non-nil third SHADOW element."
  (cl-macroexpand-all (cons 'progn body)
                      (nconc (loop
			       for (name expansion . shadow) in symbol-macros
			       do (check-type name symbol)
			       collect (list* (eq-hash name) expansion shadow))
			     env)))

(defvar cl-closure-vars nil)
;;;###autoload
(defmacro* lexical-let (bindings &rest body &environment env)
  "Like `let', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp."
  (let* ((cl-closure-vars cl-closure-vars)
	 (vars (mapcar #'(lambda (x)
			   (or (consp x) (setq x (list x)))
			   (push (gensym (concat "--" (symbol-name (car x))
						 "--" ))
				 cl-closure-vars)
			   (set (car cl-closure-vars) [bad-lexical-ref])
			   (list (car x) (cadr x) (car cl-closure-vars)))
		       bindings))
	 (ebody
	  (cl-macroexpand-all
	   (cons 'progn body)
	   (nconc (mapcar #'(lambda (x)
			      (list (eq-hash (car x))
				    (list 'symbol-value (caddr x))
				    t))
			  vars)
		  (list '(defun . cl-defun-expander))
		  env))))
    (if (not (get (car (last cl-closure-vars)) 'used))
	(list 'let (mapcar #'(lambda (x) (list (caddr x) (cadr x))) vars)
	      (sublis (mapcar #'(lambda (x)
				  (cons (caddr x) (list 'quote (caddr x))))
			      vars)
		      ebody))
      (list 'let (mapcar #'(lambda (x)
			     (list (caddr x)
				   (list 'make-symbol
					 (format "--%s--" (car x)))))
			 vars)
	    (apply 'append '(setf)
		   (mapcar #'(lambda (x)
			       (list (list 'symbol-value (caddr x)) (cadr x)))
			   vars))
	    ebody))))

;;;###autoload
(defmacro lexical-let* (bindings &rest body)
  "Like `let*', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp."
  (if (null bindings) (cons 'progn body)
    (setq bindings (reverse bindings))
    (while bindings
      (setq body (list (list* 'lexical-let (list (pop bindings)) body))))
    (car body)))

(defun cl-defun-expander (func &rest rest)
  (list 'progn
	(list 'defalias (list 'quote func)
	      (list 'function (cons 'lambda rest)))
	(list 'quote func)))

;;; Multiple values. We support full Common Lisp conventions here.

;;;###autoload
(defmacro multiple-value-bind (syms form &rest body)
  "Collect and bind multiple return values.

If FORM returns multiple values, each symbol in SYMS is bound to one of
them, in order, and BODY is executed.  If FORM returns fewer multiple values
than there are SYMS, remaining SYMS are bound to nil.  If FORM does
not return multiple values, it is treated as returning one multiple value.

Returns the value given by the last element of BODY."
  (if (null syms)
      `(progn ,form ,@body)
    (if (eql 1 (length syms))
        ;; Code written to deal with other "implementations" of multiple
        ;; values may have a one-element SYMS.
        `(let ((,(car syms) ,form))
          ,@body)
      (let ((temp (gensym)))
        `(let* ((,temp (multiple-value-list-internal 0 ,(length syms) ,form))
                ,@(loop 
                    for var in syms
                    collect `(,var (prog1 (car ,temp)
                                     (setq ,temp (cdr ,temp))))))
          ,@body)))))

;;;###autoload
(defmacro multiple-value-setq (syms form)
  "Collect and set multiple values.

FORM should normally return multiple values; the first N values are stored
in the symbols in SYMS in turn.  If FORM returns fewer than N values, the
remaining symbols have their values set to nil.  FORM not returning multiple
values is treated as FORM returning one multiple value, with other elements
of SYMS initialized to nil.

Returns the first of the multiple values given by FORM."
  (if (null syms)
      ;; Never return multiple values from multiple-value-setq:
      (and form `(values ,form))
    (if (eql 1 (length syms))
        `(setq ,(car syms) ,form)
      (let ((temp (gensym)))
        `(let* ((,temp (multiple-value-list-internal 0 ,(length syms) ,form)))
           (setq ,@(loop
                     for sym in syms
                     nconc `(,sym (car-safe ,temp)
                             ,temp (cdr-safe ,temp))))
           ,(car syms))))))

;;;###autoload
(defmacro multiple-value-list (form)
  "Evaluate FORM and return a list of the multiple values it returned."
  `(multiple-value-list-internal 0 multiple-values-limit ,form))

;;;###autoload
(defmacro nth-value (n form)
  "Evaluate FORM and return the Nth multiple value it returned."
  (if (integerp n)
      `(car (multiple-value-list-internal ,n ,(1+ n) ,form))
    (let ((temp (gensym)))
      `(let ((,temp ,n))
        (car (multiple-value-list-internal ,temp (1+ ,temp) ,form))))))

;;; Declarations.

;;;###autoload
(defmacro locally (&rest body) (cons 'progn body))

(defvar cl-proclaim-history t)    ; for future compilers
(defvar cl-declare-stack t)       ; for future compilers

(defun cl-do-proclaim (spec hist)
  (and hist (listp cl-proclaim-history) (push spec cl-proclaim-history))
  (cond ((eq (car-safe spec) 'special)
	 (if (boundp 'byte-compile-bound-variables)
	     (setq byte-compile-bound-variables
		   (append
		    ;; XEmacs change
		    (mapcar #'(lambda (v) (cons v byte-compile-global-bit))
			    (cdr spec))
		    byte-compile-bound-variables))))

	((eq (car-safe spec) 'inline)
         (while (setq spec (cdr spec))
           (let* ((assq (cdr (assq (car spec)
                                   byte-compile-macro-environment)))
                  (symbol (if (and (consp assq)
                                   (eq (nth 1 (nth 1 assq))
                                       'byte-compile-labels-args))
                              ;; It's a label, and we're using the labels
                              ;; implementation in bytecomp.el. Tell the
                              ;; compiler to inline it, don't mark the
                              ;; symbol to be inlined globally.
                              (nth 1 (nth 1 (nth 3 assq)))
                            (car spec))))
             (or (memq (get symbol 'byte-optimizer)
                       '(nil byte-compile-inline-expand))
                 (error
                  "%s already has a byte-optimizer, can't make it inline"
                  symbol))
             (put symbol 'byte-optimizer 'byte-compile-inline-expand))))
	((eq (car-safe spec) 'notinline)
	 (while (setq spec (cdr spec))
           (let* ((assq (cdr (assq (car spec)
                                   byte-compile-macro-environment)))
                  (symbol (if (and (consp assq)
                                   (eq (nth 1 (nth 1 assq))
                                       'byte-compile-labels-args))
                              ;; It's a label, and we're using the labels
                              ;; implementation in bytecomp.el. Tell the
                              ;; compiler not to inline it, don't mark the
                              ;; symbol to be notinline globally.
                              (nth 1 (nth 1 (nth 3 assq)))
                            (car spec))))
             (if (eq (get symbol 'byte-optimizer)
                     'byte-compile-inline-expand)
                 (put symbol 'byte-optimizer nil)))))
	((eq (car-safe spec) 'optimize)
	 (let ((speed (assq (nth 1 (assq 'speed (cdr spec)))
			    '((0 . nil) (1 . t) (2 . t) (3 . t))))
	       (safety (assq (nth 1 (assq 'safety (cdr spec)))
			     '((0 . t) (1 . t) (2 . t) (3 . nil)))))
	   (when speed
	     (setq cl-optimize-speed (car speed)
		   byte-optimize (cdr speed)))
	   (when safety
	     (setq cl-optimize-safety (car safety)
		   byte-compile-delete-errors (cdr safety)))))

	((and (eq (car-safe spec) 'warn) (boundp 'byte-compile-warnings))
	 (if (eq byte-compile-warnings t)
	     ;; XEmacs change
	     (setq byte-compile-warnings byte-compile-default-warnings))
	 (while (setq spec (cdr spec))
	   (if (consp (car spec))
	       (if (eq (cadar spec) 0)
		   (setq byte-compile-warnings
			 (delete* (caar spec) byte-compile-warnings))
		 (setq byte-compile-warnings
		       (adjoin (caar spec) byte-compile-warnings)))))))
  nil)

;;; Process any proclamations made before cl-macs was loaded.
(defvar cl-proclaims-deferred)
(let ((p (reverse cl-proclaims-deferred)))
  (while p (cl-do-proclaim (pop p) t))
  (setq cl-proclaims-deferred nil))

;;; Generalized variables.

;;;###autoload
(defmacro define-setf-method (name arglist &rest body)
  "Define a `setf' method.
This method shows how to handle `setf's to places of the form (NAME ARGLIST...).
The argument forms are bound according to ARGLIST, as if NAME were
going to be expanded as a macro, then the BODY forms are executed and must
return a list of five elements: a temporary-variables list, a value-forms
list, a store-variables list (of length one), a store-form, and an access-
form.  See `defsetf' for a simpler way to define most setf-methods."
  (append '(eval-when (compile load eval))
	  (if (stringp (car body))
	      (list (list 'put (list 'quote name) '(quote setf-documentation)
			  (pop body))))
	  (list (cl-transform-function-property
		 name 'setf-method (cons arglist body)))))
(defalias 'define-setf-expander 'define-setf-method)

;;;###autoload
(defmacro defsetf (func arg1 &rest args)
  "(defsetf NAME FUNC): define a `setf' method.
This macro is an easy-to-use substitute for `define-setf-method' that works
well for simple place forms.  In the simple `defsetf' form, `setf's of
the form (setf (NAME ARGS...) VAL) are transformed to function or macro
calls of the form (FUNC ARGS... VAL).  Example: (defsetf aref aset).
Alternate form: (defsetf NAME ARGLIST (STORE) BODY...).
Here, the above `setf' call is expanded by binding the argument forms ARGS
according to ARGLIST, binding the value form VAL to STORE, then executing
BODY, which must return a Lisp form that does the necessary `setf' operation.
Actually, ARGLIST and STORE may be bound to temporary variables which are
introduced automatically to preserve proper execution order of the arguments.
Example: (defsetf nth (n x) (v) (list 'setcar (list 'nthcdr n x) v))."
  (if (listp arg1)
      (let* ((largs nil) (largsr nil)
	     (temps nil) (tempsr nil)
	     (restarg nil) (rest-temps nil)
	     (store-var (car (prog1 (car args) (setq args (cdr args)))))
	     (store-temp (intern (format "--%s--temp--" store-var)))
	     (lets1 nil) (lets2 nil)
	     (docstr nil) (p arg1))
	(if (stringp (car args))
	    (setq docstr (prog1 (car args) (setq args (cdr args)))))
	(while (and p (not (eq (car p) '&aux)))
	  (if (eq (car p) '&rest)
	      (setq p (cdr p) restarg (car p))
	    (or (memq (car p) '(&optional &key &allow-other-keys))
		(setq largs (cons (if (consp (car p)) (car (car p)) (car p))
				  largs)
		      temps (cons (intern (format "--%s--temp--" (car largs)))
				  temps))))
	  (setq p (cdr p)))
	(setq largs (nreverse largs) temps (nreverse temps))
	(if restarg
	    (setq largsr (append largs (list restarg))
		  rest-temps (intern (format "--%s--temp--" restarg))
		  tempsr (append temps (list rest-temps)))
	  (setq largsr largs tempsr temps))
	(let ((p1 largs) (p2 temps))
	  (while p1
	    (setq lets1 (cons (list (car p2)
				    (list 'gensym (format "--%s--" (car p1))))
			      lets1)
		  lets2 (cons (list (car p1) (car p2)) lets2)
		  p1 (cdr p1) p2 (cdr p2))))
	(if restarg (setq lets2 (cons (list restarg rest-temps) lets2)))
	(append (list 'define-setf-method func arg1)
		(and docstr (list docstr))
		(list
		 (list 'let*
		       (nreverse
			(cons (list store-temp
				    (list 'gensym (format "--%s--" store-var)))
			      (if restarg
				  (append
				   (list
				    (list rest-temps
					  (list 'mapcar '(quote gensym)
						restarg)))
				   lets1)
				lets1)))
		       (list 'list  ; 'values
			     (cons (if restarg 'list* 'list) tempsr)
			     (cons (if restarg 'list* 'list) largsr)
			     (list 'list store-temp)
			     (cons 'let*
				   (cons (nreverse
					  (cons (list store-var store-temp)
						lets2))
					 args))
			     (cons (if restarg 'list* 'list)
				   (cons (list 'quote func) tempsr)))))))
    (list 'defsetf func '(&rest args) '(store)
	  (let ((call (list 'cons (list 'quote arg1)
			    '(append args (list store)))))
	    (if (car args)
		(list 'list '(quote progn) call 'store)
	      call)))))

;;; Some standard place types from Common Lisp.
(eval-when-compile (defvar ignored-arg)) ; XEmacs: warning suppression
(defsetf aref aset)
(defsetf car setcar)
(defsetf cdr setcdr)
(defsetf caar (x) (val) (list 'setcar (list 'car x) val))
(defsetf cadr (x) (val) (list 'setcar (list 'cdr x) val))
(defsetf cdar (x) (val) (list 'setcdr (list 'car x) val))
(defsetf cddr (x) (val) (list 'setcdr (list 'cdr x) val))
(defsetf elt (seq n) (store)
  (list 'if (list 'listp seq) (list 'setcar (list 'nthcdr n seq) store)
	(list 'aset seq n store)))
;; XEmacs change: ignore the optional DEFAULT arguments
(defsetf get (x y &optional ignored-arg) (store) (list 'put x y store))
(defsetf get* (x y &optional ignored-arg) (store) (list 'put x y store))
(defsetf gethash (x h &optional ignored-arg) (store) (list 'puthash x store h))
(defsetf nth (n x) (store) (list 'setcar (list 'nthcdr n x) store))
(defsetf subseq (seq start &optional end) (new)
  (list 'progn (list 'replace seq new :start1 start :end1 end) new))
(defsetf symbol-function fset)
(defsetf symbol-plist setplist)
(defsetf symbol-value set)

;;; Various car/cdr aliases.  Note that `cadr' is handled specially.
(defsetf first setcar)
(defsetf second (x) (store) (list 'setcar (list 'cdr x) store))
(defsetf third (x) (store) (list 'setcar (list 'cddr x) store))
(defsetf fourth (x) (store) (list 'setcar (list 'cdddr x) store))
(defsetf fifth (x) (store) (list 'setcar (list 'nthcdr 4 x) store))
(defsetf sixth (x) (store) (list 'setcar (list 'nthcdr 5 x) store))
(defsetf seventh (x) (store) (list 'setcar (list 'nthcdr 6 x) store))
(defsetf eighth (x) (store) (list 'setcar (list 'nthcdr 7 x) store))
(defsetf ninth (x) (store) (list 'setcar (list 'nthcdr 8 x) store))
(defsetf tenth (x) (store) (list 'setcar (list 'nthcdr 9 x) store))
(defsetf rest setcdr)

;;; Some more Emacs-related place types.
(defsetf buffer-file-name set-visited-file-name t)
;; XEmacs change: we do not need to wrap this in with-current-buffer
(defsetf buffer-modified-p set-buffer-modified-p t)
(defsetf buffer-name rename-buffer t)
(defsetf buffer-string () (store)
  (list 'progn '(erase-buffer) (list 'insert store)))
(defsetf buffer-substring cl-set-buffer-substring)
(defsetf current-buffer set-buffer)
(defsetf current-case-table set-case-table)
(defsetf current-column move-to-column t)
(defsetf current-global-map use-global-map t)
(defsetf current-input-mode () (store)
  (list 'progn (list 'apply 'set-input-mode store) store))
(defsetf current-local-map use-local-map t)
(defsetf current-window-configuration set-window-configuration t)
(defsetf default-file-modes set-default-file-modes t)
(defsetf default-value set-default)
(defsetf documentation-property put)
;;(defsetf extent-data set-extent-data)
(defsetf extent-face set-extent-face)
(defsetf extent-priority set-extent-priority)
;; XEmacs addition
(defsetf extent-property (x y &optional ignored-arg) (arg)
  (list 'set-extent-property x y arg))
(defsetf extent-end-position (ext) (store)
  `(progn (set-extent-endpoints ,ext (extent-start-position ,ext) ,store)
	  ,store))
(defsetf extent-start-position (ext) (store)
  `(progn (set-extent-endpoints ,ext ,store (extent-end-position ,ext))
	  ,store))
(defsetf face-foreground (f &optional s) (x) (list 'set-face-foreground f x s))
(defsetf face-foreback (f &optional s) (x) (list 'set-face-foreback f x s))
(defsetf face-background (f &optional s) (x) (list 'set-face-background f x s))
(defsetf face-background-pixmap (f &optional s) (x)
  (list 'set-face-background-pixmap f x s))
(defsetf face-background-placement (f &optional s) (x)
  (list 'set-face-background-placement f x s))
(defsetf face-font (f &optional s) (x) (list 'set-face-font f x s))
(defsetf face-underline-p (f &optional s) (x)
  (list 'set-face-underline-p f x s))
(defsetf face-shrink-p (f &optional s) (x)
  (list 'set-face-shrink-p f x s))
(defsetf file-modes set-file-modes t)
(defsetf frame-height (&optional f) (v)
  `(progn (set-frame-height ,f ,v) ,v))
(defsetf frame-parameters modify-frame-parameters t)
(defsetf frame-visible-p cl-set-frame-visible-p)
(defsetf frame-width (&optional f) (v)
  `(progn (set-frame-width ,f ,v) ,v))
;; XEmacs change: frame-properties instead of frame-parameters
(defsetf frame-properties (&optional f) (p)
  `(progn (set-frame-properties ,f ,p) ,p))
(defsetf frame-property (f p &optional ignored-arg) (v)
  `(progn (set-frame-property ,f ,v) ,p))
;; XEmacs addition
(defsetf current-frame-configuration set-frame-configuration)

;; XEmacs: new stuff
;; Consoles
(defsetf selected-console select-console t)
(defsetf selected-device select-device t)
(defsetf device-baud-rate (&optional d) (v)
  `(set-device-baud-rate ,d ,v))
;; This setf method is a bad idea, because set-specifier *adds* a
;; specification, rather than just setting it.  The net effect is that
;; it makes specifier-instance return VAL, but other things don't work
;; as expected -- letf, to name one.
;(defsetf specifier-instance (spec &optional dom def nof) (val)
;  `(set-specifier ,spec ,val ,dom))

(defsetf get-char-table (char table) (store)
  `(put-char-table ,char ,store ,table))

;; Annotations
(defsetf annotation-glyph set-annotation-glyph)
(defsetf annotation-down-glyph set-annotation-down-glyph)
(defsetf annotation-face set-annotation-face)
(defsetf annotation-layout set-annotation-layout)
(defsetf annotation-data set-annotation-data)
(defsetf annotation-action set-annotation-action)
(defsetf annotation-menu set-annotation-menu)
;; Widget
(defsetf widget-get widget-put t)
(defsetf widget-value widget-value-set t)

;; Misc
(defsetf recent-keys-ring-size set-recent-keys-ring-size)
(defsetf symbol-value-in-buffer (s b &optional ignored-arg) (store)
  `(with-current-buffer ,b (set ,s ,store)))
(defsetf symbol-value-in-console (s c &optional ignored-arg) (store)
  `(letf (((selected-console) ,c))
     (set ,s ,store)))

(defsetf buffer-dedicated-frame (&optional b) (v)
  `(set-buffer-dedicated-frame ,b ,v))
(defsetf console-type-image-conversion-list
  set-console-type-image-conversion-list)
(defsetf default-toolbar-position set-default-toolbar-position)
(defsetf device-class (&optional d) (v)
  `(set-device-class ,d ,v))
(defsetf extent-begin-glyph set-extent-begin-glyph)
(defsetf extent-begin-glyph-layout set-extent-begin-glyph-layout)
(defsetf extent-end-glyph set-extent-end-glyph)
(defsetf extent-end-glyph-layout set-extent-end-glyph-layout)
(defsetf extent-keymap set-extent-keymap)
(defsetf extent-parent set-extent-parent)
(defsetf extent-properties set-extent-properties)
;; Avoid adding various face and glyph functions.
(defsetf frame-selected-window (&optional f) (v)
  `(set-frame-selected-window ,f ,v))
(defsetf glyph-image (glyph &optional domain) (i)
  (list 'set-glyph-image glyph i domain))
(defsetf itimer-function set-itimer-function)
(defsetf itimer-function-arguments set-itimer-function-arguments)
(defsetf itimer-is-idle set-itimer-is-idle)
(defsetf itimer-recorded-run-time set-itimer-recorded-run-time)
(defsetf itimer-restart set-itimer-restart)
(defsetf itimer-uses-arguments set-itimer-uses-arguments)
(defsetf itimer-value set-itimer-value)
(defsetf keymap-parents set-keymap-parents)
(defsetf marker-insertion-type set-marker-insertion-type)
(defsetf mouse-pixel-position (&optional d) (v)
  `(progn
     (set-mouse-pixel-position ,d ,(car v) ,(car (cdr v)) ,(cdr (cdr v)))
     ,v))
(defsetf trunc-stack-length set-trunc-stack-length)
(defsetf trunc-stack-stack set-trunc-stack-stack)
(defsetf undoable-stack-max set-undoable-stack-max)
(defsetf weak-list-list set-weak-list-list)
;; End of new XEmacs stuff

(defsetf getenv setenv t)
(defsetf get-register set-register)
(defsetf global-key-binding global-set-key)
(defsetf keymap-parent set-keymap-parent)
;; XEmacs addition: more keymap-related setf forms
(defsetf keymap-name set-keymap-name)
(defsetf keymap-prompt set-keymap-prompt)
(defsetf keymap-default-binding set-keymap-default-binding)
(defsetf local-key-binding local-set-key)
(defsetf mark set-mark t)
(defsetf mark-marker set-mark t)
(defsetf marker-position set-marker t)
(defsetf match-data store-match-data t)
(defsetf mouse-position (scr) (store)
  (list 'set-mouse-position scr (list 'car store) (list 'cadr store)
	(list 'cddr store)))
(defsetf overlay-get overlay-put)
(defsetf overlay-start (ov) (store)
  (list 'progn (list 'move-overlay ov store (list 'overlay-end ov)) store))
(defsetf overlay-end (ov) (store)
  (list 'progn (list 'move-overlay ov (list 'overlay-start ov) store) store))
(defsetf point goto-char)
(defsetf point-marker goto-char t)
(defsetf point-max () (store)
  (list 'progn (list 'narrow-to-region '(point-min) store) store))
(defsetf point-min () (store)
  (list 'progn (list 'narrow-to-region store '(point-max)) store))
(defsetf process-buffer set-process-buffer)
(defsetf process-filter set-process-filter)
(defsetf process-sentinel set-process-sentinel)
;;(defsetf process-get process-put)
(defsetf read-mouse-position (scr) (store)
  (list 'set-mouse-position scr (list 'car store) (list 'cdr store)))
;;(defsetf screen-height set-screen-height t)
;;(defsetf screen-width set-screen-width t)
(defsetf selected-window select-window)
;;(defsetf selected-screen select-screen)
(defsetf selected-frame select-frame)
(defsetf standard-case-table set-standard-case-table)
(defsetf syntax-table set-syntax-table)
(defsetf visited-file-modtime set-visited-file-modtime t)
(defsetf window-buffer set-window-buffer t)
(defsetf window-display-table set-window-display-table t)
(defsetf window-dedicated-p set-window-dedicated-p t)
(defsetf window-height (&optional window) (store)
  `(progn (enlarge-window (- ,store (window-height)) nil ,window) ,store))
(defsetf window-hscroll set-window-hscroll)
(defsetf window-point set-window-point)
(defsetf window-start set-window-start)
(defsetf window-width (&optional window) (store)
  `(progn (enlarge-window (- ,store (window-width)) t ,window) ,store))
(defsetf x-get-cutbuffer x-store-cutbuffer t)
(defsetf x-get-cut-buffer x-store-cut-buffer t)   ; groan.
(defsetf x-get-secondary-selection x-own-secondary-selection t)
(defsetf x-get-selection x-own-selection t)
(defsetf get-selection own-selection t)

;;; More complex setf-methods.
;;; These should take &environment arguments, but since full arglists aren't
;;; available while compiling cl-macs, we fake it by referring to the global
;;; variable byte-compile-macro-environment directly.

(define-setf-method apply (func arg1 &rest rest)
  (or (and (memq (car-safe func) '(quote function function*))
	   (symbolp (car-safe (cdr-safe func))))
      (error "First arg to apply in setf is not (function SYM): %s" func))
  (let* ((form (cons (nth 1 func) (cons arg1 rest)))
	 (method (get-setf-method form byte-compile-macro-environment)))
    (list (car method) (nth 1 method) (nth 2 method)
	  (cl-setf-make-apply (nth 3 method) (cadr func) (car method))
	  (cl-setf-make-apply (nth 4 method) (cadr func) (car method)))))

(defun cl-setf-make-apply (form func temps)
  (if (eq (car form) 'progn)
      (list* 'progn (cl-setf-make-apply (cadr form) func temps) (cddr form))
    (or (equal (last form) (last temps))
	(error "%s is not suitable for use with setf-of-apply" func))
    (list* 'apply (list 'quote (car form)) (cdr form))))

(define-setf-method nthcdr (n place)
  (let ((method (get-setf-method place byte-compile-macro-environment))
	(n-temp (gensym "--nthcdr-n--"))
	(store-temp (gensym "--nthcdr-store--")))
    (list (cons n-temp (car method))
	  (cons n (nth 1 method))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'cl-set-nthcdr n-temp (nth 4 method)
				       store-temp)))
		(nth 3 method) store-temp)
	  (list 'nthcdr n-temp (nth 4 method)))))

(define-setf-method getf (place tag &optional def)
  (let ((method (get-setf-method place byte-compile-macro-environment))
	(tag-temp (gensym "--getf-tag--"))
	(def-temp (gensym "--getf-def--"))
	(store-temp (gensym "--getf-store--")))
    (list (append (car method) (list tag-temp def-temp))
	  (append (nth 1 method) (list tag def))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'plist-put (nth 4 method)
				       tag-temp store-temp)))
		(nth 3 method) store-temp)
	  (list 'getf (nth 4 method) tag-temp def-temp))))

(define-setf-method substring (place from &optional to)
  (let ((method (get-setf-method place byte-compile-macro-environment))
	(from-temp (gensym "--substring-from--"))
	(to-temp (gensym "--substring-to--"))
	(store-temp (gensym "--substring-store--")))
    (list (append (car method) (list from-temp to-temp))
	  (append (nth 1 method) (list from to))
	  (list store-temp)
	  (list 'let (list (list (car (nth 2 method))
				 (list 'cl-set-substring (nth 4 method)
				       from-temp to-temp store-temp)))
		(nth 3 method) store-temp)
	  (list 'substring (nth 4 method) from-temp to-temp))))

;; XEmacs addition
(define-setf-method values (&rest args)
  (let ((methods (mapcar #'(lambda (x)
			     (get-setf-method x byte-compile-macro-environment))
			 args))
	(store-temp (gensym "--values-store--")))
    (list (apply 'append (mapcar 'first methods))
	  (apply 'append (mapcar 'second methods))
	  `((,store-temp
	     (multiple-value-list-internal 0 ,(if args (length args) 1))))
	  (cons 'values
		(mapcar #'(lambda (m)
			    (cl-setf-do-store (cons (car (third m)) (fourth m))
					      (list 'pop store-temp)))
			methods))
	  (cons 'list (mapcar 'fifth methods)))))

;;; Getting and optimizing setf-methods.
;;;###autoload
(defun get-setf-method (place &optional env)
  "Return a list of five values describing the setf-method for PLACE.
PLACE may be any Lisp form which can appear as the PLACE argument to
a macro like `setf' or `incf'."
  (if (symbolp place)
      (let ((temp (gensym "--setf--")))
	(list nil nil (list temp) (list 'setq place temp) place))
    (or (and (symbolp (car place))
	     (let* ((func (car place))
		    (name (symbol-name func))
		    (method (get func 'setf-method))
		    (case-fold-search nil))
	       (or (and method
			(let ((byte-compile-macro-environment env))
			  (setq method (apply method (cdr place))))
			(if (and (consp method) (eql (length method) 5))
			    method
			  (error "Setf-method for %s returns malformed method"
				 func)))
		   (and (save-match-data
			  (string-match "\\`c[ad][ad][ad]?[ad]?r\\'" name))
			(get-setf-method (compiler-macroexpand place)))
		   (and (eq func 'edebug-after)
			(get-setf-method (nth (1- (length place)) place)
					 env)))))
	(if (eq place (setq place (macroexpand place env)))
	    (if (and (symbolp (car place)) (fboundp (car place))
		     (symbolp (symbol-function (car place))))
		(get-setf-method (cons (symbol-function (car place))
				       (cdr place)) env)
	      (error "No setf-method known for %s" (car place)))
	  (get-setf-method place env)))))

(defun cl-setf-do-modify (place opt-expr)
  (let* ((method (get-setf-method place byte-compile-macro-environment))
	 (temps (car method)) (values (nth 1 method))
	 (lets nil) (subs nil)
	 (optimize (and (not (eq opt-expr 'no-opt))
			(or (and (not (eq opt-expr 'unsafe))
				 (cl-safe-expr-p opt-expr))
			    (cl-setf-simple-store-p (car (nth 2 method))
						    (nth 3 method)))))
	 (simple (and optimize (consp place) (cl-simple-exprs-p (cdr place)))))
    (while values
      (if (or simple (cl-const-expr-p (car values)))
	  (push (cons (pop temps) (pop values)) subs)
	(push (list (pop temps) (pop values)) lets)))
    (list (nreverse lets)
	  (cons (car (nth 2 method)) (sublis subs (nth 3 method)))
	  (sublis subs (nth 4 method)))))

(defun cl-setf-do-store (spec val)
  (let ((sym (car spec))
	(form (cdr spec)))
    (if (consp sym)
	;; XEmacs change, only used for implementing #'values at the moment.
	(let* ((orig (copy-list sym))
	       (intermediate (last orig))
	       (circular-limit 32))
	  (while (consp (car intermediate))
	    (when (zerop circular-limit)
	      (error 'circular-list "Form seems to contain loops"))
	    (setq intermediate (last (car intermediate))
		  circular-limit (1- circular-limit)))
	  (setcdr intermediate (list val))
	  `(let (,orig)
	    ,form))
      (if (or (cl-const-expr-p val)
	      (and (cl-simple-expr-p val)
		   (eq (cl-expr-contains form sym) 1))
	      (cl-setf-simple-store-p sym form))
	  (subst val sym form)
	(list 'let (list (list sym val)) form)))))

(defun cl-setf-simple-store-p (sym form)
  (and (consp form) (eq (cl-expr-contains form sym) 1)
       (eq (nth (1- (length form)) form) sym)
       (symbolp (car form)) (fboundp (car form))
       (not (eq (car-safe (symbol-function (car form))) 'macro))))

;;; The standard modify macros.
;;;###autoload
(defmacro setf (&rest args)
  "(setf PLACE VAL PLACE VAL ...): set each PLACE to the value of its VAL.
This is a generalized version of `setq'; the PLACEs may be symbolic
references such as (car x) or (aref x i), as well as plain symbols.
For example, (setf (cadar x) y) is equivalent to (setcar (cdar x) y).
The return value is the last VAL in the list."
  (if (cdr (cdr args))
      (let ((sets nil))
	(while args (push (list 'setf (pop args) (pop args)) sets))
	(cons 'progn (nreverse sets)))
    (if (symbolp (car args))
	(and args (cons 'setq args))
      (let* ((method (cl-setf-do-modify (car args) (nth 1 args)))
	     (store (cl-setf-do-store (nth 1 method) (nth 1 args))))
	(if (car method) (list 'let* (car method) store) store)))))

;;;###autoload
(defmacro psetf (&rest args)
  "(psetf PLACE VAL PLACE VAL ...): set PLACEs to the values VALs in parallel.
This is like `setf', except that all VAL forms are evaluated (in order)
before assigning any PLACEs to the corresponding values."
  (let ((p args) (simple t) (vars nil))
    (while p
      (if (or (not (symbolp (car p))) (cl-expr-depends-p (nth 1 p) vars))
	  (setq simple nil))
      (if (memq (car p) vars)
	  (error "Destination duplicated in psetf: %s" (car p)))
      (push (pop p) vars)
      (or p (error "Odd number of arguments to psetf"))
      (pop p))
    (if simple
	(list 'progn (cons 'setf args) nil)
      (setq args (reverse args))
      (let ((expr (list 'setf (cadr args) (car args))))
	(while (setq args (cddr args))
	  (setq expr (list 'setf (cadr args) (list 'prog1 (car args) expr))))
	(list 'progn expr nil)))))

;;;###autoload
(defun cl-do-pop (place)
  (if (cl-simple-expr-p place)
      (list 'prog1 (list 'car-safe place) (list 'setf place (list 'cdr place)))
    (let* ((method (cl-setf-do-modify place t))
	   (temp (gensym "--pop--")))
      (list 'let*
	    (append (car method)
		    (list (list temp (nth 2 method))))
	    (list 'prog1
		  (list 'car-safe temp)
		  (cl-setf-do-store (nth 1 method) (list 'cdr temp)))))))

;;;###autoload
(defmacro remf (place tag)
  "Remove TAG from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The form returns true if TAG was found and removed, nil otherwise."
  (let* ((method (cl-setf-do-modify place t))
	 (tag-temp (and (not (cl-const-expr-p tag)) (gensym "--remf-tag--")))
	 (val-temp (and (not (cl-simple-expr-p place))
			(gensym "--remf-place--")))
	 (ttag (or tag-temp tag))
	 (tval (or val-temp (nth 2 method))))
    (list 'let*
	  (append (car method)
		  (and val-temp (list (list val-temp (nth 2 method))))
		  (and tag-temp (list (list tag-temp tag))))
	  (list 'if (list 'eq ttag (list 'car tval))
		(list 'progn
		      (cl-setf-do-store (nth 1 method) (list 'cddr tval))
		      t)
		(list 'plist-remprop tval ttag)))))

;;;###autoload
(defmacro shiftf (place &rest args)
  "(shiftf PLACE PLACE... VAL): shift left among PLACEs.
Example: (shiftf A B C) sets A to B, B to C, and returns the old A.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'."
  ;; XEmacs change: use iteration instead of recursion
  (if (every #'symbolp (butlast (cons place args)))
      (list* 'prog1 place
	     (let ((sets nil))
	       (while args
		 (push (list 'setq place (car args)) sets)
		 (setq place (pop args)))
	       (nreverse sets)))
    (let* ((places (reverse (cons place args)))
	   (form (pop places)))
      (while places
	(let ((method (cl-setf-do-modify (pop places) 'unsafe)))
	  (setq form (list 'let* (car method)
			   (list 'prog1 (nth 2 method)
				 (cl-setf-do-store (nth 1 method) form))))))
      form)))

;;;###autoload
(defmacro rotatef (&rest places)
  "Rotate left among PLACES.
Example: (rotatef A B C) sets A to B, B to C, and C to A.  It returns nil.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (every #'symbolp places)
      (and (cdr places)
	   (let ((sets nil)
		 (first (car places)))
	     (while (cdr places)
	       (setq sets (nconc sets (list (pop places) (car places)))))
	     (nconc (list 'psetf) sets (list (car places) first))))
    (let* ((places (reverse places))
	   (temp (gensym "--rotatef--"))
	   (form temp))
      (while (cdr places)
	(let ((method (cl-setf-do-modify (pop places) 'unsafe)))
	  (setq form (list 'let* (car method)
			   (list 'prog1 (nth 2 method)
				 (cl-setf-do-store (nth 1 method) form))))))
      (let ((method (cl-setf-do-modify (car places) 'unsafe)))
	(list 'let* (append (car method) (list (list temp (nth 2 method))))
	      (cl-setf-do-store (nth 1 method) form) nil)))))

;; This function is not in Common Lisp, and there are gaps in its structure and
;; implementation that reflect that it was never well-specified. E.g. with
;; setf, the question of whether a PLACE is bound or not and how to make it
;; unbound doesn't arise, but we need some way of specifying that for letf to
;; be sensible for gethash, symbol-value and so on; currently we just hard-code
;; symbol-value, symbol-function and values (the latter is XEmacs work that
;; I've just done) in the body of this function, and the following gives the
;; wrong behaviour for gethash:
;; 
;; (setq my-hash-table #s(hash-table :test equal :data ())
;;       print-gensym t)
;; => t
;; (gethash "my-key" my-hash-table (gensym))
;; => #:G68010
;; (letf (((gethash "my-key" my-hash-table) 4000))
;;   (message "key value is %S" (gethash "my-key" my-hash-table)))
;; => "key value is 4000"
;; (gethash "my-key" my-hash-table (gensym))
;; => nil ;; should be an uninterned symbol.
;;
;; Aidan Kehoe, Fr Nov 13 16:12:21 GMT 2009

;;;###autoload
(defmacro letf (bindings &rest body)
  "Temporarily bind to PLACEs.
This is the analogue of `let', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY.

arguments: (((PLACE VALUE) &rest BINDINGS) &body BODY)"
  (if (and (not (cdr bindings)) (cdar bindings) (symbolp (caar bindings)))
      (list* 'let bindings body)
    (let ((lets nil)
	  (rev (reverse bindings)))
      (while rev
	(let* ((place (if (symbolp (caar rev))
			  (list 'symbol-value (list 'quote (caar rev)))
			(caar rev)))
	       (value (cadar rev))
	       (method (cl-setf-do-modify place 'no-opt))
	       (save (gensym "--letf-save--"))
	       (bound (and (memq (car place)
                                 '(symbol-value symbol-function values))
			   (gensym "--letf-bound--")))
	       (temp (and (not (cl-const-expr-p value)) (cdr bindings)
			  (gensym "--letf-val--")))
               (syms (and (eq 'values (car place))
                          (gensym "--letf-syms--")))
               (cursor (and syms (gensym "--letf-cursor--"))))
	  (setq lets (nconc (car method)
                            (cond
                             (syms
                              `((,syms ',(loop
                                           for sym in (cdr place)
                                           nconc (if (symbolp sym) (list sym))))
                                (,cursor ,syms)
                                (,bound nil)
                                (,save
                                 (prog2
                                     (while (consp ,cursor)
                                       (setq ,bound
                                             (cons (and (boundp (car ,cursor))
                                                        (symbol-value
                                                         (car ,cursor)))
                                                   ,bound)
                                             ,cursor (cdr ,cursor)))
                                     ;; Just using ,bound as a temporary
                                     ;; variable here, to initialise ,save:
                                     (nreverse ,bound) 
                                   ;; Now, really initialise ,bound:
                                   (setq ,cursor ,syms
                                         ,bound nil
                                         ,bound 
                                         (progn (while (consp ,cursor)
                                                  (setq ,bound
                                                        (cons
                                                         (boundp (car ,cursor))
                                                         ,bound)
                                                        ,cursor (cdr ,cursor)))
                                                (nreverse ,bound)))))))
                              (bound
                               (list (list bound
                                           (list (if (eq (car place)
                                                         'symbol-value)
                                                     'boundp 'fboundp)
                                                 (nth 1 (nth 2 method))))
                                     (list save (list 'and bound
                                                      (nth 2 method)))))
                               (t
                                (list (list save (nth 2 method)))))
			    (and temp (list (list temp value)))
			    lets)
		body (list
		      (list 'unwind-protect
			    (cons 'progn
				  (if (cdr (car rev))
				      (cons (cl-setf-do-store (nth 1 method)
							      (or temp value))
					    body)
				    body))
                            (cond 
                             (syms
                              `(while (consp ,syms)
                                (if (car ,bound)
                                    (set (car ,syms) (car ,save))
                                  (makunbound (car ,syms)))
                                (setq ,syms (cdr ,syms)
                                      ,bound (cdr ,bound)
                                      ,save (cdr ,save))))
                             (bound
                              (list 'if bound
                                    (cl-setf-do-store (nth 1 method) save)
                                    (list (if (eq (car place)
                                                  'symbol-function)
                                              'fmakunbound
                                            'makunbound)
                                          (nth 1 (nth 2 method)))))
                             (t
			      (cl-setf-do-store (nth 1 method) save)))))
		rev (cdr rev))))
      (list* 'let* lets body))))

;;;###autoload
(defmacro letf* (bindings &rest body)
  "Temporarily bind to PLACES.
This is the analogue of `let*', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY.

arguments: (((PLACE VALUE) &rest BINDINGS) &body BODY)"
  (if (null bindings)
      (cons 'progn body)
    (setq bindings (reverse bindings))
    (while bindings
      (setq body (list (list* 'letf (list (pop bindings)) body))))
    (car body)))

;;;###autoload
(defmacro callf (func place &rest args)
  "Set PLACE to (FUNC PLACE ARGS...).
FUNC should be an unquoted function name.  PLACE may be a symbol,
or any generalized variable allowed by `setf'."
  (let* ((method (cl-setf-do-modify place (cons 'list args)))
	 (rargs (cons (nth 2 method) args)))
    (list 'let* (car method)
	  (cl-setf-do-store (nth 1 method)
			    (if (symbolp func) (cons func rargs)
			      (list* 'funcall (list 'function func)
				     rargs))))))

;;;###autoload
(defmacro callf2 (func arg1 place &rest args)
  "Set PLACE to (FUNC ARG1 PLACE ARGS...).
Like `callf', but PLACE is the second argument of FUNC, not the first."
  (if (and (cl-safe-expr-p arg1) (cl-simple-expr-p place) (symbolp func))
      (list 'setf place (list* func arg1 place args))
    (let* ((method (cl-setf-do-modify place (cons 'list args)))
	   (temp (and (not (cl-const-expr-p arg1)) (gensym "--arg1--")))
	   (rargs (list* (or temp arg1) (nth 2 method) args)))
      (list 'let* (append (and temp (list (list temp arg1))) (car method))
	    (cl-setf-do-store (nth 1 method)
			      (if (symbolp func) (cons func rargs)
				(list* 'funcall (list 'function func)
				       rargs)))))))

;;;###autoload
(defmacro define-modify-macro (name arglist func &optional doc)
  "Define a `setf'-like modify macro.
If NAME is called, it combines its PLACE argument with the other arguments
from ARGLIST using FUNC: (define-modify-macro incf (&optional (n 1)) +)"
  (if (memq '&key arglist) (error "&key not allowed in define-modify-macro"))
  (let ((place (gensym "--place--")))
    (list 'defmacro* name (cons place arglist) doc
	  (list* (if (memq '&rest arglist) 'list* 'list)
		 '(quote callf) (list 'quote func) place
		 (cl-arglist-args arglist)))))


;;; Structures.

;;;###autoload
(defmacro defstruct (struct &rest descs)
  "(defstruct (NAME OPTIONS...) (SLOT SLOT-OPTS...)...): define a struct type.
This macro defines a new Lisp data type called NAME, which contains data
stored in SLOTs.  This defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and setf-able `NAME-SLOT' accessors."
  (let* ((name (if (consp struct) (car struct) struct))
	 (opts (cdr-safe struct))
	 (slots nil)
	 (defaults nil)
	 (conc-name (concat (symbol-name name) "-"))
	 (constructor (intern (format "make-%s" name)))
	 (constrs nil)
	 (copier (intern (format "copy-%s" name)))
	 (predicate (intern (format "%s-p" name)))
	 (print-func nil) (print-auto nil)
	 (safety (if (cl-compiling-file) cl-optimize-safety 3))
	 (include nil)
	 (tag (intern (format "cl-struct-%s" name)))
	 (tag-symbol (intern (format "cl-struct-%s-tags" name)))
	 (include-descs nil)
	 (side-eff nil)
	 (type nil)
	 (named nil)
	 (forms nil)
	 pred-form pred-check)
    (if (stringp (car descs))
	(push (list 'put (list 'quote name) '(quote structure-documentation)
		       (pop descs)) forms))
    (setq descs (cons '(cl-tag-slot)
		      (mapcar #'(lambda (x) (if (consp x) x (list x)))
			      descs)))
    (while opts
      (let ((opt (if (consp (car opts)) (caar opts) (car opts)))
	    (args (cdr-safe (pop opts))))
	(cond ((eq opt :conc-name)
	       (if args
		   (setq conc-name (if (car args)
				       (symbol-name (car args)) ""))))
	      ((eq opt :constructor)
	       (if (cdr args)
		   (push args constrs)
		 (if args (setq constructor (car args)))))
	      ((eq opt :copier)
	       (if args (setq copier (car args))))
	      ((eq opt :predicate)
	       (if args (setq predicate (car args))))
	      ((eq opt :include)
	       (setq include (car args)
		     include-descs (mapcar #'(lambda (x)
					       (if (consp x) x (list x)))
					   (cdr args))))
	      ((eq opt :print-function)
	       (setq print-func (car args)))
	      ((eq opt :type)
	       (setq type (car args)))
	      ((eq opt :named)
	       (setq named t))
	      ((eq opt :initial-offset)
	       (setq descs (nconc (make-list (car args) '(cl-skip-slot))
				  descs)))
	      (t
	       (error "Slot option %s unrecognized" opt)))))
    (if print-func
	(setq print-func (list 'progn
			       (list 'funcall (list 'function print-func)
				     'cl-x 'cl-s 'cl-n) t))
      (or type (and include (not (get include 'cl-struct-print)))
	  (setq print-auto t
		print-func (and (or (not (or include type)) (null print-func))
				(list 'progn
				      (list 'princ (format "#S(%s" name)
					    'cl-s))))))
    (if include
	(let ((inc-type (get include 'cl-struct-type))
	      (old-descs (get include 'cl-struct-slots)))
	  (or inc-type (error "%s is not a struct name" include))
	  (and type (not (eq (car inc-type) type))
	       (error ":type disagrees with :include for %s" name))
	  (while include-descs
	    (setcar (memq (or (assq (caar include-descs) old-descs)
			      (error "No slot %s in included struct %s"
				     (caar include-descs) include))
			  old-descs)
		    (pop include-descs)))
	  (setq descs (append old-descs (delete* (assq 'cl-tag-slot descs) descs))
		type (car inc-type)
		named (assq 'cl-tag-slot descs))
	  (if (cadr inc-type) (setq tag name named t))
	  (let ((incl include))
	    (while incl
	      (push (list 'pushnew (list 'quote tag)
			     (intern (format "cl-struct-%s-tags" incl)))
		       forms)
	      (setq incl (get incl 'cl-struct-include)))))
      (if type
	  (progn
	    (or (memq type '(vector list))
		(error "Illegal :type specifier: %s" type))
	    (if named (setq tag name)))
	(setq type 'vector named 'true)))
    (or named (setq descs (delete* (assq 'cl-tag-slot descs) descs)))
    (push (list 'defvar tag-symbol) forms)
    (setq pred-form (and named
			 (let ((pos (- (length descs)
				       (length (memq (assq 'cl-tag-slot descs)
						     descs)))))
			   (if (eq type 'vector)
			       (list 'and '(vectorp cl-x)
				     (list '>= '(length cl-x) (length descs))
				     (list 'memq (list 'aref 'cl-x pos)
					   tag-symbol))
			     (if (= pos 0)
				 (list 'memq '(car-safe cl-x) tag-symbol)
			       (list 'and '(consp cl-x)
				     (list 'memq (list 'nth pos 'cl-x)
					   tag-symbol))))))
	  pred-check (and pred-form (> safety 0)
			  (if (and (eq (caadr pred-form) 'vectorp)
				   (= safety 1))
			      (cons 'and (cdddr pred-form)) pred-form)))
    (let ((pos 0) (descp descs))
      (while descp
	(let* ((desc (pop descp))
	       (slot (car desc)))
	  (if (memq slot '(cl-tag-slot cl-skip-slot))
	      (progn
		(push nil slots)
		(push (and (eq slot 'cl-tag-slot) (list 'quote tag))
			 defaults))
	    (if (assq slot descp)
		(error "Duplicate slots named %s in %s" slot name))
	    (let ((accessor (intern (format "%s%s" conc-name slot))))
	      (push slot slots)
	      (push (nth 1 desc) defaults)
	      (push (list*
			'defsubst* accessor '(cl-x)
			(append
			 (and pred-check
			      (list (list 'or pred-check
					  (list 'error
						(format "%s accessing a non-%s"
							accessor name)
						'cl-x))))
			 (list (if (eq type 'vector) (list 'aref 'cl-x pos)
				 (if (= pos 0) '(car cl-x)
				   (list 'nth pos 'cl-x)))))) forms)
	      (push (cons accessor t) side-eff)
	      (push (list 'define-setf-method accessor '(cl-x)
			     (if (cadr (memq :read-only (cddr desc)))
				 (list 'error (format "%s is a read-only slot"
						      accessor))
			       (list 'cl-struct-setf-expander 'cl-x
				     (list 'quote name) (list 'quote accessor)
				     (and pred-check (list 'quote pred-check))
				     pos)))
		       forms)
	      (if print-auto
		  (nconc print-func
			 (list (list 'princ (format " %s" slot) 'cl-s)
			       (list 'prin1 (list accessor 'cl-x) 'cl-s)))))))
	(setq pos (1+ pos))))
    (setq slots (nreverse slots)
	  defaults (nreverse defaults))
    (and predicate pred-form
	 (progn (push (list 'defsubst* predicate '(cl-x)
			       (if (eq (car pred-form) 'and)
				   (append pred-form '(t))
				 (list 'and pred-form t))) forms)
		(push (cons predicate 'error-free) side-eff)))
    (and copier
	 (progn (push (list 'defun copier '(x) '(copy-sequence x)) forms)
		(push (cons copier t) side-eff)))
    (if constructor
	(push (list constructor
                    (cons '&key (remove* nil slots)))
              constrs))
    (while constrs
      (let* ((name (caar constrs))
	     (args (cadr (pop constrs)))
	     (anames (cl-arglist-args args))
	     (make (mapcar* #'(lambda (s d) (if (memq s anames) s d))
			    slots defaults)))
	(push (list 'defsubst* name
		       (list* '&cl-defs (list 'quote (cons nil descs)) args)
		       (cons type make)) forms)
	(if (cl-safe-expr-p (cons 'progn (mapcar 'second descs)))
	    (push (cons name t) side-eff))))
    (if print-auto (nconc print-func (list '(princ ")" cl-s) t)))
    (if print-func
	(push (list 'push
		       (list 'function
			     (list 'lambda '(cl-x cl-s cl-n)
				   (list 'and pred-form print-func)))
		       'custom-print-functions) forms))
    (push (list 'setq tag-symbol (list 'list (list 'quote tag))) forms)
    (push (list* 'eval-when '(compile load eval)
		    (list 'put (list 'quote name) '(quote cl-struct-slots)
			  (list 'quote descs))
		    (list 'put (list 'quote name) '(quote cl-struct-type)
			  (list 'quote (list type (eq named t))))
		    (list 'put (list 'quote name) '(quote cl-struct-include)
			  (list 'quote include))
		    (list 'put (list 'quote name) '(quote cl-struct-print)
			  print-auto)
		    (mapcar #'(lambda (x)
				(list 'put (list 'quote (car x))
				      '(quote side-effect-free)
				      (list 'quote (cdr x))))
			    side-eff))
	     forms)
    (cons 'progn (nreverse (cons (list 'quote name) forms)))))

;;;###autoload
(defun cl-struct-setf-expander (x name accessor pred-form pos)
  (let* ((temp (gensym "--x--")) (store (gensym "--store--")))
    (list (list temp) (list x) (list store)
	  (append '(progn)
		  (and pred-form
		       (list (list 'or (subst temp 'cl-x pred-form)
				   (list 'error
					 (format
					  "%s storing a non-%s" accessor name)
					 temp))))
		  (list (if (eq (car (get name 'cl-struct-type)) 'vector)
			    (list 'aset temp pos store)
			  (list 'setcar
				(if (<= pos 5)
				    (let ((xx temp))
				      (while (>= (setq pos (1- pos)) 0)
					(setq xx (list 'cdr xx)))
				      xx)
				  (list 'nthcdr pos temp))
				store))))
	  (list accessor temp))))


;;; Types and assertions.

;;;###autoload
(defmacro deftype (name arglist &rest body)
  "Define NAME as a new data type.
The type name can then be used in `typecase', `check-type', etc."
  (list 'eval-when '(compile load eval)
	(cl-transform-function-property
	 name 'cl-deftype-handler (cons (list* '&cl-defs ''('*) arglist) body))))

(defun cl-make-type-test (val type)
  (if (symbolp type)
      (cond ((get type 'cl-deftype-handler)
	     (cl-make-type-test val (funcall (get type 'cl-deftype-handler))))
	    ((memq type '(nil t)) type)
	    ((eq type 'null) `(null ,val))
	    ((eq type 'float) `(floatp ,val))
	    ((eq type 'real) `(numberp ,val))
	    ((eq type 'fixnum) `(fixnump ,val))
	    ;; XEmacs change: we do not have char-valid-p
	    ((memq type '(character string-char)) `(characterp ,val))
	    (t
	     (let* ((name (symbol-name type))
		    (namep (intern (concat name "p"))))
	       (if (fboundp namep) (list namep val)
		 (list (intern (concat name "-p")) val)))))
    (cond ((get (car type) 'cl-deftype-handler)
	   (cl-make-type-test val (apply (get (car type) 'cl-deftype-handler)
					 (cdr type))))
	  ((memq (car-safe type) '(integer float real number))
	   (delete* t (list 'and (cl-make-type-test val (car type))
			 (if (memq (cadr type) '(* nil)) t
			   (if (consp (cadr type)) (list '> val (caadr type))
			     (list '>= val (cadr type))))
			 (if (memq (caddr type) '(* nil)) t
			   (if (consp (caddr type)) (list '< val (caaddr type))
			     (list '<= val (caddr type)))))))
	  ((memq (car-safe type) '(and or not))
	   (cons (car type)
		 (mapcar #'(lambda (x) (cl-make-type-test val x))
			 (cdr type))))
	  ((memq (car-safe type) '(member member*))
	   (list 'and (list 'member* val (list 'quote (cdr type))) t))
	  ((eq (car-safe type) 'eql)
	   (list 'eql (cadr type) val))
	  ((eq (car-safe type) 'satisfies) (list (cadr type) val))
	  (t (error "Bad type spec: %s" type)))))

;;;###autoload
(defun typep (object type)   ; See compiler macro below.
  "Check that OBJECT is of type TYPE.
TYPE is a Common Lisp-style type specifier."
  (eval (cl-make-type-test 'object type)))

;;;###autoload
(defmacro check-type (place type &optional string)
  "Verify that PLACE is of type TYPE; signal a continuable error if not.
STRING is an optional description of the desired type."
  (and (or (not (cl-compiling-file))
	   (< cl-optimize-speed 3) (= cl-optimize-safety 3))
       (let* ((temp (if (cl-simple-expr-p place 3) place (gensym)))
	      (test (cl-make-type-test temp type))
	      (signal-error `(signal 'wrong-type-argument
			      ,(list 'list (or string (list 'quote type))
				     temp (list 'quote place))))
	      (body
	       (condition-case nil
		   `(while (not ,test)
		     ,(macroexpand `(setf ,place ,signal-error)))
                 ;; Common Lisp requires that PLACE be setfable, but this is
                 ;; never a restriction that this package has enforced.
		 (error
		  `(if (not ,test) (progn ,signal-error nil))))))
	 (if (eq temp place) `(progn ,body nil)
	   `(let ((,temp ,place)) ,body nil)))))

;;;###autoload
(defmacro assert (form &optional show-args string &rest args)
  "Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used."
  (and (or (not (cl-compiling-file))
	   (< cl-optimize-speed 3) (= cl-optimize-safety 3))
       (let ((sargs (and show-args
                         (if (consp form)
                             ;; #'remove-if isn't necessarily available yet.
                             (remove* t (cdr form) :key #'cl-const-expr-p))
                         (list form))))
	 (list 'progn
	       (list 'or form
		     (if string
			 (list* 'error string (append sargs args))
		       (list 'signal '(quote cl-assertion-failed)
			     (list* 'list (list 'quote form) sargs))))
	       nil))))

;;;###autoload
(defmacro ignore-errors (&rest body)
  "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY."
  `(condition-case nil (progn ,@body) (error nil)))

;; XEmacs addition
;;;###autoload
(defmacro ignore-file-errors (&rest body)
  "Execute FORMS; if an error of type `file-error' occurs, return nil.
Otherwise, return result of last FORM."
  `(condition-case nil (progn ,@body) (file-error nil)))


;;; Compiler macros.

;;;###autoload
(defmacro define-compiler-macro (func args &rest body)
  "Define a compiler-only macro.
This is like `defmacro', but macro expansion occurs only if the call to
FUNC is compiled (i.e., not interpreted).  Compiler macros should be used
for optimizing the way calls to FUNC are compiled; the form returned by
BODY should do the same thing as a call to the normal function called
FUNC, though possibly more efficiently.  Note that, like regular macros,
compiler macros are expanded repeatedly until no further expansions are
possible.  Unlike regular macros, BODY can decide to \"punt\" and leave the
original function call alone by declaring an initial `&whole foo' parameter
and then returning foo."
  (let ((p (if (listp args) args (list '&rest args))) (res nil))
    (while (consp p) (push (pop p) res))
    (setq args (nconc (nreverse res) (and p (list '&rest p)))))
  (list 'eval-when '(compile load eval)
	(cl-transform-function-property
	 func 'cl-compiler-macro
	 (cons (if (memq '&whole args) (delete* '&whole args)
		 (cons '--cl-whole-arg-- args)) body))
	(list 'or (list 'get (list 'quote func) '(quote byte-compile))
	      (list 'put (list 'quote func) '(quote byte-compile)
		    '(quote cl-byte-compile-compiler-macro)))))

;;;###autoload
(defun compiler-macroexpand (form)
  (while
      (let ((func (car-safe form)) (handler nil))
	(while (and (symbolp func)
		    (not (setq handler (get func 'cl-compiler-macro)))
		    (fboundp func)
		    (or (not (eq (car-safe (symbol-function func)) 'autoload))
			(load (nth 1 (symbol-function func)))))
	  (setq func (symbol-function func)))
	(and handler
	     (not (eq form (setq form (apply handler form (cdr form))))))))
  form)

(defun cl-byte-compile-compiler-macro (form)
  (if (eq form (setq form (compiler-macroexpand form)))
      (byte-compile-normal-call form)
    (byte-compile-form form)))

(defmacro defsubst* (name arglist &optional docstring &rest body)
  "Define NAME as a function.
Like `defun', except the function is automatically declared `inline',
ARGLIST allows full Common Lisp conventions, and BODY is implicitly
surrounded by (block NAME ...)."
  (let* ((argns (cl-arglist-args arglist)) (p argns)
         (exec-body (if (or (stringp docstring) (null docstring))
                        body
                      (cons docstring body)))
	 (pbody (cons 'progn exec-body))
	 (unsafe (not (cl-safe-expr-p pbody))))
    (while (and p (eq (cl-expr-contains arglist (car p)) 1)) (pop p))
    (list 'progn
	  (if (or p (memq '&rest arglist))
              ; Defaults refer to earlier args, or we would have to cons up
              ; something for &rest:
              (list 'proclaim-inline name) 
	    (list 'define-compiler-macro name
		  (if (memq '&key arglist)
		      (list* '&whole 'cl-whole '&cl-quote arglist)
		    (cons '&cl-quote arglist))
		  (list* 'cl-defsubst-expand (list 'quote argns)
			 (list 'quote (list* 'block name exec-body))
			 (not (or unsafe (cl-expr-access-order pbody argns)))
			 (and (memq '&key arglist) 'cl-whole) unsafe argns)))
	  (list* 'defun* name arglist docstring body))))

(defun cl-defsubst-expand (argns body simple whole unsafe &rest argvs)
  (if (and whole (not (cl-safe-expr-p (cons 'progn argvs))))
      whole
    (if (cl-simple-exprs-p argvs)
        (setq simple t))
    (let* ((symbol-macros nil)
           (lets (mapcan #'(lambda (argn argv)
                             (if (or simple (cl-const-expr-p argv))
                                 (progn 
				   ;; Avoid infinite loop on symbol macro
				   ;; expansion:
				   (or (block find
                                         (subst nil argn argvs :test
                                                #'(lambda (elt tree)
                                                    ;; Give nil if argn is
                                                    ;; in argvs somewhere:
						    (if (eq elt tree)
							(return-from find)))))
                                       (let ((copy-symbol (copy-symbol argn)))
                                         ;; Rename ARGN within BODY so it
                                         ;; doesn't conflict with its value
                                         ;; in the including scope:
                                         (setq body
                                               (cl-macroexpand-all
                                                body `((,(eq-hash argn)
                                                        ,copy-symbol t)))
                                               argn copy-symbol)))
                                   (push (list argn argv t) symbol-macros)
                                   (and unsafe (list (list argn argv))))
                               (list (list argn argv))))
                         argns argvs)))
      `(let ,lets
         (symbol-macrolet
             ;; We don't have GNU's issue where the replacement will be done
             ;; when the symbol is used in a function context, because we're
             ;; using #'symbol-macrolet instead of #'subst. And we're using a
             ;; new, third field to the symbol expansion which specifies that
             ;; shadowing for `let', `let*' is to be applied, which avoids
             ;; problems with those subforms.
             ,symbol-macros
           ,body)))))

;; When a 64-bit build is byte-compiling code, some of its native fixnums
;; will not be represented as fixnums if the byte-compiled code is read by
;; the Lisp reader in a 32-bit build. So in that case we need to check the
;; range of fixnums as well as their types. XEmacs doesn't support machines
;; with word size less than 32, so it's OK to have that as the minimum.
(macrolet
    ((most-positive-fixnum-on-32-bit-machines () (1- (lsh 1 30)))
     (most-negative-fixnum-on-32-bit-machines ()
       (lognot (most-positive-fixnum-on-32-bit-machines))))
  (defun cl-non-immediate-number-p (object)
    "Return t if OBJECT is a number not guaranteed to be immediate."
    (and (numberp object)
	 (or (not (fixnump object))
	     (not (<= (most-negative-fixnum-on-32-bit-machines)
		      object
		      (most-positive-fixnum-on-32-bit-machines)))))))

;;; Compile-time optimizations for some functions defined in this package.
;;; Note that cl.el arranges to force cl-macs to be loaded at compile-time,
;;; mainly to make sure these macros will be present.

(define-compiler-macro eql (&whole form a b)
  (cond ((eq (cl-const-expr-p a) t)
	 (let ((val (cl-const-expr-val a)))
	   (if (cl-non-immediate-number-p val)
	       (list 'equal a b)
	     (list 'eq a b))))
	((eq (cl-const-expr-p b) t)
	 (let ((val (cl-const-expr-val b)))
	   (if (cl-non-immediate-number-p val)
	       (list 'equal a b)
	     (list 'eq a b))))
	(t form)))

(defun cl-equal-equivalent-to-eq-p (object)
  (or (symbolp object) (characterp object)
      (and (fixnump object) (not (cl-non-immediate-number-p object)))))

(defun cl-car-or-pi (object)
  (if (consp object) (car object) pi))

(defun cl-cdr-or-pi (object)
  (if (consp object) (cdr object) pi))

(define-compiler-macro equal (&whole form &rest args)
  (cond
   ((not (eql (length form) 3))
    form)
   ((or (cl-equal-equivalent-to-eq-p (cl-const-expr-val (pop args) pi))
        (cl-equal-equivalent-to-eq-p (cl-const-expr-val (pop args) pi)))
    (cons 'eq (cdr form)))
   (t form)))

(define-compiler-macro member (&whole form &rest args)
  (cond
   ((not (eql (length form) 3))
    form)
   ((or (cl-equal-equivalent-to-eq-p (cl-const-expr-val (pop args) pi))
        (every #'cl-equal-equivalent-to-eq-p
               (cl-const-expr-val (pop args) '(1.0))))
    (cons 'memq (cdr form)))
   (t form)))

(define-compiler-macro assoc (&whole form &rest args)
  (cond
   ((not (eql (length form) 3))
    form)
   ((or (cl-equal-equivalent-to-eq-p (cl-const-expr-val (pop args) pi))
        (not (find-if-not #'cl-equal-equivalent-to-eq-p
                          (cl-const-expr-val (pop args) '((1.0 . nil)))
                          :key #'cl-car-or-pi)))
    (cons 'assq (cdr form)))
   (t form)))

(define-compiler-macro rassoc (&whole form &rest args)
  (cond
   ((not (eql (length form) 3))
    form)
   ((or (cl-equal-equivalent-to-eq-p (cl-const-expr-val (pop args) pi))
        (not (find-if-not #'cl-equal-equivalent-to-eq-p
                            (cl-const-expr-val (pop args) '((nil . 1.0)))
                            :key #'cl-cdr-or-pi)))
    (cons 'rassq (cdr form)))
   (t form)))

(macrolet
    ((define-star-compiler-macros (&rest macros)
       "For `member*', `assoc*' and `rassoc*' with constant ITEM or
:test arguments, use the versions with explicit tests if that makes sense."
       (list*
	'progn
	(mapcar
	 (function*
	  (lambda ((star-function eq-function equal-function))
	    `(define-compiler-macro ,star-function (&whole form &rest keys)
              (if (< (length form) 3)
                  form
                (condition-case nil
                    (symbol-macrolet ((not-constant '#:not-constant))
                      (let* ((item (pop keys))
                             (list (pop keys))
                             (test-expr (plist-get keys :test ''eql))
                             (test (cl-const-expr-val test-expr not-constant))
                             (item-val (cl-const-expr-val item not-constant))
                             (list-val (cl-const-expr-val list not-constant)))
                        (if (and keys (not (and (eq :test (car keys))
                                                (eql 2 (length keys)))))
                            form
                          (cond ((eq test 'eq) `(,',eq-function ,item ,list))
                                ((eq test 'equal)
                                 `(,',equal-function ,item ,list))
                                ((and (eq test 'eql)
                                      (not (eq not-constant item-val)))
                                 (if (cl-non-immediate-number-p item-val)
                                     `(,',equal-function ,item ,list)
                                   `(,',eq-function ,item ,list)))
                                ((and (eq test 'eql) (not (eq not-constant
                                                              list-val)))
                                 (if (some 'cl-non-immediate-number-p list-val)
                                     `(,',equal-function ,item ,list)
                                   ;; This compiler macro used to limit
                                   ;; calls to ,,eq-function to lists where
                                   ;; all elements were either fixnums or
                                   ;; symbols. There's no reason to do this.
                                   `(,',eq-function ,item ,list)))
                                ;; This is a hilariously specific case; see
                                ;; add-to-list in subr.el.
                                ((and (eq test not-constant)
                                      (eq 'or (car-safe test-expr))
                                      (eql 3 (length test-expr))
                                      (every #'cl-safe-expr-p (cdr form))
                                      `(if ,(second test-expr)
                                        (,',star-function ,item ,list :test
                                                          ,(second test-expr))
                                        (,',star-function
                                         ,item ,list :test
                                         ,(third test-expr)))))
                                (t form)))))
                  ;; No need to warn about a malformed property list,
                  ;; #'byte-compile-normal-call will do that for us.
                  (malformed-property-list form))))))
	 macros))))
  (define-star-compiler-macros
    (member* memq member)
    (assoc* assq assoc)
    (rassoc* rassq rassoc)))

(define-compiler-macro adjoin (&whole form a list &rest keys)
  (if (and (cl-simple-expr-p a) (cl-simple-expr-p list)
	   (not (memq :key keys)))
      (list 'if (list* 'member* a list keys) list (list 'cons a list))
    form))

(define-compiler-macro delete (&whole form &rest args)
  (if (eql 3 (length form))
      (symbol-macrolet ((not-constant '#:not-constant))
        (let ((cl-const-expr-val (cl-const-expr-val (nth 1 form) not-constant)))
          (if (and (cdr form) (not (eq not-constant cl-const-expr-val))
                   (or (symbolp cl-const-expr-val) (fixnump cl-const-expr-val)
                       (characterp cl-const-expr-val)))
              (cons 'delete* (cdr form))
            `(delete* ,@(cdr form) :test #'equal))))
    form))

(define-compiler-macro delq (&whole form &rest args)
  (if (eql 3 (length form))
      (symbol-macrolet
          ((not-constant '#:not-constant))
        (let ((cl-const-expr-val (cl-const-expr-val (nth 1 form) not-constant)))
          (if (and (cdr form) (not (eq not-constant cl-const-expr-val))
                   (not (cl-non-immediate-number-p cl-const-expr-val)))
              (cons 'delete* (cdr form))
            `(delete* ,@(cdr form) :test #'eq))))
    form))

(define-compiler-macro remove (&whole form &rest args)
  (if (eql 3 (length form))
      (symbol-macrolet
          ((not-constant '#:not-constant))
        (let ((cl-const-expr-val (cl-const-expr-val (nth 1 form) not-constant)))
          (if (and (cdr form) (not (eq not-constant cl-const-expr-val))
                   (or (symbolp cl-const-expr-val) (fixnump cl-const-expr-val)
                       (characterp cl-const-expr-val)))
              (cons 'remove* (cdr form))
            `(remove* ,@(cdr form) :test #'equal))))
    form))

(define-compiler-macro remq (&whole form &rest args)
  (if (eql 3 (length form))
      (symbol-macrolet
          ((not-constant '#:not-constant))
        (let ((cl-const-expr-val (cl-const-expr-val (nth 1 form) not-constant)))
          (if (and (cdr form) (not (eq not-constant cl-const-expr-val))
                   (not (cl-non-immediate-number-p cl-const-expr-val)))
              (cons 'remove* (cdr form))
            `(remove* ,@(cdr form) :test #'eq))))
    form))
 
(macrolet
    ((define-foo-if-compiler-macros (&rest alist)
       "Avoid the funcall, variable binding and keyword parsing overhead
for the FOO-IF and FOO-IF-NOT functions, transforming to forms using the
non-standard :if and :if-not keywords at compile time."
       (cons
	'progn
	(mapcar
	 (function*
	  (lambda ((function-if . function))
	    (let ((keyword (if (equal (substring (symbol-name function-if) -3)
				      "not")
			       :if-not
			     :if)))
	      `(define-compiler-macro ,function-if (&whole form &rest args)
		 (if (and (nthcdr 2 form)
			  (or (consp (cl-const-expr-val (second form)))
			      (cl-safe-expr-p (second form))))
		     ;; It doesn't matter what the second argument is, it's
		     ;; ignored by FUNCTION.  We know that the symbol
		     ;; FUNCTION is in the constants vector, so use it.
		     `(,',function ',',function ,(third form) ,,keyword
		       ,(second form) ,@(nthcdr 3 form))
		   form)))))
	 alist))))
  (define-foo-if-compiler-macros
    (remove-if . remove*)
    (remove-if-not . remove*)
    (delete-if . delete*)
    (delete-if-not . delete*)
    (find-if . find)
    (find-if-not . find)
    (position-if . position)
    (position-if-not . position)
    (count-if . count)
    (count-if-not . count)
    (member-if . member*)
    (member-if-not . member*)
    (assoc-if . assoc*)
    (assoc-if-not . assoc*)
    (rassoc-if . rassoc*)
    (rassoc-if-not . rassoc*)))

(macrolet
    ((define-substitute-if-compiler-macros (&rest alist)
       "Like the above, but for `substitute-if' and friends."
       (cons
	'progn
	(mapcar
	 (function*
	  (lambda ((function-if . function))
	    (let ((keyword (if (equal (substring (symbol-name function-if) -3)
				      "not")
			       :if-not
			     :if)))
	      `(define-compiler-macro ,function-if (&whole form &rest args)
		 (if (and (nthcdr 3 form)
			  (or (consp (cl-const-expr-val (third form)))
			      (cl-safe-expr-p (third form))))
		     `(,',function ,(second form) ',',function ,(fourth form)
		       ,,keyword ,(third form) ,@(nthcdr 4 form))
		   form)))))
	 alist))))
  (define-substitute-if-compiler-macros
    (substitute-if . substitute)
    (substitute-if-not . substitute)
    (nsubstitute-if . nsubstitute)
    (nsubstitute-if-not . nsubstitute)))

(macrolet
    ((define-subst-if-compiler-macros (&rest alist)
       "Like the above, but for `subst-if' and friends."
       (cons
	'progn
	(mapcar
	 (function*
	  (lambda ((function-if . function))
	    (let ((keyword (if (equal (substring (symbol-name function-if) -3)
				      "not")
			       :if-not
			     :if)))
	      `(define-compiler-macro ,function-if (&whole form &rest args)
		(if (and (nthcdr 3 form)
			 (or (consp (cl-const-expr-val (third form)))
			     (cl-safe-expr-p (third form))))
		    `(,',function ,(if (cl-const-expr-p (second form))
				       `'((nil . ,(cl-const-expr-val
						   (second form))))
				     `(list (cons ',',function
						  ,(second form))))
		      ,(fourth form) ,,keyword ,(third form)
		      ,@(nthcdr 4 form))
		   form)))))
	 alist))))
  (define-subst-if-compiler-macros
    (subst-if . sublis)
    (subst-if-not . sublis)
    (nsubst-if . nsublis)
    (nsubst-if-not . nsublis)))

(define-compiler-macro list* (arg &rest others)
  (let* ((args (reverse (cons arg others)))
	 (form (car args)))
    (while (setq args (cdr args))
      (setq form (list 'cons (car args) form)))
    form))

;; XEmacs change: our builtin get takes the default argument
(define-compiler-macro get* (sym prop &optional default)
  (list 'get sym prop default))

(define-compiler-macro getf (sym prop &optional default)
  (list 'plist-get sym prop default))

(define-compiler-macro typep (&whole form val type)
  (if (cl-const-expr-p type)
      (let ((res (cl-make-type-test val (cl-const-expr-val type))))
	(if (or (memq (cl-expr-contains res val) '(nil 1))
		(cl-simple-expr-p val)) res
	  (let ((temp (gensym)))
	    (list 'let (list (list temp val)) (subst temp val res)))))
    form))

(define-compiler-macro apply-partially (&whole form &rest args)
  "Generate a #'make-byte-code call for #'apply-partially, if appropriate."
  (when (< (length args) 1)
    (return-from apply-partially form))
  (let* ((values (cdr args)) (count (length values))
         (placeholders (mapcar #'quote-maybe (mapcar #'gensym values)))
         (sublis (pairlis placeholders values))
         restp lambda arglist bindings compiled)
    (when (and (eq 'function (car-safe (nth 0 args)))
               (eq 'lambda (car-safe (nth 1 (nth 0 args)))))
      (setq lambda (nth 1 (nth 0 args))
            arglist (nth 1 lambda))
      (when (> count (function-max-args lambda))
        (byte-compile-warn
         "attempt to apply-partially %S with too many arguments" lambda)
        (return-from apply-partially form))
      (while (and arglist placeholders)
	(cond ((eq (car arglist) '&optional)
	       (if restp
                   (error 'syntax-error
                          "&optional found after &rest in %S" lambda))
	       (if (null (cdr arglist))
		   (error 'syntax-error "nothing after &optional in %S"
                          lambda)))
	      ((eq (car arglist) '&rest)
	       (if (null (cdr arglist))
		   (error 'syntax-error "nothing after &rest in %S" lambda))
	       (if (cdr (cdr arglist))
		   (error 'syntax-error "multiple vars after &rest in %S"
                          lambda))
	       (setq restp t))
	      (restp
	       (setq bindings (cons (list (car arglist)
					  (and placeholders
                                               (cons 'list placeholders)))
				    bindings)
		     placeholders nil))
	      (t
	       (setq bindings (cons (list (car arglist) (car placeholders))
				    bindings)
		     placeholders (cdr placeholders))))
	(setq arglist (cdr arglist)))
      (when (cl-const-exprs-p values)
        ;; Values are constant, no need to construct the compiled function
        ;; at runtime.
        (return-from apply-partially
          (byte-compile-lambda
           `(lambda ,arglist (let ,(sublis sublis (nreverse bindings)
                                           :test #'equal)
                               ,@(cddr lambda))))))
      (setq compiled (byte-compile-lambda
                      `(lambda ,arglist (let ,(nreverse bindings)
                                          ,@(cddr lambda)))))
      (return-from apply-partially
        `(make-byte-code
          ',(compiled-function-arglist compiled)
          ,(compiled-function-instructions compiled)
          (vector ,@(sublis sublis
                            (mapcar 'quote-maybe
                                    (compiled-function-constants compiled))
                            :test 'equal))
          ,(compiled-function-stack-depth compiled))))
    (if (cl-const-exprs-p args)
        `#'(lambda (&rest args) (apply ,@args args))
        (let* ((placeholders (mapcar 'quote-maybe (mapcar 'gensym args)))
               (compiled (byte-compile-sexp
                          `#'(lambda (&rest args)
                               (apply ,@placeholders args)))))
          (assert (equal (intersection
                          (mapcar 'quote-maybe (compiled-function-constants
                                                compiled))
                          placeholders :test 'equal :stable t)
                         placeholders)
                  t "This macro requires that the relative order is the same\
in the constants vector and in the arguments")
          `(make-byte-code
            ',(compiled-function-arglist compiled)
            ,(compiled-function-instructions compiled)
            (vector ,@(sublis (pairlis placeholders args)
                              (mapcar 'quote-maybe
                                      (compiled-function-constants compiled))
                              :test 'equal))
            ,(compiled-function-stack-depth compiled))))))

(define-compiler-macro delete-dups (list)
  `(delete-duplicates (the list ,list) :test #'equal :from-end t))

;; XEmacs; inline delete-duplicates if it's called with one of the
;; common compile-time constant tests and an optional :from-end
;; argument, we want the speed in font-lock.el.
(define-compiler-macro delete-duplicates (&whole form &rest cl-keys)
  (let ((cl-seq (if cl-keys (pop cl-keys))))
    (if (or 
	 (not (or (memq (car-safe cl-seq)
			;; No need to check for a list at runtime with
			;; these. We could expand the list, but these are all
			;; the functions in the relevant context at the moment.
			'(nreverse append nconc mapcan mapcar string-to-list))
		 (and (listp cl-seq) (equal (butlast cl-seq) '(the list)))))
	 ;; Wrong number of arguments.
	 (not (cdr form)))
	form
      (cond
       ((or (plists-equal cl-keys '(:test 'eq) t)
	    (plists-equal cl-keys '(:test #'eq) t))
	`(let* ((begin ,cl-seq)
		cl-seq)
	  (while (memq (car begin) (cdr begin))
	    (setq begin (cdr begin)))
	  (setq cl-seq begin)
	  (while (cddr cl-seq)
	    (if (memq (cadr cl-seq) (cddr cl-seq))
		(setcdr (cdr cl-seq) (cddr cl-seq)))
	    (setq cl-seq (cdr cl-seq)))
	  begin))
       ((or (plists-equal cl-keys '(:test 'eq :from-end t) t)
	    (plists-equal cl-keys '(:test #'eq :from-end t) t))
	`(let* ((begin ,cl-seq)
		(cl-seq begin))
	  (while cl-seq
	    (setq cl-seq (setcdr cl-seq
				 (delete* (car cl-seq) (cdr cl-seq)))))
	  begin))
       ((or (plists-equal cl-keys '(:test 'equal) t)
	    (plists-equal cl-keys '(:test #'equal) t))
	`(let* ((begin ,cl-seq)
		cl-seq)
	  (while (member (car begin) (cdr begin))
	    (setq begin (cdr begin)))
	  (setq cl-seq begin)
	  (while (cddr cl-seq)
	    (if (member (cadr cl-seq) (cddr cl-seq))
		(setcdr (cdr cl-seq) (cddr cl-seq)))
	    (setq cl-seq (cdr cl-seq)))
	  begin))
       ((or (plists-equal cl-keys '(:test 'equal :from-end t) t)
	    (plists-equal cl-keys '(:test #'equal :from-end t) t))
	`(let* ((begin ,cl-seq)
		(cl-seq begin))
	  (while cl-seq
	    (setq cl-seq (setcdr cl-seq (delete (car cl-seq)
						(cdr cl-seq)))))
	  begin))
       (t form)))))

;; XEmacs; it's perfectly reasonable, and often much clearer to those
;; reading the code, to call regexp-quote on a constant string, which is
;; something we can optimise here easily.
(define-compiler-macro regexp-quote (&whole form string)
  (if (stringp string)
      (regexp-quote string)
    form))

;; NOTE: `equalp' is now a primitive, although as of yet it still doesn't
;; have a byte-compiler opcode for it.  The compiler-macro for `equalp' used
;; to try and remove as much as possible of the logic of the Lisp `equalp' as
;; possible whenever one of the arguments is a constant, boiling things down
;; to a few if-statements and some calls to various no-longer-defined
;; helper functions.  Besides the fact that the helper functions aren't
;; defined, there's little point in doing any of that expansion, since it will
;; end up executing in Lisp what would otherwise be done in C by a direct
;; call to `equalp'.  The only exception is when the reduction is quite
;; simple and is to functions that do have op-codes; that may gain something.
;; However, if `equalp' becomes an opcode itself, consider removing everything
;; here except maybe when the call can directly be reduced to `equal' or `eq'.
;;
;; --ben

(define-compiler-macro equalp (&whole form x y) 
  "Expand calls to `equalp' where X or Y is a constant expression.

Much of the processing that `equalp' does is dependent on the types of both
of its arguments, and with type information for one of them, we can
eliminate much of the body of the function at compile time.

Where both X and Y are constant expressions, `equalp' is evaluated at
compile time by byte-optimize.el--this compiler macro passes FORM through to
the byte optimizer in those cases."
  ;; Cases where both arguments are constant are handled in
  ;; byte-optimize.el, we only need to handle those cases where one is
  ;; constant here.
  (let* ((equalp-sym (eval-when-compile (gensym)))
	(let-form '(progn))
	(original-y y)
	equalp-temp checked)
  (macrolet
      ((unordered-check (check)
	 `(prog1
	     (setq checked
		   (or ,check
		       (prog1 ,(sublis '((x . y) (y . x)) check :test #'eq)
			 (setq equalp-temp x x y y equalp-temp))))
	   (when checked
	     (unless (symbolp y)
	       (setq let-form `(let ((,equalp-sym ,y))) y equalp-sym))))))
    ;; In the bodies of the below clauses, x is always a constant expression
    ;; of the type we're interested in, and y is always a symbol that refers
    ;; to the result non-constant side of the comparison. 
    (cond ((unordered-check (and (arrayp x) (not (cl-const-expr-p y))))
	   ;; Strings and other arrays. A vector containing the same
	   ;; character elements as a given string is equalp to that string;
	   ;; a bit-vector can only be equalp to a string if both are
	   ;; zero-length.
	   (cond
	    ((member x '("" #* []))
	     `(and (member ,(find x (cdr form) :test-not #'eq) '("" #* [])) t))
	    (t form)))
	  ((unordered-check (and (numberp x) (not (cl-const-expr-p y))))
	   `(,@let-form
	     (and (numberp ,y)
		  (= ,x ,y))))
	  ((unordered-check (and (hash-table-p x) (not (cl-const-expr-p y))))
	   form)
	  ((unordered-check
	    ;; Symbols; eq. 
	    (and (not (cl-const-expr-p y))
		 (or (memq x '(nil t))
		     (and (eq (car-safe x) 'quote) (symbolp (second x))))))
	   (cons 'eq (cdr form)))

	  ;; This clause is wrong -- e.g. when comparing a constant char-table
	  ;; against a non-constant expression that evaluates to a char-table,
	  ;; or some for range tables or certain other types, `equalp' is
	  ;; not the same as `equal'.  We could insert the known list of
	  ;; types with special `equalp' property, but it's fragile and may
	  ;; not be much of an optimization, esp. since these types don't
	  ;; occur that often are often big.
	  ;;((unordered-check
	  ;;  ;; Compare conses at runtime, there's no real upside to
	  ;;  ;; unrolling the function -> they fall through to the next
	  ;;  ;; clause in this function.
	  ;;  (and (cl-const-expr-p x) (not (consp x))
	  ;;       (not (cl-const-expr-p y))))
	  ;; ;; All other types; use equal.
	  ;; (cons 'equal (cdr form)))
	  
	  ;; Neither side is a constant expression, do all our evaluation at
	  ;; runtime (or both are, and equalp will be called from
	  ;; byte-optimize.el).
	  (t form)))))

(define-compiler-macro notany (&whole form &rest cl-rest)
  `(not (some ,@(cdr form))))

(define-compiler-macro notevery (&whole form &rest cl-rest)
  `(not (every ,@(cdr form))))

(define-compiler-macro constantly (&whole form value &rest more-values)
  (cond
   ((< (length form) 2)
    ;; Error at runtime:
    form)
   ((cl-const-exprs-p (cdr form))
    `#'(lambda (&rest ignore) (values ,@(cdr form))))
   (t
    (let* ((num-values (length (cdr form)))
	   (placeholders-counts (make-vector num-values -1))
	   (placeholders (loop
			   for i from 0 below num-values
			   collect (make-symbol (format "%d" i))))
	   (compiled
	    (byte-compile-sexp
	     `#'(lambda (&rest ignore)
		  ;; Compiles to a references into the compiled function
		  ;; constants vector:
		  (values ,@(mapcar #'quote-maybe placeholders)))))
	   position)
      `(make-byte-code '(&rest ignore)
	,(compiled-function-instructions compiled)
	(vector ,@(loop
		    for constant across (compiled-function-constants compiled)
		    collect (if (setq position
				      (position constant placeholders))
				(prog2
				    (incf (aref placeholders-counts position))
				    (nth position (cdr form)))
			      (quote-maybe constant))
		    finally
		    (assert (every #'zerop placeholders-counts)
			    t "Placeholders should each have been used once")))
	,(compiled-function-stack-depth compiled))))))

(define-compiler-macro stable-sort (&whole form &rest cl-rest)
  (cons 'sort* (cdr form)))

(define-compiler-macro svref (&whole form)
  (cons 'aref (cdr form)))

(define-compiler-macro acons (a b c)
  `(cons (cons ,a ,b) ,c))

(define-compiler-macro pairlis (a b &optional c)
  `(nconc (mapcar* #'cons ,a ,b) ,c))

(define-compiler-macro revappend (&whole form &rest args)
  (if (eql 3 (length form)) `(nconc (reverse ,(pop args)) ,(pop args)) form))

(define-compiler-macro nreconc (&whole form &rest args)
  (if (eql 3 (length form)) `(nconc (nreverse ,(pop args)) ,(pop args)) form))

(define-compiler-macro complement (&whole form fn)
  (if (or (eq (car-safe fn) 'function) (eq (car-safe fn) 'quote))
      (cond
       ((and (symbolp (second fn)) (get (second fn) 'byte-compile-negated-op))
        (list 'function (get (second fn) 'byte-compile-negated-op)))
       ((and (symbolp (second fn)) (fboundp (second fn))
             (compiled-function-p (indirect-function (second fn))))
        (let* ((cf (indirect-function (second fn)))
               (cfa (compiled-function-arglist cf))
               (do-apply (memq '&rest cfa)))
          `#'(lambda ,cfa
               (not (,@(if do-apply `(apply ',(second fn)) (list (second fn)))
                       ,@(remq '&optional
                               (remq '&rest cfa)))))))
       (t
        `#'(lambda (&rest arguments)
             (not (apply ,fn arguments)))))
    ;; Determine the function to call at runtime.
    (destructuring-bind
        (arglist instructions constants stack-depth)
        (let ((compiled-lambda
               (byte-compile-sexp
                #'(lambda (&rest arguments)
                    (not (apply 'placeholder arguments))))))
          (list
           (compiled-function-arglist compiled-lambda)
           (compiled-function-instructions compiled-lambda)
           (append (compiled-function-constants compiled-lambda) nil)
           (compiled-function-stack-depth compiled-lambda)))
      `(make-byte-code
        ',arglist ,instructions (vector
                                 ,@(nsublis
                                    (list (cons (quote-maybe
                                                 'placeholder)
                                                fn))
                                    (mapcar #'quote-maybe constants)
                                    :test #'equal))
        ,stack-depth))))

(define-compiler-macro concatenate (&whole form type &rest seqs)
  (if (and (cl-const-expr-p type) (memq (cl-const-expr-val type)
                                        '(vector bit-vector list string)))
      (case (cl-const-expr-val type)
        (list (append (list 'append) (cddr form) '(nil)))
        (vector (cons 'vconcat (cddr form)))
        (bit-vector (cons 'bvconcat (cddr form)))
        (string (cons 'concat (cddr form))))
    form))

(define-compiler-macro subst-char-in-string (&whole form fromchar tochar
					     string &optional inplace)
  (if (every #'cl-safe-expr-p (cdr form))
      `(funcall (if ,inplace #'nsubstitute #'substitute) ,tochar ,fromchar
	(the string ,string) :test #'eq)
    form))

(define-compiler-macro assoc-ignore-case (&whole form &rest args)
  (if (eql 2 (length args))
      `(assoc* (the string ,(pop args))
               (the (and list (satisfies
                               (lambda (list)
                                 (not (find-if-not 'stringp list :key 'car)))))
                    ,(pop args))
               :test 'equalp)
    form))

(define-compiler-macro assoc-ignore-representation (&whole form &rest args)
  (if (eql 2 (length args))
      `(assoc* (the string ,(pop args))
               (the (and list (satisfies
                               (lambda (list)
                                 (not (find-if-not 'stringp list :key 'car)))))
                    ,(pop args))
               :test 'equalp)
    form))

(define-compiler-macro member-ignore-case (&whole form &rest args)
  (if (eql 2 (length args))
      `(member* (the string ,(pop args))
                (the (and list (satisfies
                                (lambda (list) (every 'stringp list))))
                     ,(pop args))
                :test 'equalp)
    form))

(define-compiler-macro stable-union (&whole form &rest cl-keys)
  (if (> (length form) 2)
      (list* 'union (pop cl-keys) (pop cl-keys) :stable t cl-keys)
    form))

(define-compiler-macro stable-intersection (&whole form &rest cl-keys)
  (if (> (length form) 2)
      (list* 'intersection (pop cl-keys) (pop cl-keys) :stable t cl-keys)
    form))

(define-compiler-macro princ (&whole form object &optional stream)
  "When passing `princ' a string, call `write-sequence' instead.

This avoids the resource- and time-intensive initialization of the printer,
and functions equivalently. Such code will not run on 21.4, but 21.4 will
not normally encounter it, and the error message will be clear enough (that
`write-sequence' has a void function definition) in the odd event that it
does."
  (cond ((not (<= 2 (length form) 3))
	 form)
	((or (stringp object)
	     (member (car-safe object)
		     '(buffer-string buffer-substring concat format gettext
		       key-description make-string mapconcat
		       substitute-command-keys substring-no-properties
		       symbol-name text-char-description string)))
	 (cons 'write-sequence (cdr form)))
	((member (car-safe object) '(substring subseq))
	 `(write-sequence ,(nth 1 object) ,stream :start ,(nth 2 object)
	                  ,@(if (nth 3 object) `((:end ,(nth 3 object))))))
	(t form)))

;; No point doing this, if terpri is a loop hotspot the Lisp programmer is
;; doing something wrong. We win relatively by having one symbol plus a
;; funcall instruction in the compiled function rather than a symbol, a
;; funcall instruction, a character, and two not instructions.
;(define-compiler-macro terpri (&whole form &optional stream)
;  `(not (not (write-char ?\n ,@(cdr form)))))

(map nil
     #'(lambda (function)
         ;; There are byte codes for the two-argument versions of these
         ;; functions; if the form has more arguments and those arguments
         ;; have no side effects, transform to a series of two-argument
         ;; calls.
         (put function 'cl-compiler-macro
              #'(lambda (form &rest arguments)
                  (if (or (null (nthcdr 3 form))
                          (notevery #'cl-safe-expr-p (butlast (cdr arguments))))
                      form
                    (cons 'and (mapcon
                                #'(lambda (rest)
                                    (and (cdr rest)
                                         `((,(car form) ,(pop rest)
                                            ,(car rest)))))
                                (cdr form)))))))
     '(= < > <= >=))

;; XEmacs; unroll this loop at macro-expansion time, so the compiler macros
;; are byte-compiled.
(macrolet
    ((inline-side-effect-free-compiler-macros (&rest details)
       (cons
        'progn
        (loop
          for (function . details) in details
          nconc `((put ',function 'side-effect-free t)
                  (define-compiler-macro ,function (&whole form x)
                    ,(if (symbolp (car details))
                         (reduce #'(lambda (object1 object2)
                                     `(list ',object1 ,object2))
                                 details :from-end t :initial-value 'x)
                       (cons 'list details))))))))
  (inline-side-effect-free-compiler-macros
   (first 'car x) (second 'cadr x) (third 'caddr x) (fourth 'cadddr x)
   (fifth 'nth 4 x) (sixth 'nth 5 x) (seventh 'nth 6 x)
   (eighth 'nth 7 x) (ninth 'nth 8 x) (tenth 'nth 9 x)
   (rest 'cdr x) (plusp '> x 0) (minusp '< x 0)
   (oddp  'eql (list 'logand x 1) 1)
   (evenp 'eql (list 'logand x 1) 0)
   (caar car car) (cadr car cdr) (cdar cdr car) (cddr cdr cdr)
   (caaar car caar) (caadr car cadr) (cadar car cdar)
   (caddr car cddr) (cdaar cdr caar) (cdadr cdr cadr)
   (cddar cdr cdar) (cdddr cdr cddr) (caaaar car caaar)
   (caaadr car caadr) (caadar car cadar) (caaddr car caddr)
   (cadaar car cdaar) (cadadr car cdadr) (caddar car cddar)
   (cadddr car cdddr) (cdaaar cdr caaar) (cdaadr cdr caadr)
   (cdadar cdr cadar) (cdaddr cdr caddr) (cddaar cdr cdaar)
   (cddadr cdr cdadr) (cdddar cdr cddar) (cddddr cdr cdddr)))

;;; Things that are inline. XEmacs; the functions that used to be here have
;;; compiler macros or are built-in.
(proclaim '(inline cl-set-elt))

;;; Things that are side-effect-free.  Moved to byte-optimize.el
;[...]

;;; Things that are side-effect-and-error-free.  Moved to byte-optimize.el
;[...]

;; XEmacs; move the following macros to the end of this file, since the
;; override the versions in byte-compile-initial-macro-environment for the
;; duration of the file they're defined in.

;;;###autoload
(defmacro the (type form)
  "Assert that FORM gives a result of type TYPE, and return that result.

TYPE is a Common Lisp type specifier.

If macro expansion of a `the' form happens during byte compilation, and the
byte compiler customization variable `byte-compile-delete-errors' is
non-nil, `the' is equivalent to FORM without any type checks."
  (if (cl-safe-expr-p form)
      `(prog1 ,form (assert ,(cl-make-type-test form type) t))
    (let ((saved (gensym)))
      `(let ((,saved ,form))
        (prog1 ,saved (assert ,(cl-make-type-test saved type) t))))))

;;;###autoload
(defmacro declare (&rest specs)
  nil)

;;;###autoload
(defmacro load-time-value (form &optional read-only)
  "Evaluate FORM once at load time if byte-compiled.

The result of FORM is returned and stored for later access.  In
interpreted code, `load-time-value' is equivalent to `progn'."
  (list 'progn form))

;;;###autoload
(defmacro* labels (bindings &rest body &environment env)
  "Make temporary function bindings.

This is like `flet', except the bindings are lexical instead of dynamic.
Unlike `flet', this macro is compliant with the Common Lisp standard with
regard to the scope and extent of the function bindings.

Each function may be called from within FORM, from within the BODY of the
function itself (that is, recursively), and from any other function bodies
in FUNCTIONS.

Within FORM, to access the function definition of a bound function (for
example, to pass it as a FUNCTION argument to `map'), quote its symbol name
using `function'.

arguments: (((FUNCTION ARGLIST &body BODY) &rest FUNCTIONS) &body FORM)
"
  ;; XEmacs; the byte-compiler has a much better implementation of `labels'
  ;; in `byte-compile-initial-macro-environment' that is used in compiled
  ;; code.
  (let ((vars nil) (sets nil))
    (while bindings
      (let ((var (gensym)))
	(push var vars)
	(push `#'(lambda ,@(cdr (cl-transform-lambda (cdar bindings)
                                                     (caar bindings)))) sets)
	(push var sets)
	(push (list (car (pop bindings)) 'lambda '(&rest cl-labels-args)
		       (list 'list* '(quote funcall) (list 'quote var)
			     'cl-labels-args))
              env)))
    (cl-macroexpand-all `(lexical-let ,vars (setq ,@sets) ,@body) env)))

;;;###autoload
(defmacro flet (functions &rest form)
  "Make temporary function definitions.

This is an analogue of `let' that operates on the function cell of FUNC
rather than its value cell.  The FORMs are evaluated with the specified
function definitions in place, then the definitions are undone (the FUNCs go
back to their previous definitions, or lack thereof).  This is in
contravention of Common Lisp, where `flet' makes a lexical, not a dynamic,
function binding.

Normally you should use `labels', not `flet'; `labels' does not have the
problems caused by dynamic scope, is less expensive when byte-compiled, and
allows lexical shadowing of functions with byte-codes and byte-compile
methods, where `flet' will fail.  The byte-compiler will warn when this
happens.

If you need to shadow some existing function at run time, and that function
has no associated byte code or compiler macro, then `flet' is appropriate.

arguments: (((FUNCTION ARGLIST &body BODY) &rest FUNCTIONS) &body FORM)"
  ;; XEmacs; leave warnings, errors and modifications of
  ;; byte-compile-function-environment to the byte compiler. See
  ;; byte-compile-initial-macro-environment in bytecomp.el.
  (list*
   'letf*
   (mapcar
    (function*
     (lambda ((function . definition))
       `((symbol-function ',function) 
         ,(cons 'lambda (cdr (cl-transform-lambda definition function))))))
    functions) form))

(run-hooks 'cl-macs-load-hook)

;;; arch-tag: afd947a6-b553-4df1-bba5-000be6388f46
;;; cl-macs.el ends here
