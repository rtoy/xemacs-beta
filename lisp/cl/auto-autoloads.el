;;; DO NOT MODIFY THIS FILE
(if (featurep 'cl-autoloads) (error "Already loaded"))

;;;### (autoloads (compiler-macroexpand define-compiler-macro ignore-errors assert check-type typep deftype cl-struct-setf-expander defstruct define-modify-macro callf2 callf letf* letf rotatef shiftf remf cl-do-pop psetf setf get-setf-method defsetf define-setf-method declare the locally multiple-value-setq multiple-value-bind lexical-let* lexical-let symbol-macrolet macrolet labels flet progv psetq do-all-symbols do-symbols dotimes dolist do* do loop return-from return block etypecase typecase ecase case load-time-value eval-when destructuring-bind function* defmacro* defun* gentemp gensym cl-compile-time-init) "cl-macs" "cl/cl-macs.el")

(autoload 'cl-compile-time-init "cl-macs" nil nil nil)

(autoload 'gensym "cl-macs" "\
Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"." nil nil)

(autoload 'gentemp "cl-macs" "\
Generate a new interned symbol with a unique name.
The name is made by appending a number to PREFIX, default \"G\"." nil nil)

(autoload 'defun* "cl-macs" "\
(defun* NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.
Like normal `defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...)." nil 'macro)

(autoload 'defmacro* "cl-macs" "\
(defmacro* NAME ARGLIST [DOCSTRING] BODY...): define NAME as a macro.
Like normal `defmacro', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...)." nil 'macro)

(autoload 'function* "cl-macs" "\
(function* SYMBOL-OR-LAMBDA): introduce a function.
Like normal `function', except that if argument is a lambda form, its
ARGLIST allows full Common Lisp conventions." nil 'macro)

(autoload 'destructuring-bind "cl-macs" nil nil 'macro)

(autoload 'eval-when "cl-macs" "\
(eval-when (WHEN...) BODY...): control when BODY is evaluated.
If `compile' is in WHEN, BODY is evaluated when compiled at top-level.
If `load' is in WHEN, BODY is evaluated when loaded after top-level compile.
If `eval' is in WHEN, BODY is evaluated when interpreted or at non-top-level." nil 'macro)

(autoload 'load-time-value "cl-macs" "\
Like `progn', but evaluates the body at load time.
The result of the body appears to the compiler as a quoted constant." nil 'macro)

(autoload 'case "cl-macs" "\
(case EXPR CLAUSES...): evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and compared
against each key in each KEYLIST; the corresponding BODY is evaluated.
If no clause succeeds, case returns nil.  A single atom may be used in
place of a KEYLIST of one atom.  A KEYLIST of `t' or `otherwise' is
allowed only in the final clause, and matches if no other keys match.
Key values are compared by `eql'." nil 'macro)

(autoload 'ecase "cl-macs" "\
(ecase EXPR CLAUSES...): like `case', but error if no case fits.
`otherwise'-clauses are not allowed." nil 'macro)

(autoload 'typecase "cl-macs" "\
(typecase EXPR CLAUSES...): evals EXPR, chooses from CLAUSES on that value.
Each clause looks like (TYPE BODY...).  EXPR is evaluated and, if it
satisfies TYPE, the corresponding BODY is evaluated.  If no clause succeeds,
typecase returns nil.  A TYPE of `t' or `otherwise' is allowed only in the
final clause, and matches if no other keys match." nil 'macro)

(autoload 'etypecase "cl-macs" "\
(etypecase EXPR CLAUSES...): like `typecase', but error if no case fits.
`otherwise'-clauses are not allowed." nil 'macro)

(autoload 'block "cl-macs" "\
(block NAME BODY...): define a lexically-scoped block named NAME.
NAME may be any symbol.  Code inside the BODY forms can call `return-from'
to jump prematurely out of the block.  This differs from `catch' and `throw'
in two respects:  First, the NAME is an unevaluated symbol rather than a
quoted symbol or other form; and second, NAME is lexically rather than
dynamically scoped:  Only references to it within BODY will work.  These
references may appear inside macro expansions, but not inside functions
called from BODY." nil 'macro)

(autoload 'return "cl-macs" "\
(return [RESULT]): return from the block named nil.
This is equivalent to `(return-from nil RESULT)'." nil 'macro)

(autoload 'return-from "cl-macs" "\
(return-from NAME [RESULT]): return from the block named NAME.
This jump out to the innermost enclosing `(block NAME ...)' form,
returning RESULT from that form (or nil if RESULT is omitted).
This is compatible with Common Lisp, but note that `defun' and
`defmacro' do not create implicit blocks as they do in Common Lisp." nil 'macro)

(autoload 'loop "cl-macs" "\
(loop CLAUSE...): The Common Lisp `loop' macro.
Valid clauses are:
  for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM,
  for VAR in LIST by FUNC, for VAR on LIST by FUNC, for VAR = INIT then EXPR,
  for VAR across ARRAY, repeat NUM, with VAR = INIT, while COND, until COND,
  always COND, never COND, thereis COND, collect EXPR into VAR,
  append EXPR into VAR, nconc EXPR into VAR, sum EXPR into VAR,
  count EXPR into VAR, maximize EXPR into VAR, minimize EXPR into VAR,
  if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  do EXPRS..., initially EXPRS..., finally EXPRS..., return EXPR,
  finally return EXPR, named NAME." nil 'macro)

(autoload 'do "cl-macs" "\
The Common Lisp `do' loop.
Format is: (do ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)" nil 'macro)

(autoload 'do* "cl-macs" "\
The Common Lisp `do*' loop.
Format is: (do* ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)" nil 'macro)

(autoload 'dolist "cl-macs" "\
(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil." nil 'macro)

(autoload 'dotimes "cl-macs" "\
(dotimes (VAR COUNT [RESULT]) BODY...): loop a certain number of times.
Evaluate BODY with VAR bound to successive integers from 0, inclusive,
to COUNT, exclusive.  Then evaluate RESULT to get return value, default
nil." nil 'macro)

(autoload 'do-symbols "cl-macs" "\
(dosymbols (VAR [OBARRAY [RESULT]]) BODY...): loop over all symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY." nil 'macro)

(autoload 'do-all-symbols "cl-macs" nil nil 'macro)

(autoload 'psetq "cl-macs" "\
(psetq SYM VAL SYM VAL ...): set SYMs to the values VALs in parallel.
This is like `setq', except that all VAL forms are evaluated (in order)
before assigning any symbols SYM to the corresponding values." nil 'macro)

(autoload 'progv "cl-macs" "\
(progv SYMBOLS VALUES BODY...): bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each SYMBOL in the first list is bound to the corresponding VALUE in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time." nil 'macro)

(autoload 'flet "cl-macs" "\
(flet ((FUNC ARGLIST BODY...) ...) FORM...): make temporary function defns.
This is an analogue of `let' that operates on the function cell of FUNC
rather than its value cell.  The FORMs are evaluated with the specified
function definitions in place, then the definitions are undone (the FUNCs
go back to their previous definitions, or lack thereof)." nil 'macro)

(autoload 'labels "cl-macs" "\
(labels ((FUNC ARGLIST BODY...) ...) FORM...): make temporary func bindings.
This is like `flet', except the bindings are lexical instead of dynamic.
Unlike `flet', this macro is fully complaint with the Common Lisp standard." nil 'macro)

(autoload 'macrolet "cl-macs" "\
(macrolet ((NAME ARGLIST BODY...) ...) FORM...): make temporary macro defns.
This is like `flet', but for macros instead of functions." nil 'macro)

(autoload 'symbol-macrolet "cl-macs" "\
(symbol-macrolet ((NAME EXPANSION) ...) FORM...): make symbol macro defns.
Within the body FORMs, references to the variable NAME will be replaced
by EXPANSION, and (setq NAME ...) will act like (setf EXPANSION ...)." nil 'macro)

(autoload 'lexical-let "cl-macs" "\
(lexical-let BINDINGS BODY...): like `let', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp." nil 'macro)

(autoload 'lexical-let* "cl-macs" "\
(lexical-let* BINDINGS BODY...): like `let*', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp." nil 'macro)

(autoload 'multiple-value-bind "cl-macs" "\
(multiple-value-bind (SYM SYM...) FORM BODY): collect multiple return values.
FORM must return a list; the BODY is then executed with the first N elements
of this list bound (`let'-style) to each of the symbols SYM in turn.  This
is analogous to the Common Lisp `multiple-value-bind' macro, using lists to
simulate true multiple return values.  For compatibility, (values A B C) is
a synonym for (list A B C)." nil 'macro)

(autoload 'multiple-value-setq "cl-macs" "\
(multiple-value-setq (SYM SYM...) FORM): collect multiple return values.
FORM must return a list; the first N elements of this list are stored in
each of the symbols SYM in turn.  This is analogous to the Common Lisp
`multiple-value-setq' macro, using lists to simulate true multiple return
values.  For compatibility, (values A B C) is a synonym for (list A B C)." nil 'macro)

(autoload 'locally "cl-macs" nil nil 'macro)

(autoload 'the "cl-macs" nil nil 'macro)

(autoload 'declare "cl-macs" nil nil 'macro)

(autoload 'define-setf-method "cl-macs" "\
(define-setf-method NAME ARGLIST BODY...): define a `setf' method.
This method shows how to handle `setf's to places of the form (NAME ARGS...).
The argument forms ARGS are bound according to ARGLIST, as if NAME were
going to be expanded as a macro, then the BODY forms are executed and must
return a list of five elements: a temporary-variables list, a value-forms
list, a store-variables list (of length one), a store-form, and an access-
form.  See `defsetf' for a simpler way to define most setf-methods." nil 'macro)

(autoload 'defsetf "cl-macs" "\
(defsetf NAME FUNC): define a `setf' method.
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
Example: (defsetf nth (n x) (v) (list 'setcar (list 'nthcdr n x) v))." nil 'macro)

(autoload 'get-setf-method "cl-macs" "\
Return a list of five values describing the setf-method for PLACE.
PLACE may be any Lisp form which can appear as the PLACE argument to
a macro like `setf' or `incf'." nil nil)

(autoload 'setf "cl-macs" "\
(setf PLACE VAL PLACE VAL ...): set each PLACE to the value of its VAL.
This is a generalized version of `setq'; the PLACEs may be symbolic
references such as (car x) or (aref x i), as well as plain symbols.
For example, (setf (cadar x) y) is equivalent to (setcar (cdar x) y).
The return value is the last VAL in the list." nil 'macro)

(autoload 'psetf "cl-macs" "\
(psetf PLACE VAL PLACE VAL ...): set PLACEs to the values VALs in parallel.
This is like `setf', except that all VAL forms are evaluated (in order)
before assigning any PLACEs to the corresponding values." nil 'macro)

(autoload 'cl-do-pop "cl-macs" nil nil nil)

(autoload 'remf "cl-macs" "\
(remf PLACE TAG): remove TAG from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The form returns true if TAG was found and removed, nil otherwise." nil 'macro)

(autoload 'shiftf "cl-macs" "\
(shiftf PLACE PLACE... VAL): shift left among PLACEs.
Example: (shiftf A B C) sets A to B, B to C, and returns the old A.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'." nil 'macro)

(autoload 'rotatef "cl-macs" "\
(rotatef PLACE...): rotate left among PLACEs.
Example: (rotatef A B C) sets A to B, B to C, and C to A.  It returns nil.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'." nil 'macro)

(autoload 'letf "cl-macs" "\
(letf ((PLACE VALUE) ...) BODY...): temporarily bind to PLACEs.
This is the analogue of `let', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY." nil 'macro)

(autoload 'letf* "cl-macs" "\
(letf* ((PLACE VALUE) ...) BODY...): temporarily bind to PLACEs.
This is the analogue of `let*', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY." nil 'macro)

(autoload 'callf "cl-macs" "\
(callf FUNC PLACE ARGS...): set PLACE to (FUNC PLACE ARGS...).
FUNC should be an unquoted function name.  PLACE may be a symbol,
or any generalized variable allowed by `setf'." nil 'macro)

(autoload 'callf2 "cl-macs" "\
(callf2 FUNC ARG1 PLACE ARGS...): set PLACE to (FUNC ARG1 PLACE ARGS...).
Like `callf', but PLACE is the second argument of FUNC, not the first." nil 'macro)

(autoload 'define-modify-macro "cl-macs" "\
(define-modify-macro NAME ARGLIST FUNC): define a `setf'-like modify macro.
If NAME is called, it combines its PLACE argument with the other arguments
from ARGLIST using FUNC: (define-modify-macro incf (&optional (n 1)) +)" nil 'macro)

(autoload 'defstruct "cl-macs" "\
(defstruct (NAME OPTIONS...) (SLOT SLOT-OPTS...)...): define a struct type.
This macro defines a new Lisp data type called NAME, which contains data
stored in SLOTs.  This defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and setf-able `NAME-SLOT' accessors." nil 'macro)

(autoload 'cl-struct-setf-expander "cl-macs" nil nil nil)

(autoload 'deftype "cl-macs" "\
(deftype NAME ARGLIST BODY...): define NAME as a new data type.
The type name can then be used in `typecase', `check-type', etc." nil 'macro)

(autoload 'typep "cl-macs" "\
Check that OBJECT is of type TYPE.
TYPE is a Common Lisp-style type specifier." nil nil)

(autoload 'check-type "cl-macs" "\
Verify that FORM is of type TYPE; signal an error if not.
STRING is an optional description of the desired type." nil 'macro)

(autoload 'assert "cl-macs" "\
Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used." nil 'macro)

(autoload 'ignore-errors "cl-macs" "\
Execute FORMS; if an error occurs, return nil.
Otherwise, return result of last FORM." nil 'macro)

(autoload 'define-compiler-macro "cl-macs" "\
(define-compiler-macro FUNC ARGLIST BODY...): Define a compiler-only macro.
This is like `defmacro', but macro expansion occurs only if the call to
FUNC is compiled (i.e., not interpreted).  Compiler macros should be used
for optimizing the way calls to FUNC are compiled; the form returned by
BODY should do the same thing as a call to the normal function called
FUNC, though possibly more efficiently.  Note that, like regular macros,
compiler macros are expanded repeatedly until no further expansions are
possible.  Unlike regular macros, BODY can decide to \"punt\" and leave the
original function call alone by declaring an initial `&whole foo' parameter
and then returning foo." nil 'macro)

(autoload 'compiler-macroexpand "cl-macs" nil nil nil)

;;;***

(provide 'cl-autoloads)
