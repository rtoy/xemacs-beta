;;; DO NOT MODIFY THIS FILE
(if (featurep 'TopLevel-autoloads) (error "Already loaded"))

;;;### (autoloads (batch-byte-recompile-directory batch-byte-recompile-directory-norecurse batch-byte-compile display-call-tree byte-compile-sexp byte-compile compile-defun byte-compile-file byte-recompile-file byte-recompile-directory byte-force-recompile) "bytecomp" "lisp/bytecomp.el")

(autoload 'byte-force-recompile "bytecomp" "\
Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also." t nil)

(autoload 'byte-recompile-directory "bytecomp" "\
Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also unless argument
NORECURSION is non-nil.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it.  Prefix argument 0 means
don't ask and compile the file anyway.

A nonzero prefix argument also means ask about each subdirectory.

If the fourth argument FORCE is non-nil,
recompile every `.el' file that already has a `.elc' file." t nil)

(autoload 'byte-recompile-file "bytecomp" "\
Recompile a file of Lisp code named FILENAME if it needs recompilation.
This is if the `.elc' file exists but is older than the `.el' file.

If the `.elc' file does not exist, normally the `.el' file is *not*
compiled.  But a prefix argument (optional second arg) means ask user
whether to compile it.  Prefix argument 0 don't ask and recompile anyway." t nil)

(autoload 'byte-compile-file "bytecomp" "\
Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending `c' to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), load the file after compiling." t nil)

(autoload 'compile-defun "bytecomp" "\
Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form." t nil)

(autoload 'byte-compile "bytecomp" "\
If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function." nil nil)

(autoload 'byte-compile-sexp "bytecomp" "\
Compile and return SEXP." nil nil)

(autoload 'display-call-tree "bytecomp" "\
Display a call graph of a specified file.
This lists which functions have been called, what functions called
them, and what functions they call.  The list includes all functions
whose definitions have been compiled in this Emacs session, as well as
all functions called by those functions.

The call graph does not include macros, inline functions, or
primitives that the byte-code interpreter knows about directly (eq,
cons, etc.).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled), and which cannot be
invoked interactively." t nil)

(autoload 'batch-byte-compile "bytecomp" "\
Run `byte-compile-file' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\"" nil nil)

(autoload 'batch-byte-recompile-directory-norecurse "bytecomp" "\
Same as `batch-byte-recompile-directory' but without recursion." nil nil)

(autoload 'batch-byte-recompile-directory "bytecomp" "\
Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `xemacs -batch -f batch-byte-recompile-directory .'." nil nil)

;;;***

;;;### (autoloads (compiler-macroexpand define-compiler-macro ignore-errors assert check-type typep deftype cl-struct-setf-expander defstruct define-modify-macro callf2 callf letf* letf rotatef shiftf remf cl-do-pop psetf setf get-setf-method defsetf define-setf-method declare the locally multiple-value-setq multiple-value-bind lexical-let* lexical-let symbol-macrolet macrolet labels flet progv psetq do-all-symbols do-symbols dotimes dolist do* do loop return-from return block etypecase typecase ecase case load-time-value eval-when destructuring-bind function* defmacro* defun* gentemp gensym cl-compile-time-init) "cl-macs" "lisp/cl-macs.el")

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

;;;### (autoloads (Custom-make-dependencies) "cus-dep" "lisp/cus-dep.el")

(autoload 'Custom-make-dependencies "cus-dep" "\
Extract custom dependencies from .el files in SUBDIRS.
SUBDIRS is a list of directories.  If it is nil, the command-line
arguments are used.  If it is a string, only that directory is
processed.  This function is especially useful in batch mode.

Batch usage: xemacs -batch -l cus-dep.el -f Custom-make-dependencies DIRS" t nil)

;;;***

;;;### (autoloads (customize-menu-create custom-menu-create custom-save-all customize-save-customized customize-browse custom-buffer-create-other-window custom-buffer-create customize-apropos-groups customize-apropos-faces customize-apropos-options customize-apropos customize-saved customize-customized customize-face-other-window customize-face customize-option-other-window customize-variable customize-other-window customize customize-save-variable customize-set-variable customize-set-value) "cus-edit" "lisp/cus-edit.el")

(autoload 'customize-set-value "cus-edit" "\
Set VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value." t nil)

(autoload 'customize-set-variable "cus-edit" "\
Set the default for VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload 'customize-save-variable "cus-edit" "\
Set the default for VARIABLE to VALUE, and save it for future sessions.
If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload 'customize "cus-edit" "\
Select a customization buffer which you can use to set user options.
User options are structured into \"groups\".
The default group is `Emacs'." t nil)

(defalias 'customize-group 'customize)

(autoload 'customize-other-window "cus-edit" "\
Customize SYMBOL, which must be a customization group." t nil)

(defalias 'customize-group-other-window 'customize-other-window)

(defalias 'customize-option 'customize-variable)

(autoload 'customize-variable "cus-edit" "\
Customize SYMBOL, which must be a user option variable." t nil)

(defalias 'customize-variable-other-window 'customize-option-other-window)

(autoload 'customize-option-other-window "cus-edit" "\
Customize SYMBOL, which must be a user option variable.
Show the buffer in another window, but don't select it." t nil)

(autoload 'customize-face "cus-edit" "\
Customize SYMBOL, which should be a face name or nil.
If SYMBOL is nil, customize all faces." t nil)

(autoload 'customize-face-other-window "cus-edit" "\
Show customization buffer for FACE in other window." t nil)

(autoload 'customize-customized "cus-edit" "\
Customize all user options set since the last save in this session." t nil)

(autoload 'customize-saved "cus-edit" "\
Customize all already saved user options." t nil)

(autoload 'customize-apropos "cus-edit" "\
Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which are not
user-settable, as well as faces and groups." t nil)

(autoload 'customize-apropos-options "cus-edit" "\
Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable." t nil)

(autoload 'customize-apropos-faces "cus-edit" "\
Customize all user faces matching REGEXP." t nil)

(autoload 'customize-apropos-groups "cus-edit" "\
Customize all user groups matching REGEXP." t nil)

(autoload 'custom-buffer-create "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload 'custom-buffer-create-other-window "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload 'customize-browse "cus-edit" "\
Create a tree browser for the customize hierarchy." t nil)

(defcustom custom-file (if (boundp 'emacs-user-extension-dir) (concat "~" init-file-user emacs-user-extension-dir "options.el") "~/.emacs") "File used for storing customization information.\nIf you change this from the default \"~/.emacs\" you need to\nexplicitly load that file for the settings to take effect." :type 'file :group 'customize)

(autoload 'customize-save-customized "cus-edit" "\
Save all user options which have been set in this session." t nil)

(autoload 'custom-save-all "cus-edit" "\
Save all customizations in `custom-file'." nil nil)

(autoload 'custom-menu-create "cus-edit" "\
Create menu for customization group SYMBOL.
The menu is in a format applicable to `easy-menu-define'." nil nil)

(autoload 'customize-menu-create "cus-edit" "\
Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu.
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'." nil nil)

;;;***

;;;### (autoloads (custom-set-faces custom-declare-face) "cus-face" "lisp/cus-face.el")

(autoload 'custom-declare-face "cus-face" "\
Like `defface', but FACE is evaluated as a normal argument." nil nil)

(autoload 'custom-set-faces "cus-face" "\
Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC will be stored as the saved value for FACE.  If NOW is present
and non-nil, FACE will also be created according to SPEC.

See `defface' for the format of SPEC." nil nil)

;;;***

;;;### (autoloads (disassemble) "disass" "lisp/disass.el")

(autoload 'disassemble "disass" "\
Print disassembled code for OBJECT in (optional) BUFFER.
OBJECT can be a symbol defined as a function, or a function itself
\(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol." t nil)

;;;***

;;;### (autoloads (widget-minor-mode widget-browse-other-window widget-browse widget-browse-at) "wid-browse" "lisp/wid-browse.el")

(autoload 'widget-browse-at "wid-browse" "\
Browse the widget under point." t nil)

(autoload 'widget-browse "wid-browse" "\
Create a widget browser for WIDGET." t nil)

(autoload 'widget-browse-other-window "wid-browse" "\
Show widget browser for WIDGET in other window." t nil)

(autoload 'widget-minor-mode "wid-browse" "\
Togle minor mode for traversing widgets.
With arg, turn widget mode on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (widget-delete widget-create widget-prompt-value) "wid-edit" "lisp/wid-edit.el")

(autoload 'widget-prompt-value "wid-edit" "\
Prompt for a value matching WIDGET, using PROMPT.
The current value is assumed to be VALUE, unless UNBOUND is non-nil." nil nil)

(autoload 'widget-create "wid-edit" "\
Create widget of TYPE.
The optional ARGS are additional keyword arguments." nil nil)

(autoload 'widget-delete "wid-edit" "\
Delete WIDGET." nil nil)

;;;***

;;;### (autoloads (font-menu-weight-constructor font-menu-size-constructor font-menu-family-constructor reset-device-font-menus) "x-font-menu" "lisp/x-font-menu.el")

(defcustom font-menu-ignore-scaled-fonts t "*If non-nil, then the font menu will try to show only bitmap fonts." :type 'boolean :group 'x)

(defcustom font-menu-this-frame-only-p nil "*If non-nil, then changing the default font from the font menu will only\naffect one frame instead of all frames." :type 'boolean :group 'x)

(fset 'install-font-menus 'reset-device-font-menus)

(autoload 'reset-device-font-menus "x-font-menu" "\
Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system, 
or if you change your font path, you can call this to re-initialize the menus." nil nil)

(autoload 'font-menu-family-constructor "x-font-menu" nil nil nil)

(autoload 'font-menu-size-constructor "x-font-menu" nil nil nil)

(autoload 'font-menu-weight-constructor "x-font-menu" nil nil nil)

;;;***

(provide 'TopLevel-autoloads)
