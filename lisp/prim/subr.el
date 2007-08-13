;;; subr.el --- basic lisp subroutines for XEmacs

;;; Copyright (C) 1985, 1986, 1992, 1994, 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;;; Copyright (C) 1995 Sun Microsystems.

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; There's not a whole lot in common now with the FSF version,
;; be wary when applying differences.  I've left in a number of lines
;; of commentary just to give diff(1) something to synch itself with to
;; provide useful context diffs. -sb

;;; Code:


;;;; Lisp language features.

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of lisp expressions."
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  ;; #### - I don't see why.  So long as backquote.el doesn't use anything
  ;; from subr.el, there's no problem with using backquotes here.  --Stig 
  ;;(list 'function (cons 'lambda cdr)))
  `(function (lambda ,@cdr)))

(defmacro defun-when-void (&rest args)
  "Define a function, just like `defun', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ',(car args))
       nil
     (defun ,@args)))

(defmacro define-function-when-void (&rest args)
  "Define a function, just like `define-function', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ,(car args))
       nil
     (define-function ,@args)))


;;;; Keymap support.
;; XEmacs: removed to keymap.el

;;;; The global keymap tree.  

;;; global-map, esc-map, and ctl-x-map have their values set up in
;;; keymap.c; we just give them docstrings here.

;;;; Event manipulation functions.

;; The call to `read' is to ensure that the value is computed at load time
;; and not compiled into the .elc file.  The value is negative on most
;; machines, but not on all!
;; XEmacs: This stuff is done in C Code.

;;;; Obsolescent names for functions.
;; XEmacs: not used.

;; XEmacs:
(define-function 'not 'null)
(define-function-when-void 'numberp 'integerp) ; different when floats

(defun local-variable-if-set-p (sym buffer)
  "Return t if SYM would be local to BUFFER after it is set.
A nil value for BUFFER is *not* the same as (current-buffer), but
can be used to determine whether `make-variable-buffer-local' has been
called on SYM."
  (local-variable-p sym buffer t))


;;;; Hook manipulation functions.

;; (defconst run-hooks 'run-hooks ...)

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making `t' a member of the buffer-local value,
which acts as a flag to run the hook functions in the default value as
well.  This works for all normal hooks, but does not work for most
non-normal hooks yet.  We will be changing the callers of non-normal
hooks so that they can handle localness; this has to be done one by
one.

This function does nothing if HOOK is already local in the current
buffer.

Do not use `make-local-variable' to make a hook variable buffer-local."
  (if (local-variable-p hook (current-buffer)) ; XEmacs
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t))))

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (if (or local
	  ;; Detect the case where make-local-variable was used on a hook
	  ;; and do what we used to do.
	  (and (local-variable-if-set-p hook (current-buffer)) ; XEmacs
	       (not (memq t (symbol-value hook)))))
      ;; Alter the local value only.
      (or (if (consp function)
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook 
	       (if append
		   (append (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))
    ;; Alter the global value (which is also the only value,
    ;; if the hook doesn't have a local value).
    (or (if (consp function)
	    (member function (default-value hook))
	  (memq function (default-value hook)))
	(set-default hook 
		     (if append
			 (append (default-value hook) (list function))
		       (cons function (default-value hook)))))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes no difference if the hook is not buffer-local.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (not (default-boundp 'hook))
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (if (or local
	    ;; Detect the case where make-local-variable was used on a hook
	    ;; and do what we used to do.
	    (and (local-variable-p hook (current-buffer))
		 (not (memq t (symbol-value hook)))))
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (if (member function hook-value)
		  (setq hook-value (delete function (copy-sequence hook-value))))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value))
      (let ((hook-value (default-value hook)))
	(if (consp hook-value)
	    (if (member function hook-value)
		(setq hook-value (delete function (copy-sequence hook-value))))
	  (if (equal hook-value function)
	      (setq hook-value nil)))
	(set-default hook hook-value)))))

(defun add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var)))))

;; XEmacs additions
;; called by Fkill_buffer()
(defvar kill-buffer-hook nil
  "Function or functions to be called when a buffer is killed.
The value of this variable may be buffer-local.
The buffer about to be killed is current when this hook is run.")

;; in C in FSFmacs
(defvar kill-emacs-hook nil
  "Function or functions to be called when `kill-emacs' is called,
just before emacs is actually killed.")

;; not obsolete.
(define-function 'rplaca 'setcar)
(define-function 'rplacd 'setcdr)

;; XEmacs
(defun mapvector (__function __seq)
  "Apply FUNCTION to each element of SEQ, making a vector of the results.
The result is a vector of the same length as SEQ.
SEQ may be a list, a vector or a string."
  (let* ((len (length __seq))
	 (vec (make-vector len 'nil))
	 (i 0))
    (while (< i len)
      (aset vec i (funcall __function (cond ((listp __seq)
					     (nth i __seq))
					    (t (aref __seq i)))))
      (setq i (+ i 1)))
    vec))

;;;; String functions.

;; XEmacs
(defun replace-in-string (str regexp newtext &optional literal)
  "Replaces all matches in STR for REGEXP with NEWTEXT string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (if (not (stringp str))
      (error "(replace-in-string): First argument must be a string: %s" str))
  (if (stringp newtext)
      nil
    (error "(replace-in-string): 3rd arg must be a string: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(replace-in-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

(defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))

(defmacro with-output-to-string (&rest forms)
  "Collect output to `standard-output' while evaluating FORMS and return
it as a string."
  ;; by "William G. Dubuque" <wgd@zurich.ai.mit.edu> w/ mods from Stig
  (` (save-excursion
       (set-buffer (get-buffer-create " *string-output*"))
       (setq buffer-read-only nil)
       (buffer-disable-undo (current-buffer))
       (erase-buffer)
       (let ((standard-output (current-buffer)))
	 (,@ forms))
       (prog1
	   (buffer-string)
	 (erase-buffer)))))

(defmacro with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
	   (save-excursion
	     (set-buffer ,temp-buffer)
	     ,@forms)
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

;; Moved from mule-coding.el.
(defmacro with-string-as-buffer-contents (str &rest body)
  "With the contents of the current buffer being STR, run BODY.
Returns the new contents of the buffer, as modified by BODY.
The original current buffer is restored afterwards."
  `(let ((curbuf (current-buffer))
         (tempbuf (get-buffer-create " *string-as-buffer-contents*")))
     (unwind-protect
         (progn
           (set-buffer tempbuf)
           (buffer-disable-undo (current-buffer))
           (erase-buffer)
           (insert ,str)
           ,@body
           (buffer-string))
       (erase-buffer tempbuf)
       (set-buffer curbuf))))

(defun insert-face (string face)
  "Insert STRING and highlight with FACE.  Returns the extent created."
  (let ((p (point)) ext)
    (insert string)
    (setq ext (make-extent p (point)))
    (set-extent-face ext face)
    ext))

;; not obsolete.
(define-function 'string= 'string-equal)
(define-function 'string< 'string-lessp)
(define-function 'int-to-string 'number-to-string)
(define-function 'string-to-int 'string-to-number)

;; alist/plist functions
(defun plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is not modified.  See also `destructive-plist-to-alist'."
  (let (alist)
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun destructive-plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is destroyed in the process of constructing the alist.
See also `plist-to-alist'."
  (let ((head plist)
	next)
    (while plist
      ;; remember the next plist pair.
      (setq next (cddr plist))
      ;; make the cons holding the property value into the alist element.
      (setcdr (cdr plist) (cadr plist))
      (setcar (cdr plist) (car plist))
      ;; reattach into alist form.
      (setcar plist (cdr plist))
      (setcdr plist next)
      (setq plist next))
    head))

(defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

;; getf, remf in cl*.el.

(defmacro putf (plist prop val)
  "Add property PROP to plist PLIST with value VAL.
Analogous to (setq PLIST (plist-put PLIST PROP VAL))."
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defmacro laxputf (lax-plist prop val)
  "Add property PROP to lax plist LAX-PLIST with value VAL.
Analogous to (setq LAX-PLIST (lax-plist-put LAX-PLIST PROP VAL))."
  `(setq ,lax-plist (lax-plist-put ,lax-plist ,prop ,val)))

(defmacro laxremf (lax-plist prop)
  "Remove property PROP from lax plist LAX-PLIST.
Analogous to (setq LAX-PLIST (lax-plist-remprop LAX-PLIST PROP))."
  `(setq ,lax-plist (lax-plist-remprop ,lax-plist ,prop)))

;;; Error functions

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'.
This error is not continuable: you cannot continue execution after the
error using the debugger `r' command.  See also `cerror'."
  (while t
    (apply 'cerror args)))

(defun cerror (&rest args)
  "Like `error' but signals a continuable error."
  (signal 'error (list (apply 'format args))))

(defmacro check-argument-type (predicate argument)
  "Check that ARGUMENT satisfies PREDICATE.
If not, signal a continuable `wrong-type-argument' error until the
returned value satisfies PREDICATE, and assign the returned value
to ARGUMENT."
  `(if (not (,(eval predicate) ,argument))
       (setq ,argument
	     (wrong-type-argument ,predicate ,argument))))

(defun signal-error (error-symbol data)
  "Signal a non-continuable error.  Args are ERROR-SYMBOL, and associated DATA.
An error symbol is a symbol defined using `define-error'.
DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See also `signal', and the functions to handle errors: `condition-case'
and `call-with-condition-handler'."
  (while t
    (signal error-symbol data)))

(defun define-error (error-sym doc-string &optional inherits-from)
  "Define a new error, denoted by ERROR-SYM.
DOC-STRING is an informative message explaining the error, and will be
printed out when an unhandled error occurs.
ERROR-SYM is a sub-error of INHERITS-FROM (which defaults to `error').

\[`define-error' internally works by putting on ERROR-SYM an `error-message'
property whose value is DOC-STRING, and an `error-conditions' property
that is a list of ERROR-SYM followed by each of its super-errors, up
to and including `error'.  You will sometimes see code that sets this up
directly rather than calling `define-error', but you should *not* do this
yourself.]"
  (check-argument-type 'symbolp error-sym)
  (check-argument-type 'stringp doc-string)
  (put error-sym 'error-message doc-string)
  (or inherits-from (setq inherits-from 'error))
  (let ((conds (get inherits-from 'error-conditions)))
    (or conds (signal-error 'error (list "Not an error symbol" error-sym)))
    (put error-sym 'error-conditions (cons error-sym conds))))

;;;; Miscellanea.

(defun buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without extents, as a string."
  (let ((string (buffer-substring beg end)))
    (map-extents (lambda (ext &rest junk)
		   (delete-extent ext))
		 string)
    string))

(defun ignore (&rest ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

(defmacro save-current-buffer (&rest forms)
  "Restore the current buffer setting after executing FORMS.
Does not restore the values of point and mark.
See also: `save-excursion'."
  ;; by Stig@hackvan.com
  (` (let ((_cur_buf_ (current-buffer)))
       (unwind-protect
	   (progn (,@ forms))
	 (set-buffer _cur_buf_)))))

(defmacro eval-in-buffer (buffer &rest forms)
  "Evaluate FORMS in BUFFER.
See also: `save-current-buffer' and `save-excursion'."
  ;; by Stig@hackvan.com
  (` (save-current-buffer
      (set-buffer (, buffer))
      (,@ forms))))

;;; The real defn is in abbrev.el but some early callers
;;;  (eg lisp-mode-abbrev-table) want this before abbrev.el is loaded...

(if (not (fboundp 'define-abbrev-table))
    (progn
      (setq abbrev-table-name-list '())
      (fset 'define-abbrev-table (function (lambda (name defs)
                                   ;; These are fixed-up when abbrev.el loads.
                                   (setq abbrev-table-name-list
                                         (cons (cons name defs)
                                               abbrev-table-name-list)))))))

(defun functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond
   ((symbolp obj) (fboundp obj))
   ((subrp obj))
   ((compiled-function-p obj))
   ((consp obj)
    (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
   (t nil)))

;; This was not present before.  I think Jamie had some objections
;; to this, so I'm leaving this undefined for now. --ben

;;; The objection is this: there is more than one way to load the same file.
;;; "foo", "foo.elc", "foo.el", and "/some/path/foo.elc" are all different
;;; ways to load the exact same code.  `eval-after-load' is too stupid to
;;; deal with this sort of thing.  If this sort of feature is desired, then
;;; it should work off of a hook on `provide'.  Features are unique and
;;; the arguments to (load) are not.  --Stig

;;;; Specifying things to do after certain files are loaded.

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
  ;; Make sure there is an element for FILE.
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  ;; Add FORM to the element if it isn't there.
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
	(progn
	  (nconc elt (list form))
	  ;; If the file has been loaded already, run FORM right away.
	  (and (assoc file load-history)
	       (eval form)))))
  form)
(make-compatible 'eval-after-load "")

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))
(make-compatible 'eval-next-after-load "")

; alternate names (not obsolete)
(if (not (fboundp 'mod)) (define-function 'mod '%))
(define-function 'move-marker 'set-marker)
(define-function 'beep 'ding)  ; preserve lingual purity
(define-function 'indent-to-column 'indent-to)
(define-function 'backward-delete-char 'delete-backward-char)
(define-function 'search-forward-regexp (symbol-function 're-search-forward))
(define-function 'search-backward-regexp (symbol-function 're-search-backward))
(define-function 'remove-directory 'delete-directory)
(define-function 'set-match-data 'store-match-data)
(define-function 'send-string-to-terminal 'external-debugging-output)
(define-function 'buffer-string 'buffer-substring)

;;; subr.el ends here
