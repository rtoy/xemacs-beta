;;; byte-run.el --- byte-compiler support for inlining

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Keywords: internal

;; The code in this file should always be loaded, because it defines things 
;; like "defsubst" which should work interpreted as well.  The code in 
;; bytecomp.el and byte-optimize.el can be loaded as needed.
;;
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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:

;;; interface to selectively inlining functions.
;;; This only happens when source-code optimization is turned on.

;;; Code:

;; Redefined in byte-optimize.el.
;; This is not documented--it's not clear that we should promote it.
(fset 'inline 'progn)
(put 'inline 'lisp-indent-hook 0)


;;; Interface to inline functions.

;; FSF comments the next two out, but I see no reason to do so. --ben
(defmacro proclaim-inline (&rest fns)
  "Cause the named functions to be open-coded when called from compiled code.
They will only be compiled open-coded when byte-optimize is true."
  (cons 'eval-and-compile
	(apply
	 'nconc
	 (mapcar
	  '(lambda (x)
	     (` ((or (memq (get '(, x) 'byte-optimizer)
			   '(nil byte-compile-inline-expand))
		     (error
		      "%s already has a byte-optimizer, can't make it inline"
		      '(, x)))
		 (put '(, x) 'byte-optimizer 'byte-compile-inline-expand))))
	  fns))))


(defmacro proclaim-notinline (&rest fns)
  "Cause the named functions to no longer be open-coded."
  (cons 'eval-and-compile
	(apply
	 'nconc
	 (mapcar
	  '(lambda (x)
	     (` ((if (eq (get '(, x) 'byte-optimizer)
			 'byte-compile-inline-expand)
		     (put '(, x) 'byte-optimizer nil)))))
	  fns))))

;; This has a special byte-hunk-handler in bytecomp.el.
(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'."
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  (list 'prog1
	(cons 'defun (cons name (cons arglist body)))
	(list 'proclaim-inline name)))
; Instead of the above line, FSF has this:
;	(list 'eval-and-compile
;	      (list 'put (list 'quote name)
;		    ''byte-optimizer ''byte-compile-inline-expand))))

(defun make-obsolete (fn new)
  "Make the byte-compiler warn that FUNCTION is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (let ((handler (get fn 'byte-compile)))
    (if (eq 'byte-compile-obsolete handler)
	(setcar (get fn 'byte-obsolete-info) new)
      (put fn 'byte-obsolete-info (cons new handler))
      (put fn 'byte-compile 'byte-compile-obsolete)))
  fn)

(defun make-obsolete-variable (var new)
  "Make the byte-compiler warn that VARIABLE is obsolete,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message."
  (interactive
   (list
    (let ((str (completing-read "Make variable obsolete: " obarray 'boundp t)))
      (if (equal str "") (error ""))
      (intern str))
    (car (read-from-string (read-string "Obsoletion replacement: ")))))
  (put var 'byte-obsolete-variable new)
  var)

(put 'dont-compile 'lisp-indent-hook 0)
(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;;; interface to evaluating things at compile time and/or load time
;;; these macro must come after any uses of them in this file, as their
;;; definition in the file overrides the magic definitions on the
;;; byte-compile-macro-environment.

(put 'eval-when-compile 'lisp-indent-hook 0)
(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant."
  ;; Not necessary because we have it in b-c-initial-macro-environment
  ;; (list 'quote (eval (cons 'progn body)))
  (cons 'progn body))

(put 'eval-and-compile 'lisp-indent-hook 0)
(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time."
  ;; Remember, it's magic.
  (cons 'progn body))


;;; Interface to file-local byte-compiler parameters.
;;; Redefined in bytecomp.el.

;;; The great RMS speaketh:
;;;
;;; I nuked this because it's not a good idea for users to think of
;;; using it.  These options are a matter of installation preference,
;;; and have nothing to do with particular source files; it's a
;;; mistake to suggest to users that they should associate these with
;;; particular source files.  There is hardly any reason to change
;;; these parameters, anyway.  --rms.
;;;
;;; But I'll leave this stuff alone. --ben

(put 'byte-compiler-options 'lisp-indent-hook 0)
(defmacro byte-compiler-options (&rest args)
  "Set some compilation-parameters for this file.  
This will affect only the file in which it appears; this does nothing when
evaluated, or when loaded from a .el file.

Each argument to this macro must be a list of a key and a value.

  Keys:		  Values:		Corresponding variable:

  verbose	  t, nil		byte-compile-verbose
  optimize	  t, nil, source, byte	byte-optimize
  warnings	  list of warnings	byte-compile-warnings
  file-format	  emacs18, emacs19	byte-compile-emacs18-compatibility

The value specificed with the `warnings' option must be a list, containing
some subset of the following flags:

  free-vars	references to variables not in the current lexical scope.
  unused-vars	references to non-global variables bound but not referenced.
  unresolved	calls to unknown functions.
  callargs	lambda calls with args that don't match the definition.
  redefine	function cell redefined from a macro to a lambda or vice
		versa, or redefined to take a different number of arguments.

If the first element if the list is `+' or `-' then the specified elements 
are added to or removed from the current set of warnings, instead of the
entire set of warnings being overwritten.

For example, something like this might appear at the top of a source file:

    (byte-compiler-options
      (optimize t)
      (warnings (- callargs))		; Don't warn about arglist mismatch
      (warnings (+ unused-vars))	; Do warn about unused bindings
      (file-format emacs19))"
  nil)

;;; bytecomp-runtime.el ends here
