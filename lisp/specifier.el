;;; specifier.el --- Lisp interface to specifiers

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 2000, 2002 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Keywords: internal, dumped

;;; Synched up with: Not in FSF.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

(defun make-specifier-and-init (type spec-list &optional dont-canonicalize)
  "Create and initialize a specifier of type TYPE with spec(s) SPEC-LIST.

A convenience API combining `make-specifier' and `set-specifier', allowing you
to create a specifier and add specs to it at the same time.
TYPE specifies the specifier type.  See `make-specifier' for known types.
SPEC-LIST supplies the specification(s) to be added to the specifier, in any
  form acceptable to `canonicalize-spec-list'.
Optional DONT-CANONICALIZE, if non-nil, inhibits the conversion, and the
  SPEC-LIST must already be in full form."
  (let ((sp (make-specifier type)))
    (if (not dont-canonicalize)
	(setq spec-list (canonicalize-spec-list spec-list type)))
    (add-spec-list-to-specifier sp spec-list)
    sp))

;; God damn, do I hate dynamic scoping.

(defun map-specifier (ms-specifier ms-func &optional ms-locale ms-maparg
		      ms-tag-set ms-exact-p)
  "Apply MS-FUNC to the specification(s) for MS-LOCALE in MS-SPECIFIER.

If optional MS-LOCALE is a locale, MS-FUNC will be called for that locale.
If MS-LOCALE is a locale type, MS-FUNC will be mapped over all locales of that
type.  If MS-LOCALE is 'all or nil, MS-FUNC will be mapped over all locales in
MS-SPECIFIER.

Optional MS-TAG-SET and MS-EXACT-P are as in `specifier-spec-list'.
Optional MS-MAPARG will be passed to MS-FUNC.

MS-FUNC is called with four arguments: the MS-SPECIFIER, the locale
being mapped over, the inst-list for that locale, and the
optional MS-MAPARG.  If any invocation of MS-FUNC returns non-nil,
the mapping will stop and the returned value becomes the
value returned from `map-specifier'.  Otherwise, `map-specifier'
returns nil."
  (let ((ms-specs (specifier-spec-list ms-specifier ms-locale ms-tag-set
				       ms-exact-p))
	ms-result)
    (while (and ms-specs (not ms-result))
      (let ((ms-this-spec (car ms-specs)))
	(setq ms-result (funcall ms-func ms-specifier (car ms-this-spec)
			      (cdr ms-this-spec) ms-maparg))
	(setq ms-specs (cdr ms-specs))))
    ms-result))

(defun canonicalize-inst-pair (inst-pair specifier-type &optional noerror)
  "Canonicalize the given INST-PAIR.

SPECIFIER-TYPE specifies the type of specifier that this SPEC-LIST
will be used for.

Canonicalizing means converting to the full form for an inst-pair, i.e.
`(TAG-SET . INSTANTIATOR)'.  A single, untagged instantiator is given
a tag set of nil (the empty set), and a single tag is converted into
a tag set consisting only of that tag.

If NOERROR is non-nil, signal an error if the inst-pair is invalid;
otherwise return t."
  ;; OK, the possibilities are:
  ;;
  ;; a) a single instantiator
  ;; b) a cons of a tag and an instantiator
  ;; c) a cons of a tag set and an instantiator
  (cond ((valid-instantiator-p inst-pair specifier-type)
	 ;; case (a)
	 (cons nil inst-pair))

	((not (consp inst-pair))
	 ;; not an inst-pair
	 (if noerror t
	   ;; this will signal an appropriate error.
	   (check-valid-instantiator inst-pair specifier-type)))

	((and (valid-specifier-tag-p (car inst-pair))
	      (valid-instantiator-p (cdr inst-pair) specifier-type))
	 ;; case (b)
	 (cons (list (car inst-pair)) (cdr inst-pair)))

	((and (valid-specifier-tag-set-p (car inst-pair))
	      (valid-instantiator-p (cdr inst-pair) specifier-type))
	 ;; case (c)
	 inst-pair)
	 
	(t
	 (if noerror t
	   (signal 'error (list "Invalid specifier tag set"
				(car inst-pair)))))))

(defun canonicalize-inst-list (inst-list specifier-type &optional noerror)
  "Canonicalize the given INST-LIST (a list of inst-pairs).

SPECIFIER-TYPE specifies the type of specifier that this INST-LIST
will be used for.

Canonicalizing means converting to the full form for an inst-list, i.e.
`((TAG-SET . INSTANTIATOR) ...)'.  This function accepts a single
inst-pair or any abbreviation thereof or a list of (possibly
abbreviated) inst-pairs. (See `canonicalize-inst-pair'.)

If NOERROR is non-nil, signal an error if the inst-list is invalid;
otherwise return t."

  ;; OK, the possibilities are:
  ;;
  ;; a) an inst-pair or various abbreviations thereof
  ;; b) a list of (a)
  (let ((result (canonicalize-inst-pair inst-list specifier-type t)))
    (if (not (eq result t))
	;; case (a)
	(list result)

      (if (not (consp inst-list))
	  ;; not an inst-list.
	  (if noerror t
	   ;; this will signal an appropriate error.
	    (check-valid-instantiator inst-list specifier-type))

	;; case (b)
	(catch 'cann-inst-list
	  ;; don't use mapcar here; we need to catch the case of
	  ;; an invalid list.
	  (let ((rest inst-list)
		(result nil))
	    (while rest
	      (if (not (consp rest))
		  (if noerror (throw 'cann-inst-list t)
		    (signal 'error (list "Invalid list format" inst-list)))
		(let ((res2 (canonicalize-inst-pair (car rest) specifier-type
						    noerror)))
		  (if (eq res2 t)
		      ;; at this point, we know we're noerror because
		      ;; otherwise canonicalize-inst-pair would have
		      ;; signalled an error.
		      (throw 'cann-inst-list t)
		    (setq result (cons res2 result)))))
	      (setq rest (cdr rest)))
	    (nreverse result)))))))

(defun canonicalize-spec (spec specifier-type &optional noerror)
  "Canonicalize the given SPEC (a specification).

SPECIFIER-TYPE is the type of specifier that this SPEC will be used for.

Canonicalizing means converting to the full form for a spec, i.e.
`(LOCALE (TAG-SET . INSTANTIATOR) ...)'.  This function accepts a
possibly abbreviated inst-list or a cons of a locale and a possibly
abbreviated inst-list. (See `canonicalize-inst-list'.)

If NOERROR is nil, signal an error if the specification is invalid;
otherwise return t."
  ;; OK, the possibilities are:
  ;;
  ;; a) an inst-list or some abbreviation thereof
  ;; b) a cons of a locale and an inst-list
  (let ((result (canonicalize-inst-list spec specifier-type t)))
    (if (not (eq result t))
	;; case (a)
	(cons 'global result)

      (if (not (consp spec))
	  ;; not a spec.
	  (if noerror t
	    ;; this will signal an appropriate error.
	    (check-valid-instantiator spec specifier-type))

	(if (not (valid-specifier-locale-p (car spec)))
	    ;; invalid locale.
	    (if noerror t
	      (signal 'error (list "Invalid specifier locale" (car spec))))

	  ;; case (b)
	  (let ((result (canonicalize-inst-list (cdr spec) specifier-type
						noerror)))
	    (if (eq result t)
		;; at this point, we know we're noerror because
		;; otherwise canonicalize-inst-list would have
		;; signalled an error.
		t
	      (cons (car spec) result))))))))

(defun canonicalize-spec-list (spec-list specifier-type &optional noerror)
  "Canonicalize the given SPEC-LIST (a list of specifications).

SPECIFIER-TYPE specifies the type of specifier that this SPEC-LIST
will be used for.

Canonicalizing means converting to the full form for a spec-list, i.e.
`((LOCALE (TAG-SET . INSTANTIATOR) ...) ...)'.  This function accepts
a possibly abbreviated specification or a list of such things. (See
`canonicalize-spec'.) This is the function used to convert spec-lists
accepted by `set-specifier' and such into a form suitable for
`add-spec-list-to-specifier'.

The canonicalization algorithm is as follows:

1. Attempt to parse SPEC-LIST as a single, possibly abbreviated, specification.
2. If (1) fails, attempt to parse SPEC-LIST as a list of (abbreviated)
   specifications.
3. If (2) fails, SPEC-LIST is invalid.

A possibly abbreviated specification SPEC is parsed by

1. Attempt to parse SPEC as a possibly abbreviated inst-list.
2. If (1) fails, attempt to parse SPEC as a cons of a locale and an
   (abbreviated) inst-list.
3. If (2) fails, SPEC is invalid.

A possibly abbreviated inst-list INST-LIST is parsed by

1. Attempt to parse INST-LIST as a possibly abbreviated inst-pair.
2. If (1) fails, attempt to parse INST-LIST as a list of (abbreviated)
   inst-pairs.
3. If (2) fails, INST-LIST is invalid.

A possibly abbreviated inst-pair INST-PAIR is parsed by

1. Check if INST-PAIR is `valid-instantiator-p'.
2. If not, check if INST-PAIR is a cons of something that is a tag, ie,
   `valid-specifier-tag-p', and something that is `valid-instantiator-p'.
3. If not, check if INST-PAIR is a cons of a list of tags and something that
   is `valid-instantiator-p'.

In summary, this function generally prefers more abbreviated forms.

This function tries extremely hard to resolve any ambiguities, and the
built-in specifier types (font, image, toolbar, etc.) are designed so that
there won't be any ambiguities.  (#### Unfortunately there are bugs in the
treatment of toolbar spec-lists and generic spec-lists; avoid depending on
canonicalization for these types.)

If NOERROR is nil, signal an error if the spec-list is invalid;
otherwise return t."
  ;; OK, the possibilities are:
  ;;
  ;; a) a spec or various abbreviations thereof
  ;; b) a list of (a)
  (let ((result (canonicalize-spec spec-list specifier-type t)))
    (if (not (eq result t))
	;; case (a)
	(list result)

      (if (not (consp spec-list))
	  ;; not a spec-list.
	  (if noerror t
	   ;; this will signal an appropriate error.
	    (check-valid-instantiator spec-list specifier-type))

	;; case (b)
	(catch 'cann-spec-list
	  ;; don't use mapcar here; we need to catch the case of
	  ;; an invalid list.
	  (let ((rest spec-list)
		(result nil))
	    (while rest
	      (if (not (consp rest))
		  (if noerror (throw 'cann-spec-list t)
		    (signal 'error (list "Invalid list format" spec-list)))
		(let ((res2 (canonicalize-spec (car rest) specifier-type
					       noerror)))
		  (if (eq res2 t)
		      ;; at this point, we know we're noerror because
		      ;; otherwise canonicalize-spec would have
		      ;; signalled an error.
		      (throw 'cann-spec-list t)
		    (setq result (cons res2 result)))))
	      (setq rest (cdr rest)))
	    (nreverse result)))))))

(defun set-specifier (specifier value &optional locale tag-set how-to-add)
  "Add the specification(s) given by VALUE to SPECIFIER in LOCALE.

VALUE may be any of the values accepted by `canonicalize-spec-list', including

-- an instantiator (either a Lisp object which will be returned when the
   specifier is instanced, or a Lisp object that can be instantiated to
   produce an opaque value: eg, a font name (string) can be used for a font
   specifier, but an instance will be a font object)
-- a list of instantiators
-- a cons of a locale and an instantiator, or of a locale and a list of
   instantiators
-- a cons of a tag or tag-set and an instantiator (or list of instantiators)
-- a cons of a locale and the previous type of item
-- a list of one or more of any of the previous types of items
-- a canonical spec-list.

See `canonicalize-spec-list' for details.  If you need to know the details,
though, strongly consider using the unambiguous APIs `add-spec-to-specifier'
and `add-spec-list-to-specifier' instead.

Finally, VALUE can itself be a specifier (of the same type as
SPECIFIER), if you want to copy specifications from one specifier
to another; this is equivalent to calling `copy-specifier', and
LOCALE, TAG-SET, and HOW-TO-ADD have the same semantics as with
that function.

Note that a VALUE of `nil' is either illegal or will be treated as a value of
`nil'; it does not remove existing specifications.  Use `remove-specifier' for
that.  N.B. `remove-specifier' defaults to removing all specifications, not
just the 'global one!

Warning: this function is inherently heuristic, and should not be relied on to
properly resolve ambiguities, when specifier instantiators can be lists
\(currently, for toolbar specifiers and generic specifiers).  In those cases
use either `add-spec-to-specifier' or `add-spec-list-to-specifier'.

LOCALE indicates where this specification is active, and should be
a buffer, a window, a frame, a device, or the symbol `global' to
indicate that it applies everywhere.  LOCALE defaults to
`global' if omitted, and is overridden by locales provided by VALUE (in the
cases where value is a full specification or a spec-list).

Optional argument TAG-SET is a tag or a list of tags, to be associated
with the VALUE.  Tags are symbols (usually naming device types, such
as `x' and `tty', or device classes, such as `color', `mono', and
`grayscale'); specifying a TAG-SET restricts the scope of VALUE to
devices that match all specified tags. (You can also create your
own tags using `define-specifier-tag', and use them to identify
specifications added by you, so you can remove them later.)

Optional argument HOW-TO-ADD should be either nil or one of the
symbols `prepend', `append', `remove-tag-set-prepend',
`remove-tag-set-append', `remove-locale', `remove-locale-type',
or `remove-all'.  This specifies what to do with existing
specifications in LOCALE (and possibly elsewhere in the specifier).
Most of the time, you do not need to worry about this argument;
the default behavior of `remove-tag-set-prepend' is usually fine.
See `copy-specifier' and `add-spec-to-specifier' for a full
description of what each of these means.

Note that `set-specifier' is exactly complementary to `specifier-specs'
except in the case where SPECIFIER has no specs at all in it but nil
is a valid instantiator (in that case, `specifier-specs' will return
nil (meaning no specs) and `set-specifier' will interpret the `nil'
as meaning \"I'm adding a global instantiator and its value is `nil'\"),
or in strange cases where there is an ambiguity between a spec-list
and an inst-list, etc. (The built-in specifier types are designed
in such a way as to avoid any such ambiguities.)"

  ;; backward compatibility: the old function had HOW-TO-ADD as the
  ;; third argument and no arguments after that.
  ;; #### this should disappear at some point.
  (if (and (null how-to-add)
	   (memq locale '(prepend append remove-tag-set-prepend
				  remove-tag-set-append remove-locale
				  remove-locale-type remove-all)))
      (progn
	(setq how-to-add locale)
	(setq locale nil)))

  ;; proper beginning of the function.
  (let ((is-valid (valid-instantiator-p value (specifier-type specifier)))
	(nval value))
    (cond ((and (not is-valid) (specifierp nval))
	   (copy-specifier nval specifier locale tag-set nil how-to-add))
	  (t
	   (if tag-set
	       (progn
		 (if (not (listp tag-set))
		     (setq tag-set (list tag-set)))
		 ;; You tend to get more accurate errors
		 ;; for a variety of cases if you call
		 ;; canonicalize-tag-set here.
		 (setq tag-set (canonicalize-tag-set tag-set))
		 (if (and (not is-valid) (consp nval))
		     (setq nval
			   (mapcar #'(lambda (x)
				       (check-valid-instantiator
					x (specifier-type specifier))
				       (cons tag-set x))
				   nval))
		   (setq nval (cons tag-set nval)))))
	   (if locale
	       (setq nval (cons locale nval)))
	   (add-spec-list-to-specifier
	    specifier
	    (canonicalize-spec-list nval (specifier-type specifier))
	    how-to-add))))
  value)

(defun modify-specifier-instances (specifier func &optional args force default
				   locale tag-set)
  "Modify all specifications that match LOCALE and TAG-SET by FUNC.

For each specification that exists for SPECIFIER, in locale LOCALE
that matches TAG-SET, call the function FUNC with the instance as its
first argument and with optional arguments ARGS.  The result is then
used as the new value of the instantiator.

If there is no specification in the domain LOCALE matching TAG-SET and
FORCE is non-nil, an explicit one is created from the matching
specifier instance if that exists or DEFAULT otherwise. If LOCALE is
not a domain (i.e. a buffer), DEFAULT is always used. FUNC is then
applied like above and the resulting specification is added."

  (let ((spec-list (specifier-spec-list specifier locale tag-set)))
    (cond
     (spec-list
      ;; Destructively edit the spec-list
      (mapc #'(lambda (spec)
		(mapc #'(lambda (inst-pair)
			  (setcdr inst-pair
				  (apply func (cdr inst-pair) args)))
		      (cdr spec)))
	    spec-list)
      (add-spec-list-to-specifier specifier spec-list))
     (force
      (set-specifier specifier
                     (apply func
                            (or (and (valid-specifier-domain-p locale)
                                     (specifier-instance specifier))
                                default) args)
                     locale tag-set)))))

(defmacro let-specifier (specifier-list &rest body)
  "Add specifier specs, evaluate forms in BODY and restore the specifiers.
\(let-specifier SPECIFIER-LIST BODY...)

Each element of SPECIFIER-LIST should look like this:
\(SPECIFIER VALUE &optional LOCALE TAG-SET HOW-TO-ADD).

SPECIFIER is the specifier to be temporarily modified.  VALUE is the
instantiator to be temporarily added to SPECIFIER in LOCALE.  LOCALE,
TAG-SET and HOW-TO-ADD have the same meaning as in
`add-spec-to-specifier'.

The code resulting from macro expansion will add specifications to
specifiers using `add-spec-to-specifier'.  After BODY is finished, the
temporary specifications are removed and old spec-lists are restored.

LOCALE, TAG-SET and HOW-TO-ADD may be omitted, and default to nil.
The value of the last form in BODY is returned.

NOTE: If you want the specifier's instance to change in all
circumstances, use (selected-window) as the LOCALE.  If LOCALE is nil
or omitted, it defaults to `global'.

Example:
    (let-specifier ((modeline-shadow-thickness 0 (selected-window)))
      (sit-for 1))"
  (check-argument-type 'listp specifier-list)
  (flet ((gensym-frob (x name)
	   (if (or (atom x) (eq (car x) 'quote))
	       (list x)
	     (list (gensym name) x))))
    ;; VARLIST is a list of
    ;; ((SPECIFIERSYM SPECIFIER) (VALUE) (LOCALESYM LOCALE)
    ;;  (TAG-SET) (HOW-TO-ADD))
    ;; If any of these is an atom, then a separate symbol is
    ;; unnecessary, the CAR will contain the atom and CDR will be nil.
    (let* ((varlist (mapcar #'(lambda (listel)
				(or (and (consp listel)
					 (<= (length listel) 5)
					 (> (length listel) 1))
				    (signal 'error
					    (list
					     "should be a list of 2-5 elements"
					     listel)))
				;; VALUE, TAG-SET and HOW-TO-ADD are
				;; referenced only once, so we needn't
				;; frob them with gensym.
				(list (gensym-frob (nth 0 listel) "specifier-")
				      (list (nth 1 listel))
				      (gensym-frob (nth 2 listel) "locale-")
				      (list (nth 3 listel))
				      (list (nth 4 listel))))
			    specifier-list))
	   ;; OLDVALLIST is a list of (OLDVALSYM OLDVALFORM)
	   (oldvallist (mapcar #'(lambda (varel)
				   (list (gensym "old-")
					 `(specifier-spec-list
					   ,(car (nth 0 varel))
					   ,(car (nth 2 varel)))))
			       varlist)))
      ;; Bind the appropriate variables.
      `(let* (,@(mapcan #'(lambda (varel)
			    (delq nil (mapcar
				       #'(lambda (varcons)
					   (and (cdr varcons) varcons))
				       varel)))
			varlist)
		,@oldvallist)
	 (unwind-protect
	     (progn
	       ,@(mapcar #'(lambda (varel)
			     `(add-spec-to-specifier
			       ,(car (nth 0 varel)) ,(car (nth 1 varel))
			       ,(car (nth 2 varel)) ,(car (nth 3 varel))
			       ,(car (nth 4 varel))))
			 varlist)
	       ,@body)
	   ;; Reverse the unwinding order, so that using the same
	   ;; specifier multiple times works.
	   ,@(apply #'nconc (nreverse (mapcar*
				       #'(lambda (oldval varel)
					   `((remove-specifier
					      ,(car (nth 0 varel))
					      ,(car (nth 2 varel)))
					     (add-spec-list-to-specifier
					      ,(car (nth 0 varel))
					      ,(car oldval))))
				       oldvallist varlist))))))))

(defun make-integer-specifier (spec-list)
  "Return a new `integer' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for integer specifiers are integers."
  (make-specifier-and-init 'integer spec-list))

(defun make-boolean-specifier (spec-list)
  "Return a new `boolean' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for boolean specifiers are t and nil."
  (make-specifier-and-init 'boolean spec-list))

(defun make-natnum-specifier (spec-list)
  "Return a new `natnum' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for natnum specifiers are non-negative integers."
  (make-specifier-and-init 'natnum spec-list))

(defun make-generic-specifier (spec-list)
  "Return a new `generic' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for generic specifiers are all Lisp values.
They are returned back unchanged when a specifier is instantiated."
  (make-specifier-and-init 'generic spec-list))

(defun make-display-table-specifier (spec-list)
  "Return a new `display-table' specifier object with the given spec list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Valid instantiators for display-table specifiers are described in
detail in the doc string for `current-display-table'."
  (make-specifier-and-init 'display-table spec-list))

;; Evaluate this for testing:
; (cl-prettyexpand '(let-specifier ((modeline-shadow-thickness 0 (selected-window) 'x) (fubar (value) baz)) (sit-for 1)))

(define-specifier-tag 'win 'device-on-window-system-p)

;; Add tags for device types that don't have support compiled
;; into the binary that we're about to dump.  This will prevent
;; code like
;;
;; (set-face-foreground 'default "black" nil '(x color))
;;
;; from producing an error if no X support was compiled in.

(or (valid-specifier-tag-p 'x)
    (define-specifier-tag 'x (lambda (dev) (eq (device-type dev) 'x))))
(or (valid-specifier-tag-p 'tty)
    (define-specifier-tag 'tty (lambda (dev) (eq (device-type dev) 'tty))))
(or (valid-specifier-tag-p 'mswindows)
    (define-specifier-tag 'mswindows (lambda (dev)
				       (eq (device-type dev) 'mswindows))))
(or (valid-specifier-tag-p 'gtk)
    (define-specifier-tag 'gtk (lambda (dev) (eq (device-type dev) 'gtk))))

;; Add special tag for use by initialization code.  Code that
;; sets up default specs should use this tag.  Code that needs to
;; override default specs (e.g. the X resource initialization
;; code) can safely clear specs with this tag without worrying
;; about clobbering user settings.

(define-specifier-tag 'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    "Heuristic" specifier functions                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "Heuristic" is a euphemism for kludge.  This stuff works well in
;;; practice, though.

;;; You might view all the contortions we do here and in Face-frob-property
;;; as indicative of design failures with specifiers, and perhaps you're
;;; right.  But in fact almost all code that attempts to interface to
;;; humans and produce "intuitive" results gets messy, particularly with a
;;; system as complicated as specifiers, whose complexity results from an
;;; attempt to work well in many different circumstances.  We could create
;;; a much simpler system, but the tradeoff would be that you'd have to
;;; programmatically control all the stuff that gets handled automatically
;;; by setting the right specifiers -- and then things wouldn't "just work"
;;; if the user simultaneously creates a TTY and X device, or X devices on
;;; different types of machines, or wants some buffers to display
;;; differently from others, etc. without a lot of hook functions and other
;;; glue machinery to set everything up.  The result would be just as much
;;; complexity, but worse, and much harder to control, since there wouldn't
;;; be any standard framework for managing all these hook functions and the
;;; user would have to be able to write lots of Lisp code to get things
;;; working.

;;; The problem is that we have no high-level code, e.g. custom, to make it
;;; easy for the user to control specifiers nicely.  The following
;;; lower-level code, though, should make it easier to implement the
;;; high-level code.

;;; #### Something like Face-frob-property, but more general, should be
;;; created for general specifier frobbing.

;;; #### Other possible extensions to specifiers would be
;;;
;;; (a) the ability to create specifications for particular types of
;;;     buffers, e.g. all C-mode buffers one way, all text-mode buffers
;;;     another way, etc.  Perhaps this should be implemented through hook
;;;     functions; but that wouldn't easily allow you to `make-face-bold'
;;;     and have it work on these other kinds of specifications.  Probably
;;;     a better way is to extend the tag mechanism so that it can specify
;;;     things other than device types.  One way would be to simply allow
;;;     tags to have arbitrary elisp attached to them -- a function that
;;;     takes a domain and returns whether the attached instantiator
;;;     applies.  This should be doable given (a) that we now have code to
;;;     allow elisp to be run inside a "sandbox", sufficiently protected
;;;     that it can even be called from redisplay, and (b) the large amount
;;;     of caching we already have, which would minimize the speed hit.
;;;     However, this still runs into problems -- (a) it requires
;;;     programming to get anything at all done, and (b) you'll get
;;;     horrible namespace clashes very quickly.  Another possibility to be
;;;     used in conjunction with this would be vector tags, with an
;;;     extendable mechanism to control their syntax.  For example,
;;;
;;;     [tag :mode 'c] (buffer in c-mode)
;;;     [tag :buffer-name "\\*Help: function"] (help-on-function buffers)
;;;     [tag :buffer-coding-system 'japanese-euc] (buffer's coding system is
;;;	     		                   EUC-JP)
;;;     [tag :buffer-file-name "^#.*#$"] (autosave files)
;;;     [tag :language-environment "French"] (whenever the global language
;;;                                           environment is French)
;;;     [tag :font-height-minimum '(default 12)] (if the height of the default
;;;						  font is at least 12 pixels
;;;						  in this domain)
;;;                                                         
;;;     The general idea is that the properties allowable in a tag vector
;;;     are extendable, just by specifying the property name and a function
;;;     of two arguments, the property value and the domain, which should
;;;     return whether the tag applies.  You could imagine very complex
;;;     behavior (e.g. combining two tags in a single tag set makes an
;;;     `and', and putting the two tags separately with separate (perhaps
;;;     identical) instantiators makes an `or'.  You could effectively do a
;;;     lot of what you might want to do with hooks, but in a much more
;;;     controllable fashion.  Obviously, much of this complexity wouldn't
;;;     necessarily be directly set by the user -- they wouldn't probably
;;;     do more than simple tags based on mode, buffer or file name, etc.
;;;     But a higher-level interface could easily have various possible
;;;     "behaviors" to choose from, implemented using this mechanism.
;;;
;;;     #### WE NEED CUSTOM SUPPORT!
;;;
;;; (b) Another possibility is "partial" inheritance.  For example --
;;;     toolbars and menubars are complex specifications.  Currently the
;;;     only way to make a change is to copy the entire value and make the
;;;     necessary modifications.  What we would like instead is to be able
;;;     to construct a mini-menubar that says something like "add this menu
;;;     here" and combine with everything else.  That would require a
;;;     slightly different approach to instantiation.  Currently it just
;;;     searches up the tree from specific to general, looking for a match;
;;;     from this match, it generates the instance.  Instead, it would
;;;     potentially have to record all the matches it found and pass a list
;;;     of them to the instantiation function.  To implement this, we would
;;;     create another specifier method "instantiator_inherits_up", which
;;;     looks at the instantiator to determine if it calls for combining
;;;     itself with the value higher up.  this tells the specifier code
;;;     whether to stop now or keep going.  It would then pass a Dynarr of
;;;     the instantiators to the instantiate method, which might be a
;;;     special version, e.g. "instantiate_multi".

(defun instance-to-instantiator (inst)
  "Convert an instance to an instantiator.
If we have an instance object, we fetch the instantiator that generated the object.  Otherwise, we just return the instance."
  (cond ((font-instance-p inst)
	 (setq inst (font-instance-name inst)))
	((color-instance-p inst)
	 (setq inst (color-instance-name inst)))
	((image-instance-p inst)
	 (setq inst (image-instance-instantiator inst)))
	(t inst)))

(defun device-type-matches-spec (devtype devtype-spec)
  ;; Return DEVTYPE (a devtype) if it matches DEVTYPE-SPEC, else nil.
  ;; DEVTYPE-SPEC can be nil (all types OK), a device type (only that type
  ;; OK), or `window-system' -- window system device types OK.
  (cond ((not devtype-spec) devtype)
	((eq devtype-spec 'window-system)
	 (and (not (memq devtype '(tty stream))) devtype))
	(t (and (eq devtype devtype-spec) devtype))))

(defun add-tag-to-inst-list (inst-list tag-set)
  "Add TAG-SET (tag or tag-set) to all tags in INST-LIST."
  ;; Ah, all is sweetness and light with `loop'
  (if (null tag-set) inst-list
    (loop for (t2 . x2) in inst-list
      for newt2 = (delete-duplicates
		   (append (if (listp tag-set) tag-set (list tag-set))
			   (if (listp t2) t2 (list t2))))
      collect (cons newt2 x2))))

(defun derive-domain-from-locale (locale &optional devtype-spec current-device)
  "Given a locale, try to derive the \"most reasonable\" domain.

This is a heuristic \(\"works most of the time\") algorithm.

\[Remember that, in specifiers, locales are what you attach specifications or
\"instantiators\" to, and domains are the contexts in which you can
retrieve the value or \"instance\" of the specifier.  Not all locales are
domains.  In particular, buffers are locales but not domains because
buffers may be displayed in different windows on different frames, and thus
end up with different values if the frames each have a frame-local
instantiator and the instantiators are different.  However, we may well
find ourselves in a situation where we want to figure out the most likely
value of a specifier in a buffer -- for example we might conceptually want
to make a buffer's modeline face be bold, so we need to figure out what the
current face is.  If the buffer already has an instantiator, it's easy; but
if it doesn't, we want to do something reasonable rather than just issue an
error, even though technically the value is not well-defined.  We want
something that gives the right answer most of the time.]

LOCALE is a specifier locale -- i.e. a buffer, window, frame, device, the
symbol `global', or nil, meaning the same as `global'.

DEVTYPE-SPEC, if given, can restrict the possible return values to domains
on devices of that device type; or if it's `window-system', to domains on
window-system devices.

CURRENT-DEVICE is what should be considered as the \"selected device\" when
this value is needed.  It defaults to the currently selected device.

-- If LOCALE is a domain, it's simply returned.
-- If LOCALE is `all', `global', or nil, we return CURRENT-DEVICE.
-- If LOCALE is a buffer, we use `get-buffer-window' to find a window viewing
   the buffer, and return it if there is one; otherwise we return the selected
   window on CURRENT-DEVICE.

The return value may be nil if the only possible values don't agree with
DEVTYPE-SPEC."
  ;; DEVICE aims to be the selected device, but picks some other
  ;; device if that won't work.  may be nil.
  (let* ((device (or current-device (selected-device)))
	 (device (if (device-type-matches-spec (device-type device)
					       devtype-spec)
		     device
		   (first
		    (delete-if-not
		     #'(lambda (x)
			 (device-type-matches-spec (device-type x)
						   devtype-spec))
		     (device-list))))))
    (cond ((memq locale '(all nil global)) device)
	  ((valid-specifier-domain-p locale)
	   (and (device-type-matches-spec (device-type (dfw-device locale))
					  devtype-spec)
		locale))
	  ((bufferp locale)
	   (let ((win (get-buffer-window locale t devtype-spec)))
	     (or win (and device (selected-window device))))))))

(defun derive-device-type-from-tag-set (tag-set &optional try-stages
					devtype-spec current-device)
  "Given a tag set, try (heuristically) to get a device type from it.

There are three stages that this function proceeds through, each one trying
harder than the previous to get a value.  TRY-STAGES controls how many
stages to try.  If nil or 1, only stage 1 is done; if 2; stages 1 and 2 are
done; if 3, stages 1-3 are done; if t, all stages are done (currently 1-3).

Stage 1 looks at the tags themselves to see if any of them are device-type
tags.  If so, it returns the device type.  If there is more than one device
type, this tag can never match anything, but we go ahead and return one of
them.  If no device types in the tags, we fail.

Stage 2 runs all devices through the tag set to see if any match, and
accumulate a list of device types of all matching devices.  If there is
exactly one device type in the list, we return it, else fail.

Stage 3 picks up from where stage 2 left off, and tries hard to return
*SOME* device type in all possible situations, modulo the DEVTYPE-SPEC
flag. \(DEVTYPE-SPEC and CURRENT-DEVICE are the same as in
`derive-domain-from-locale'.)

Specifically:

\(a) if no matching devices, return the selected device's type.
\(b) if more than device type and the selected device's type is
     listed, use it.
\(c) else, pick one of the device types (currently the first).

This will never return a device type that's incompatible with the
DEVTYPE-SPEC flag; thus, it may return nil."
  (or try-stages (setq try-stages 1))
  (if (eq try-stages t) (setq try-stages 3))
  (check-argument-range try-stages 1 3)
  (flet ((delete-wrong-type (x)
	   (delete-if-not
	    #'(lambda (y)
		(device-type-matches-spec y devtype-spec))
	    x)))
    (let ((both (intersection (device-type-list)
			      (canonicalize-tag-set tag-set))))
      ;; shouldn't be more than one (will fail), but whatever
      (if both (first (delete-wrong-type both))
	(and (>= try-stages 2)
	     ;; no device types mentioned.  try the hard way,
	     ;; i.e. check each existing device to see if it will
	     ;; pass muster.
	     (let ((okdevs
		    (delete-wrong-type
		     (delete-duplicates
		      (mapcan
		       #'(lambda (dev)
			   (and (device-matches-specifier-tag-set-p
				 dev tag-set)
				(list (device-type dev))))
		       (device-list)))))
		   (devtype (cond ((or (null devtype-spec)
				       (eq devtype-spec 'window-system))
				   (let ((dev (derive-domain-from-locale
					       'global devtype-spec
					       current-device)))
				     (and dev (device-type dev))))
				  (t devtype-spec))))
	       (cond ((= 1 (length okdevs)) (car okdevs))
		     ((< try-stages 3) nil)
		     ((null okdevs) devtype)
		     ((memq devtype okdevs) devtype)
		     (t (car okdevs)))))))))

;; Sheesh, the things you do to get "intuitive" behavior.
(defun derive-device-type-from-locale-and-tag-set (locale tag-set
						   &optional devtype-spec
						   current-device)
  "Try to derive a device type from a locale and tag set.

If the locale is a domain, use the domain's device type.  Else, if the tag
set uniquely specifies a device type, use it.  Else, if a buffer is given,
find a window visiting the buffer, and if any, use its device type.
Finally, go back to the tag set and \"try harder\" -- if the selected
device matches the tag set, use its device type, else use some valid device
type from the tag set.

DEVTYPE-SPEC and CURRENT-DEVICE as in `derive-domain-from-locale'."

  (cond ((valid-specifier-domain-p locale)
	 ;; if locale is a domain, then it must match DEVTYPE-SPEC,
	 ;; or we exit immediately with nil.
	 (device-type-matches-spec (device-type (dfw-device locale))
				   devtype-spec))
	((derive-device-type-from-tag-set tag-set 2 devtype-spec
					  current-device))
	((and (bufferp locale)
	      (let ((win (get-buffer-window locale t devtype-spec)))
		(and win (device-type (dfw-device win))))))
	((derive-device-type-from-tag-set tag-set t devtype-spec
					  current-device))))

(defun derive-specifier-specs-from-locale (specifier locale
					   &optional devtype-spec
					   current-device
					   global-use-fallback)
  "Heuristically find the specs of a specifier in a locale.

This tries to find some reasonable instantiators that are most likely to
correspond to the specifier's \"value\" (i.e. instance) in a particular
locale, even when the user has not specifically set any such instantiators.
This is useful for functions that want to modify the instance of a
specifier in a particular locale, and only in that locale.

Keep in mind that this is a heuristic (i.e. kludge) function, and that it
may not always give the right results, since the operation is not
technically well-defined in many cases! (See `derive-domain-from-locale'.)

DEVTYPE-SPEC and CURRENT-DEVICE are as in `derive-domain-from-locale'.

The return value is an inst-list, i.e.

   ((TAG-SET . INSTANTIATOR) ...)

More specifically, if there is already a spec in the locale, it's just
returned.  Otherwise, if LOCALE is `global', `all', or nil: If
GLOBAL-USE-FALLBACK is non-nil, the fallback is fetched, and returned, with
`default' added to the tag set; else, we use CURRENT-DEVICE (defaulting to
the selected device) as a domain and proceed as in the following.  If
LOCALE is a domain (window, frame, device), the specifier's instance in
that domain is computed, and converted back to an instantiator
\(`instance-to-instantiator').  Else, if LOCALE is a buffer, we use
`derive-domain-from-locale' to heuristically get a likely domain, and
proceed as if LOCALE were a domain."
  (if (memq locale '(all nil)) (setq locale 'global))
  (let ((current (specifier-spec-list specifier locale)))
    (if current (cdar current)
      ;; case 1: a global locale, fallbacks
      (cond ((and (eq locale 'global) global-use-fallback)
	     ;; if nothing there globally, retrieve the fallback.
	     ;; this is either an inst-list or a specifier.  in the
	     ;; latter case, we need to recursively retrieve its
	     ;; fallback.
	     (let (sofar
		   (fallback (specifier-fallback specifier)))
	       (while (specifierp fallback)
		 (setq sofar (nconc sofar
				    (cdar (specifier-spec-list fallback
							       'global))))
		 (setq fallback (specifier-fallback fallback)))
	       (add-tag-to-inst-list (nconc sofar fallback) 'default)))
	    (t
	     (let (domain)
	       ;; case 2: window, frame, device locale
	       (cond ((eq locale 'global)
		      (setq domain (or current-device (selected-device))))
		     ((valid-specifier-domain-p locale)
		      (setq domain locale))
		     ;; case 3: buffer locale
		     ((bufferp locale)
		      (setq domain (derive-domain-from-locale
				    locale devtype-spec current-device)))
		     (t nil))
	       ;; retrieve an instance, convert back to instantiator
	       (when domain
		 (let ((inst
			(instance-to-instantiator
			 (specifier-instance specifier domain))))
		   (list (cons nil inst))))))))))

;;; specifier.el ends here
