;;; backquote.el --- Full backquote support for elisp.  Reverse compatible too.

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;; The bulk of the code is originally from CMU Common Lisp (original notice
;; below).

;; It correctly supports nested backquotes and backquoted vectors.

;; Converted to work with elisp by Miles Bader <miles@cogsci.ed.ac.uk>

;; Changes by Jonathan Stigelman <Stig@hackvan.com>:
;;   - Documentation added
;;   - support for old-backquote-compatibility-hook nixed because the
;;	old-backquote compatibility is now done in the reader...
;;   - nixed support for |,.| because
;;	(a) it's not in CLtl2
;;	(b) ",.foo" is the same as ". ,foo"
;;	(c) because RMS isn't interested in using this version of backquote.el
;;
;; ben@xemacs.org added ,. support back in:
;;     (a) yes, it is in CLtl2.  Read closely on page 529.
;;     (b) RMS in 19.30 adds C support for ,. even if it's not really
;;         handled.
;;
;; **********************************************************************
;; This code was written as part of the CMU Common Lisp project at
;; Carnegie Mellon University, and has been placed in the public domain.
;; If you want to use this code or any part of CMU Common Lisp, please contact
;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;
;; **********************************************************************
;;
;;    BACKQUOTE: Code Spice Lispified by Lee Schumacher.
;;
;; The flags passed back by BQ-PROCESS-2 can be interpreted as follows:
;;
;;   |`,|: [a] => a
;;    NIL: [a] => a		;the NIL flag is used only when a is NIL
;;      T: [a] => a		;the T flag is used when a is self-evaluating
;;  QUOTE: [a] => (QUOTE a)
;; APPEND: [a] => (APPEND . a)
;;  NCONC: [a] => (NCONC . a)
;;   LIST: [a] => (LIST . a)
;;  LIST*: [a] => (LIST* . a)
;;
;; The flags are combined according to the following set of rules:
;;  ([a] means that a should be converted according to the previous table)
;;
;;   \ car  ||   otherwise    |   QUOTE or     |    |`,@|      |    |`,.|
;;cdr \     ||                |   T or NIL     |               |
;;============================================================================
;;  |`,|    ||LIST* ([a] [d]) |LIST* ([a] [d]) |APPEND (a [d]) |NCONC  (a [d])
;;  NIL     ||LIST    ([a])   |QUOTE    (a)    |<hair>    a    |<hair>    a
;;QUOTE or T||LIST* ([a] [d]) |QUOTE  (a . d)  |APPEND (a [d]) |NCONC (a [d])
;; APPEND   ||LIST* ([a] [d]) |LIST* ([a] [d]) |APPEND (a . d) |NCONC (a [d])
;; NCONC    ||LIST* ([a] [d]) |LIST* ([a] [d]) |APPEND (a [d]) |NCONC (a . d)
;;  LIST    ||LIST  ([a] . d) |LIST  ([a] . d) |APPEND (a [d]) |NCONC (a [d])
;;  LIST*   ||LIST* ([a] . d) |LIST* ([a] . d) |APPEND (a [d]) |NCONC  (a [d])
;;
;;<hair> involves starting over again pretending you had read ".,a)" instead
;; of ",@a)"
;;

;; These are the forms it expects:  |backquote|  |`|  |,|  |,@| and |,.|.

;;; Code:

(defconst bq-backquote-marker 'backquote)
(defconst bq-backtick-marker '\`)	; remnant of the old lossage
(defconst bq-comma-marker '\,)
(defconst bq-at-marker '\,@)
(defconst bq-dot-marker '\,\.)

;;; ----------------------------------------------------------------

(fset '\` 'backquote)

(defmacro backquote (template)
  "Expand the internal representation of a backquoted TEMPLATE into a lisp form.

The backquote character is like the quote character in that it prevents the
template which follows it from being evaluated, except that backquote
permits you to evaluate portions of the quoted template.  A comma character
inside TEMPLATE indicates that the following item should be evaluated.  A
comma character may be followed by an at-sign, which indicates that the form
which follows should be evaluated and inserted and \"spliced\" into the
template.  Forms following ,@ must evaluate to lists.

Here is how to use backquotes:
  (setq p 'b
        q '(c d e))
  `(a ,p ,@q)   -> (a b c d e)
  `(a . b)      -> (a . b)
  `(a . ,p)     -> (a . b)

The XEmacs lisp reader expands lisp backquotes as it reads them.
Examples:
  `atom             is read as (backquote atom)
  `(a ,b ,@(c d e)) is read as (backquote (a (\\, b) (\\,\\@ (c d e))))
  `(a . ,p)         is read as (backquote (a \\, p))

\(backquote TEMPLATE) is a macro that produces code to construct TEMPLATE.
Note that this is very slow in interpreted code, but fast if you compile.
TEMPLATE is one or more nested lists or vectors, which are `almost quoted'.
They are copied recursively, with elements preceded by comma evaluated.
 (backquote (a b))     == (list 'a 'b)
 (backquote (a [b c])) == (list 'a (vector 'b 'c))

However, certain special lists are not copied.  They specify substitution.
Lists that look like (\\, EXP) are evaluated and the result is substituted.
 (backquote (a (\\, (+ x 5)))) == (list 'a (+ x 5))

Elements of the form (\\,\\@ EXP) are evaluated and then all the elements
of the result are substituted.  This result must be a list; it may
be `nil'.

Elements of the form (\\,\\. EXP) are evaluated and then all the elements
of the result are concatenated to the list of preceding elements in the list.
They must occur as the last element of a list (not a vector).
EXP may evaluate to nil.

As an example, a simple macro `push' could be written:
   (defmacro push (v l)
     `(setq ,l (cons ,@(list v l))))
or as
   (defmacro push (v l)
     `(setq ,l (cons ,v ,l)))

For backwards compatibility, old-style emacs-lisp backquotes are still read.
     OLD STYLE                        NEW STYLE
     (` (foo (, bar) (,@ bing)))      `(foo ,bar ,@bing)

Because of the old-style backquote support, you cannot use a new-style
backquoted form as the first element of a list.  Perhaps some day this
restriction will go away, but for now you should be wary of it:
    (`(this ,will ,@fail))
    ((` (but (, this) will (,@ work))))
This is an extremely rare thing to need to do in lisp."
  (bq-process template))

;;; ----------------------------------------------------------------

(defconst bq-comma-flag 'unquote)
(defconst bq-at-flag 'unquote-splicing)
(defconst bq-dot-flag 'unquote-nconc-splicing)

(defun bq-process (form)
  (let* ((flag-result (bq-process-2 form))
	 (flag (car flag-result))
	 (result (cdr flag-result)))
    (cond ((eq flag bq-at-flag)
	   (error 'invalid-read-syntax ",@ after ` in form" form))
	  ((eq flag bq-dot-flag)
	   (error 'invalid-read-syntax ",. after ` in form" form))
	  (t
	   (bq-process-1 flag result)))))

;;; ----------------------------------------------------------------

;;; This does the expansion from table 2.
(defun bq-process-2 (code)
  (cond ((atom code)
	 (cond ((null code)
                (cons nil nil))
               ((vectorp code)
                (let* ((dflag-d (bq-process-2 (append code nil)))
                       (dflag (car dflag-d))
                       (d (cdr dflag-d)))
                  (cond
                    ((member* dflag '(quote nil))
                     (cons t code))
                    ((eq dflag 'list)
                     (cons 'vector d))
                    ((eq dflag 'append)
                     ;; The idea for this is from GNU, thank you GNU. I don't
                     ;; like #'vconcat much, it's not very CL, but it does fit
                     ;; this use case, and there is no prospect of our
                     ;; removing it.
                     (cons 'vconcat d))
                    (t (cons 'vector* (bq-process-1 dflag d))))))
               (t (cons 'quote code))))
	((eq (car code) bq-at-marker)
	 (cons bq-at-flag (nth 1 code)))
	((eq (car code) bq-dot-marker)
	 (cons bq-dot-flag (nth 1 code)))
	((eq (car code) bq-comma-marker)
	 (bq-comma (nth 1 code)))
	((or (eq (car code) bq-backquote-marker)
	     (eq (car code) bq-backtick-marker))	; old lossage
	 (bq-process-2 (bq-process (nth 1 code))))
	(t (let* ((aflag-a (bq-process-2 (car code)))
		  (aflag (car aflag-a))
		  (a (cdr aflag-a)))
	     (let* ((dflag-d (bq-process-2 (cdr code)))
		    (dflag (car dflag-d))
		    (d (cdr dflag-d)))
	       (if (eq dflag bq-at-flag)
		   ;; get the errors later.
		   (error 'invalid-read-syntax ",@ after dot" code))
	       (if (eq dflag bq-dot-flag)
		   (error 'invalid-read-syntax ",. after dot" code))
	       (cond
		((eq aflag bq-at-flag)
		 (if (null dflag)
		     (bq-comma a)
                   (cons 'append
                         (cond ((eq dflag 'append)
                                (cons a d ))
                               (t (list a (bq-process-1 dflag d)))))))
                ((eq aflag bq-dot-flag)
                 (if (null dflag)
                     (bq-comma a)
                   (cons 'nconc
                         (cond ((eq dflag 'nconc)
                                (cons a d))
                               (t (list a (bq-process-1 dflag d)))))))
		((null dflag)
		 (if (member* aflag '(quote t nil))
		     (cons 'quote (list a))
                   (cons 'list (list (bq-process-1 aflag a)))))
		((member* dflag '(quote t))
		 (if (member* aflag '(quote t nil))
		     (cons 'quote (cons a d ))
                   (cons 'list* (list (bq-process-1 aflag a)
                                      (bq-process-1 dflag d)))))
		(t (setq a (bq-process-1 aflag a))
		   (if (member* dflag '(list list*))
		       (cons dflag (cons a d))
                     (cons 'list*
                           (list a (bq-process-1 dflag d)))))))))))

;;; This handles the <hair> cases
(defun bq-comma (code)
  (cond ((atom code)
	 (cond ((null code)
		(cons nil nil))
	       ((eq code t)
		(cons t code))
	       (t (cons bq-comma-flag code))))
	((eq (car code) 'quote)
	 (cons (car code) (car (cdr code))))
	((member* (car code) '(append list list* nconc))
	 (cons (car code) (cdr code)))
	((eq (car code) 'cons)
	 (cons 'list* (cdr code)))
	(t (cons bq-comma-flag code))))

;;; This handles table 1.
(defun bq-process-1 (flag thing)
  (cond ((eq flag bq-comma-flag)
	 thing)
	((member* flag '(quote t nil))
         (quote-maybe thing))
	((eq flag 'vector*)
         (list 'apply '(function vector) thing))
	(t (cons flag thing))))

(provide 'backquote)

;;; backquote.el ends here
