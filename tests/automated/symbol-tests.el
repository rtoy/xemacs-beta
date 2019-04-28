;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Created: 1999
;; Keywords: tests

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test symbols operations.
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))


(defun ts-fresh-symbol-name (name)
  "Return a variant of NAME (a string) that is not interned."
  (when (intern-soft name)
    (let ((count 1)
	  (orig name))
      (while (progn
	       (setq name (format "%s-%d" orig count))
	       (intern-soft name))
	(incf count))))
  name)

;;-----------------------------------------------------
;; Creating, reading, and printing symbols
;;-----------------------------------------------------

(dolist (name '("foo" "bar" ""
		"something with space in it"
		"a string with \0 in the middle."
		"100" "10.0" "#<>[]]]];'\\';"
		"!@#$%^^&*(()__"))
  (let ((interned (intern name))
	(uninterned (make-symbol name)))
    (Assert (symbolp interned))
    (Assert (symbolp uninterned))
    (Assert (equal (symbol-name interned) name))
    (Assert (equal (symbol-name uninterned) name))
    (Assert (not (eq interned uninterned)))
    (Assert (not (equal interned uninterned)))))

(macrolet
    ((Check-package-case-sensitivity (&rest names)
       (cons
        'progn
        (mapcan
         #'(lambda (name)
             (block done
               (if (eq 'decode-coding-string (car-safe name))
                   (if (featurep 'mule)
                       (setq name (eval name))
                     ;; Can't handle this string on non-mule:
                     (return-from done nil)))
               (let ((name-upper (upcase name))
                     (name-lower (downcase name))
                     (name-titlecase (upcase-initials name)))
                 `((Assert (not (eq (intern ,name sensitive)
                                    (intern ,name insensitive)))
                           ,(concat "Checking `" name
                                    "' interns as distinct symbols in "
                                    "distinct packages"))
                   ,@(unless
                      (equal name name-upper)
                      `((Assert (eq (intern ,name insensitive)
                                    (intern ,name-upper insensitive))
                                ,(concat "Checking `" name
                                         "' and `" name-upper
                                         "' map to the same symbol, "
                                         "case-insensitive package"))
                        (Assert (eq (intern-soft ,name-upper sensitive pi)
                                    pi)
                                ,(concat "Checking `" name-upper
                                         "' not yet interned in the "
                                         "case-sensitive package"))
                        (Assert (not (eq (intern ,name sensitive)
                                         (intern ,name-upper sensitive)))
                                ,(concat "Checking `" name
                                         "' and `" name-upper
                                         "' map to distinct symbols, "
                                         "case-sensitive package"))))
                   ,@(unless
                      (equal name name-lower)
                      `((Assert (eq (intern ,name insensitive)
                                    (intern ,name-lower insensitive))
                                ,(concat "Checking `" name
                                         "' and `" name-lower
                                         "' map to the same symbol, "
                                         "case-insensitive package"))
                        (Assert (eq (intern-soft ,name-lower sensitive pi)
                                    pi)
                                ,(concat "Checking `" name-lower
                                         "' not yet interned in the "
                                         "case-sensitive package"))
                        (Assert (not (eq (intern ,name sensitive)
                                         (intern ,name-lower sensitive)))
                                ,(concat "Checking `" name
                                         "' and `" name-lower
                                         "' map to distinct symbols, "
                                         "case-sensitive package"))))
                   ,@(unless
                      (equal name name-titlecase)
                      `((Assert (eq (intern ,name insensitive)
                                    (intern ,name-titlecase insensitive))
                                ,(concat "Checking `" name
                                         "' and `" name-titlecase
                                         "' map to the same symbol, "
                                         "case-insensitive package"))
                        (Assert (eq (intern-soft ,name-titlecase sensitive pi)
                                    pi)
                                ,(concat "Checking `" name-titlecase
                                         "' not yet interned in the "
                                         "case-sensitive package"))
                        (Assert (not (eq (intern ,name sensitive)
                                         (intern ,name-titlecase sensitive)))
                                ,(concat "Checking `" name
                                         "' and `" name-titlecase
                                         "' map to distinct symbols, "
                                         "case-sensitive package"))))))))
           names))))
  (let ((insensitive (make-hash-table :test #'equalp))
        (sensitive (make-hash-table :test #'equal)))
    (Check-package-case-sensitivity
     "foo" "bar" "" "something with space in it"
     "a string with \0 in the middle." "100" "10.0" "#<>[]]]];'\\';"
     ;; "καρδιακή"
     (decode-coding-string
      "\xce\xba\xce\xb1\xcf\x81\xce\xb4\xce\xb9\xce\xb1\xce\xba\xce\xae"
      'utf-8)
     ;; ", κοινώς γνωστό ως καρδιακή προσβολή, "
     (decode-coding-string
      "\x2c\x20\xce\xba\xce\xbf\xce\xb9\xce\xbd\xcf\x8e\xcf\x82\x20\xce\xb3\xce\xbd\xcf\x89\xcf\x83\xcf\x84\xcf\x8c\x20\xcf\x89\xcf\x82\x20\xce\xba\xce\xb1\xcf\x81\xce\xb4\xce\xb9\xce\xb1\xce\xba\xce\xae\x20\xcf\x80\xcf\x81\xce\xbf\xcf\x83\xce\xb2\xce\xbf\xce\xbb\xce\xae\x2c\x20"
      'utf-8)
     "!@#$%^^&*(()__"
     ;; "Иногда"
     (decode-coding-string "\xd0\x98\xd0\xbd\xd0\xbe\xd0\xb3\xd0\xb4\xd0\xb0"
                           'utf-8)
     ;; "цианокобаламин, так как именно"
     (decode-coding-string "\xd1\x86\xd0\xb8\xd0\xb0\xd0\xbd\xd0\xbe\xd0\xba\xd0\xbe\xd0\xb1\xd0\xb0\xd0\xbb\xd0\xb0\xd0\xbc\xd0\xb8\xd0\xbd\x2c\x20\xd1\x82\xd0\xb0\xd0\xba\x20\xd0\xba\xd0\xb0\xd0\xba\x20\xd0\xb8\xd0\xbc\xd0\xb5\xd0\xbd\xd0\xbd\xd0\xbe"
                           'utf-8))))

(labels ((check-weak-list-unique (weak-list &optional reversep)
           "Check that elements of WEAK-LIST are referenced only there."
           (let ((len (length (weak-list-list weak-list))))
             (if (string-match "Using the new GC algorithms."
                               Installation-string)
                 (Implementation-Incomplete-Expect-Failure
                  (Assert (not (zerop len)))
                  (garbage-collect)
                  (Assert (eq (length (weak-list-list weak-list))
                              (if (not reversep) 0 len))))
               (Assert (not (zerop len)))
               (garbage-collect)
               (Assert (eq (length (weak-list-list weak-list))
                           (if (not reversep) 0 len)))))))
  (let ((weak-list (make-weak-list))
	(gc-cons-threshold most-positive-fixnum))
    ;; Symbols created with `make-symbol' and `gensym' should be fresh
    ;; and not referenced anywhere else.  We check that no other
    ;; references are available using a weak list.
    (eval
     ;; This statement must not be run byte-compiled, or the values
     ;; remain referenced on the bytecode interpreter stack.
     '(set-weak-list-list weak-list (list (make-symbol "foo") (gensym "foo"))))
    (check-weak-list-unique weak-list)

    ;; Equivalent test for `intern' and `gentemp'.
    (eval
     '(set-weak-list-list weak-list
			  (list (intern (ts-fresh-symbol-name "foo"))
				(gentemp (ts-fresh-symbol-name "bar")))))
    (check-weak-list-unique weak-list 'not)))

(Assert (not (intern-soft (make-symbol "foo"))))
(Assert (not (intern-soft (gensym "foo"))))
(Assert (intern-soft (intern (ts-fresh-symbol-name "foo"))))
(Assert (intern-soft (gentemp (ts-fresh-symbol-name "bar"))))

;; Reading a symbol should intern it automatically, unless the symbol
;; is marked specially.
(dolist (string (mapcar #'ts-fresh-symbol-name '("foo" "bar" "\\\0\\\1")))
  (setq symbol (read string)
	string (read (concat "\"" string "\"")))
  (Assert (intern-soft string))
  (Assert (intern-soft symbol))
  (Assert (eq (intern-soft string) (intern-soft symbol))))

(let ((fresh (read (concat "#:" (ts-fresh-symbol-name "foo")))))
  (Assert (not (intern-soft fresh))))

;; Check #N=OBJECT and #N# read syntax.
(let* ((list (read "(#1=#:foo #1# #2=#:bar #2# #1# #2#)"))
       (foo  (nth 0 list))
       (foo2 (nth 1 list))
       (bar  (nth 2 list))
       (bar2 (nth 3 list))
       (foo3 (nth 4 list))
       (bar3 (nth 5 list)))
  (Assert (symbolp foo))
  (Assert (not (intern-soft foo)))
  (Assert (equal (symbol-name foo) "foo"))
  (Assert (symbolp bar))
  (Assert (not (intern-soft bar)))
  (Assert (equal (symbol-name bar) "bar"))

  (Assert (eq foo foo2))
  (Assert (eq foo2 foo3))
  (Assert (eq bar bar2))
  (Assert (eq bar2 bar3)))

;; Check #N=OBJECT and #N# print syntax.
(let* ((foo (make-symbol "foo"))
       (bar (make-symbol "bar"))
       (list (list foo foo bar bar foo bar)))
  (let* ((print-gensym nil)
	 (printed-list (prin1-to-string list)))
    (Assert (equal printed-list "(foo foo bar bar foo bar)")))
  (let* ((print-gensym t)
	 (print-continuous-numbering t)
	 (printed-list (prin1-to-string list)))
    (Assert (equal printed-list "(#1=#:foo #1# #2=#:bar #2# #1# #2#)"))))

;;-----------------------------------------------------
;; Read-only symbols
;;-----------------------------------------------------

(Check-Error setting-constant
  (set nil nil))
(Check-Error setting-constant
  (set t nil))

;;-----------------------------------------------------
;; Variable indirections
;;-----------------------------------------------------

(let ((foo 0)
      (bar 1))
  (defvaralias 'foo 'bar)
  (Assert (eq foo bar))
  (Assert (eq foo 1))
  (Assert (eq (variable-alias 'foo) 'bar))
  (defvaralias 'bar 'foo)
  (Check-Error cyclic-variable-indirection
    (symbol-value 'foo))
  (Check-Error cyclic-variable-indirection
    (symbol-value 'bar))
  (defvaralias 'foo nil)
  (Assert (eq foo 0))
  (defvaralias 'bar nil)
  (Assert (eq bar 1)))

;;-----------------------------------------------------
;; Keywords
;;-----------------------------------------------------

;;; Reading keywords

;; In Elisp, a keyword is by definition a symbol beginning with `:'
;; that is interned in the global obarray.

;; In Elisp, a keyword is interned as any other symbol.
(Assert (eq (read ":foo") (intern ":foo")))

;; A keyword is self-quoting and evaluates to itself.
(Assert (eq (eval (intern ":foo")) :foo))

;; Keywords are recognized as such only if interned in the global
;; obarray, and `keywordp' is aware of that.
(Assert (keywordp :foo))
(Assert (not (keywordp (intern ":foo" [0]))))

;; Keywords used to be initialized at read-time, which resulted in
;; (symbol-value (intern ":some-new-keyword")) signaling an error.
;; Now we handle keywords at the time when the symbol is interned, so
;; that (intern ":something) and (read ":something) will be
;; equivalent.  These tests check various operations on symbols that
;; are guaranteed to be freshly interned.

;; Interning a fresh keyword string should produce a regular
;; keyword.
(let* ((fresh-keyword-name (ts-fresh-symbol-name ":foo"))
       (fresh-keyword (intern fresh-keyword-name)))
  (Assert (eq (symbol-value fresh-keyword) fresh-keyword))
  (Assert (keywordp fresh-keyword)))

;; Likewise, reading a fresh keyword string should produce a regular
;; keyword.
(let* ((fresh-keyword-name (ts-fresh-symbol-name ":foo"))
       (fresh-keyword (read fresh-keyword-name)))
  (Assert (eq (symbol-value fresh-keyword) fresh-keyword))
  (Assert (keywordp fresh-keyword)))

;;; Assigning to keywords

;; You shouldn't be able to set its value to something bogus.
(Check-Error setting-constant
  (set :foo 5))

;; But, for backward compatibility, setting to the same value is OK.
(Assert
  (eq (set :foo :foo) :foo))

;; Playing games with `intern' shouldn't fool us.
(Check-Error setting-constant
  (set (intern ":foo" obarray) 5))
(Assert
  (eq (set (intern ":foo" obarray) :foo) :foo))

;; But symbols not interned in the global obarray are not real
;; keywords (in elisp):
(Assert (eq (set (intern ":foo" [0]) 5) 5))

;;; Printing keywords

(let ((print-gensym t))
  (Assert (equal (prin1-to-string :foo)                ":foo"))
  (Assert (equal (prin1-to-string (intern ":foo"))     ":foo"))
  (Assert (equal (prin1-to-string (intern ":foo" [0])) "#::foo")))

(let ((print-gensym nil))
  (Assert (equal (prin1-to-string :foo)                ":foo"))
  (Assert (equal (prin1-to-string (intern ":foo"))     ":foo"))
  (Assert (equal (prin1-to-string (intern ":foo" [0])) ":foo")))

;; #### Add many more tests for printing and reading symbols, as well
;; as print-gensym and print-gensym-alist!

;;-----------------------------------------------------
;; Magic symbols
;;-----------------------------------------------------

;; Magic symbols are only half implemented.  However, a subset of the
;; functionality is being used to implement backward compatibility or
;; clearer error messages for new features such as specifiers and
;; glyphs.  These tests try to test that working subset.

(let ((mysym (make-symbol "test-symbol"))
      save)
  (dontusethis-set-symbol-value-handler
   mysym
   'set-value
   (lambda (&rest args)
     (throw 'test-tag args)))
  (Assert (not (boundp mysym)))
  (Assert (equal (catch 'test-tag
		   (set mysym 'foo))
		 `(,mysym (foo) set nil nil)))
  (Assert (not (boundp mysym)))
  (dontusethis-set-symbol-value-handler
   mysym
   'set-value
   (lambda (&rest args) (setq save (nth 1 args))))
  (set mysym 'foo)
  (Assert (equal save '(foo)))
  (Assert (eq (symbol-value mysym) 'foo))
  )

(let ((mysym (make-symbol "test-symbol"))
      save)
  (dontusethis-set-symbol-value-handler
   mysym
   'make-unbound
   (lambda (&rest args)
     (throw 'test-tag args)))
  (Assert (equal (catch 'test-tag
		   (makunbound mysym))
		 `(,mysym nil makunbound nil nil)))
  (dontusethis-set-symbol-value-handler
   mysym
   'make-unbound
   (lambda (&rest args) (setq save (nth 2 args))))
  (Assert (not (boundp mysym)))
  (set mysym 'bar)
  (Assert (null save))
  (Assert (eq (symbol-value mysym) 'bar))
  (makunbound mysym)
  (Assert (not (boundp mysym)))
  (Assert (eq save 'makunbound))
  )

;; pathname-coding-system is no more.
; (when (featurep 'file-coding)
;   (Assert (eq pathname-coding-system file-name-coding-system))
;   (let ((val1 file-name-coding-system)
; 	(val2 pathname-coding-system))
;     (Assert (eq val1 val2))
;     (let ((file-name-coding-system 'no-conversion-dos))
;       (Assert (eq file-name-coding-system 'no-conversion-dos))
;       (Assert (eq pathname-coding-system file-name-coding-system)))
;     (let ((pathname-coding-system 'no-conversion-mac))
;       (Assert (eq file-name-coding-system 'no-conversion-mac))
;       (Assert (eq pathname-coding-system file-name-coding-system)))
;     (Assert (eq file-name-coding-system pathname-coding-system))
;     (Assert (eq val1 file-name-coding-system)))
;   (Assert (eq pathname-coding-system file-name-coding-system)))


;(let ((mysym (make-symbol "test-symbol")))
;  (dontusethis-set-symbol-value-handler
;   mysym
;   'make-local
;   (lambda (&rest args)
;     (throw 'test-tag args)))
;  (Assert (equal (catch 'test-tag
;		   (set mysym 'foo))
;		 `(,mysym (foo) make-local nil nil))))

;; ----------------------------------------------------------------
;; Symbol documentation
;; ----------------------------------------------------------------

;; built-in variable documentation
(Assert (string= (built-in-symbol-file 'internal-doc-file-name)
		 "doc.c"))

;; built-in function documentation
(Assert (string= (built-in-symbol-file 'built-in-symbol-file)
		 "doc.c"))

;; built-in macro documentation
(Assert (string= (built-in-symbol-file 'when)
		 "eval.c"))

;; #### we should handle symbols defined in Lisp, dumped, autoloaded,
;; and required, too.

