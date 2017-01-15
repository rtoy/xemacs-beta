;;; cl-seq.el --- Common Lisp extensions for XEmacs Lisp (part three)

;; Copyright (C) 1993 Free Software Foundation, Inc.
;; Copyright (C) 2010 Ben Wing.

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
;; This package works with Emacs 18, Emacs 19, and Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the Common Lisp sequence and list functions
;; which take keyword arguments.

;; See cl.el for Change Log.

;;; Code:

;; XEmacs; all the heavy lifting of this file is now in C. There's no need
;; for the cl-parsing-keywords macro. We could use defun* for the
;; keyword-parsing code, which would avoid the necessity of the arguments:
;; () lists in the docstrings, but that often breaks because of dynamic
;; scope (e.g. a variable called start bound in this file and one in a
;; user-supplied test predicate may well interfere with each other).

(defun remove-if (cl-predicate cl-seq &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQUENCE.

This is a non-destructive function; it makes a copy of SEQUENCE if necessary
to avoid corrupting the original SEQUENCE.  If SEQUENCE is a list, the copy
may share list structure with SEQUENCE.  If no item satisfies PREDICATE,
SEQUENCE itself is returned, unmodified.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'IDENTITY) (START 0) END FROM-END COUNT)"
  (apply 'remove* 'remove* cl-seq :if cl-predicate cl-keys))

(defun remove-if-not (cl-predicate cl-seq &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQUENCE.

This is a non-destructive function; it makes a copy of SEQUENCE if necessary
to avoid corrupting the original SEQUENCE.  If SEQUENCE is a list, the copy
may share list structure with SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'IDENTITY) (START 0) END FROM-END COUNT)"
  (apply 'remove* 'remove* cl-seq :if-not cl-predicate cl-keys))

(defun delete-if (cl-predicate cl-seq &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQUENCE.

This is a destructive function; if SEQUENCE is a list, it reuses its
storage.  If SEQUENCE is an array and some element satisfies PREDICATE, a
copy is always returned.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'IDENTITY) (START 0) END FROM-END COUNT)"
  (apply 'delete* 'delete* cl-seq :if cl-predicate cl-keys))

(defun delete-if-not (cl-predicate cl-seq &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQUENCE.

This is a destructive function; it reuses the storage of SEQUENCE whenever
possible.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'IDENTITY) (START 0) END FROM-END COUNT)"
  (apply 'delete* 'delete* cl-seq :if-not cl-predicate cl-keys))

(defun substitute-if (cl-new cl-predicate cl-seq &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQUENCE.

This is a non-destructive function; it makes a copy of SEQUENCE if necessary
to avoid corrupting the original SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (NEW PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END COUNT FROM-END)"
  (apply 'substitute cl-new 'substitute cl-seq :if cl-predicate cl-keys))

(defun substitute-if-not (cl-new cl-predicate cl-seq &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQUENCE.

This is a non-destructive function; it makes a copy of SEQUENCE if necessary
to avoid corrupting the original SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (NEW PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END COUNT FROM-END)"
  (apply 'substitute cl-new 'substitute cl-seq :if-not cl-predicate
         cl-keys))

(defun nsubstitute-if (cl-new cl-predicate cl-seq &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQUENCE.

This is destructive function; it modifies SEQUENCE directly, never returning
a copy.  See `substitute-if' for a non-destructive version.

See `remove*' for the meaning of the keywords.

arguments: (NEW PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END COUNT FROM-END)"
  (apply 'nsubstitute cl-new 'nsubstitute cl-seq :if cl-predicate
         cl-keys))

(defun nsubstitute-if-not (cl-new cl-predicate cl-seq &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQUENCE.

This is destructive function; it modifies SEQUENCE directly, never returning
a copy.  See `substitute-if-not' for a non-destructive version.

See `remove*' for the meaning of the keywords.

arguments: (NEW PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END COUNT FROM-END)"
  (apply 'nsubstitute cl-new 'nsubstitute cl-seq :if-not cl-predicate
         cl-keys))

(defun find-if (cl-predicate cl-seq &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQUENCE.

Return the matching item, or DEFAULT (not a keyword specified for this
function by Common Lisp) if not found.

See `remove*' for the meaning of the other keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END DEFAULT)"
  (apply 'find 'find cl-seq :if cl-predicate cl-keys))

(defun find-if-not (cl-predicate cl-seq &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQUENCE.

Return the matching ITEM, or DEFAULT (not a keyword specified for this
function by Common Lisp) if not found.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END DEFAULT)"
  (apply 'find 'find cl-seq :if-not cl-predicate cl-keys))

(defun position-if (cl-predicate cl-seq &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQUENCE.

Return the index of the matching item, or nil if not found.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END)"
  (apply 'position 'position cl-seq :if cl-predicate cl-keys))

(defun position-if-not (cl-predicate cl-seq &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQUENCE.

Return the index of the matching item, or nil if not found.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END)"
  (apply 'position 'position cl-seq :if-not cl-predicate cl-keys))

(defun count-if (cl-predicate cl-seq &rest cl-keys)
  "Count the number of items satisfying PREDICATE in SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END)"
  (apply 'count 'count cl-seq :if cl-predicate cl-keys))

(defun count-if-not (cl-predicate cl-seq &rest cl-keys)
  "Count the number of items not satisfying PREDICATE in SEQUENCE.

See `remove*' for the meaning of the keywords.

arguments: (PREDICATE SEQUENCE &key (KEY #'identity) (START 0) END FROM-END)"
  (apply 'count 'count cl-seq :if-not cl-predicate cl-keys))

(defun stable-sort (cl-seq cl-predicate &rest cl-keys)
  "Sort the argument SEQUENCE stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQUENCE if possible.
Keywords supported:  :key
:key specifies a one-argument function that transforms elements of SEQUENCE
into \"comparison keys\" before the test predicate is applied.  See
`member*' for more information.

arguments: (SEQUENCE PREDICATE &key (KEY #'identity))"
  (apply 'sort* cl-seq cl-predicate cl-keys))

(defun member-if (cl-predicate cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
See `member*' for the meaning of :key.

arguments: (PREDICATE LIST &key (KEY #'identity))"
  (apply 'member* 'member* cl-list :if cl-predicate cl-keys))

(defun member-if-not (cl-predicate cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
See `member*' for the meaning of :key.

arguments: (PREDICATE LIST &key (KEY #'identity))"
  (apply 'member* 'member* cl-list :if-not cl-predicate cl-keys))

(defun assoc-if (cl-predicate cl-alist &rest cl-keys)
  "Return the first item whose car satisfies PREDICATE in ALIST.
See `member*' for the meaning of :key.

arguments: (PREDICATE ALIST &key (KEY #'identity))"
  (apply 'assoc* 'assoc* cl-alist :if cl-predicate cl-keys))

(defun assoc-if-not (cl-predicate cl-alist &rest cl-keys)
  "Return the first item whose car does not satisfy PREDICATE in ALIST.
See `member*' for the meaning of :key.

arguments: (PREDICATE ALIST &key (KEY #'identity))"
  (apply 'assoc* 'assoc* cl-alist :if-not cl-predicate cl-keys))

(defun rassoc-if (cl-predicate cl-alist &rest cl-keys)
  "Return the first item whose cdr satisfies PREDICATE in ALIST.
See `member*' for the meaning of :key.

arguments: (PREDICATE ALIST &key (KEY #'identity))"
  (apply 'rassoc* 'rassoc* cl-alist :if cl-predicate cl-keys))

(defun rassoc-if-not (cl-predicate cl-alist &rest cl-keys)
  "Return the first item whose cdr does not satisfy PREDICATE in ALIST.
See `member*' for the meaning of :key.

arguments: (PREDICATE ALIST &key (KEY #'identity))"
  (apply 'rassoc* 'rassoc* cl-alist :if-not cl-predicate cl-keys))

;; XEmacs addition: NOT IN COMMON LISP.
(defun stable-union (cl-list1 cl-list2 &rest cl-keys)
  "Stably combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
The result is \"stable\" in that it preserves the ordering of elements in
LIST1 and LIST2.  The result specifically consists of the elements in LIST1
in order, followed by any elements in LIST2 that are not also in LIST1, in
the order given in LIST2.

This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

See `union' for the meaning of :test, :test-not and :key.

NOTE: This is *NOT* a function defined by Common Lisp, but an XEmacs
extension.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)"
  ;; The standard `union' doesn't produce a "stable" union --
  ;; it iterates over the second list instead of the first one, and returns
  ;; the values in backwards order.  According to the CLTL2 documentation,
  ;; `union' is not required to preserve the ordering of elements in
  ;; any fashion, so we add a new function rather than changing the
  ;; semantics of `union'.
  (apply 'union cl-list1 cl-list2 :stable t cl-keys))

;; XEmacs addition: NOT IN COMMON LISP.
(defun stable-intersection (cl-list1 cl-list2 &rest cl-keys)
  "Stably combine LIST1 and LIST2 using a set-intersection operation.

The result list contains all items that appear in both LIST1 and LIST2.
The result is \"stable\" in that it preserves the ordering of elements in
LIST1 that are also in LIST2.

This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

See `union' for the meaning of :test, :test-not and :key.

NOTE: This is *NOT* a function defined by Common Lisp, but an XEmacs
extension.

arguments: (LIST1 LIST2 &key (TEST #'eql) (KEY #'identity) TEST-NOT)"
  ;; The standard `intersection' doesn't produce a "stable" intersection --
  ;; it iterates over the second list instead of the first one, and returns
  ;; the values in backwards order.  According to the CLTL2 documentation,
  ;; `intersection' is not required to preserve the ordering of elements in
  ;; any fashion, but it's trivial to implement a stable ordering in C,
  ;; given that the order of arguments to the test function is specified.
  (apply 'intersection cl-list1 cl-list2 :stable t cl-keys))

(defun subst-if (cl-new cl-predicate cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (non-destructively).

Return a copy of TREE with all matching elements replaced by NEW.  If no
element matches PREDICATE, return tree.

See `member*' for the meaning of :key.

arguments: (NEW PREDICATE TREE &key (KEY #'identity))"
  (apply 'subst cl-new 'subst cl-tree :if cl-predicate cl-keys))

(defun subst-if-not (cl-new cl-predicate cl-tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE.

Return a copy of TREE with all matching elements replaced by NEW.  If every
element matches PREDICATE, return tree.

See `member*' for the meaning of :key.

arguments: (NEW PREDICATE TREE &key (KEY #'identity))"
  (apply 'subst cl-new 'subst cl-tree :if-not cl-predicate cl-keys))

(defun nsubst-if (cl-new cl-predicate cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (destructively).

Any element of TREE which matches is changed to NEW (via a call to `setcar').

See `member*' for the meaning of :key.

arguments: (NEW PREDICATE TREE &key (KEY #'identity))"
  (apply 'nsubst cl-new 'nsubst cl-tree :if cl-predicate cl-keys))

(defun nsubst-if-not (cl-new cl-predicate cl-tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE (destructively).

Any element of TREE which matches is changed to NEW (via a call to `setcar').

See `member*' for the meaning of :key.

arguments: (NEW PREDICATE TREE &key (KEY #'identity))"
  (apply 'nsubst cl-new 'nsubst cl-tree :if-not cl-predicate cl-keys))

;;; arch-tag: ec1cc072-9006-4225-b6ba-d6b07ed1710c
;;; cl-seq.el ends here
