;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Mike Sperber <mike@xemacs.org>
;; Maintainer: Mike Sperber <mike@xemacs.org>
;; Created: 2002
;; Keywords: tests, database

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test implementation of weak boxes, ephemerons, and weak lists
;;; See test-harness.el

(condition-case err
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

(when test-harness-test-compiled
  ;; this ha-a-ack depends on the compiled test coming last
  (setq test-harness-failure-tag
	"KNOWN BUG - fix reverted; after 2003-10-31 bitch at stephen\n"))

(garbage-collect)

;; tests for weak-boxes
(let ((w (make-weak-box (cons 2 3))))
  (Assert (equal (cons 2 3) (weak-box-ref w)))
  (garbage-collect)
  (Assert (not (weak-box-ref w))))

(garbage-collect)

;; tests for ephemerons
(let* ((p (cons 3 4))
       (finalized-p nil)
       (eph1 (make-ephemeron (cons 1 2) p
			     '(lambda (value)
				(setq finalized-p t))))
       (eph2 (make-ephemeron p p)))
  (Assert (eq p (ephemeron-ref (make-ephemeron (cons 1 2) p))))
  (Assert (ephemeron-p (make-ephemeron (cons 1 2) p)))

  (garbage-collect)
  (garbage-collect) ; ensure the post-gc hook runs

  (Assert finalized-p)
  (Assert (not (ephemeron-ref eph1)))

  (garbage-collect)
  
  (Assert (eq p (ephemeron-ref eph2))))

(garbage-collect)

;; tests for simple weak-lists
(let* ((a (cons 23 42))
       (b (cons 42 65))
       (testlist (list a b))
       (weaklist1 (make-weak-list 'simple))
       (weaklist2 (make-weak-list 'simple))
       (weaklist3 (make-weak-list 'simple))
       (weaklist4 (make-weak-list 'simple)))
  (set-weak-list-list weaklist1 testlist)
  (set-weak-list-list weaklist2 (list (cons 1 2) a b))
  (set-weak-list-list weaklist3 (list a (cons 1 2) b))
  (set-weak-list-list weaklist4 (list a b (cons 1 2)))
  (Assert (weak-list-p weaklist1))
  (Assert (eq (weak-list-type weaklist1) 'simple))
  (Assert (weak-list-p weaklist2))
  (Assert (eq (weak-list-type weaklist2) 'simple))
  (Assert (weak-list-p weaklist3))
  (Assert (eq (weak-list-type weaklist3) 'simple))
  (Assert (weak-list-p weaklist4))
  (Assert (eq (weak-list-type weaklist4) 'simple))

  (garbage-collect)

  (Assert (eq (weak-list-list weaklist1) testlist))
  (Assert (equal (weak-list-list weaklist2) testlist))
  (Assert (equal (weak-list-list weaklist3) testlist))
  (Assert (equal (weak-list-list weaklist4) testlist)))

(garbage-collect)

;; tests for assoc weak-lists
(let* ((a (cons 23 42))
       (b (cons a a))
       (testlist (list b b))
       (weaklist1 (make-weak-list 'assoc))
       (weaklist2 (make-weak-list 'assoc))
       (weaklist3 (make-weak-list 'assoc))
       (weaklist4 (make-weak-list 'assoc)))
  (set-weak-list-list weaklist1 testlist)
  (set-weak-list-list weaklist2 (list b (cons (cons 1 2) a) b))
  (set-weak-list-list weaklist3 (list b (cons a (cons 1 2)) b))
  (set-weak-list-list weaklist4 (list b (cons (cons 1 2) (cons 3 4)) b))
  (Assert (weak-list-p weaklist1))
  (Assert (eq (weak-list-type weaklist1) 'assoc))
  (Assert (weak-list-p weaklist2))
  (Assert (eq (weak-list-type weaklist2) 'assoc))
  (Assert (weak-list-p weaklist3))
  (Assert (eq (weak-list-type weaklist3) 'assoc))
  (Assert (weak-list-p weaklist4))
  (Assert (eq (weak-list-type weaklist4) 'assoc))

  (garbage-collect)

  (Assert (eq (weak-list-list weaklist1) testlist))
  (Assert (equal (weak-list-list weaklist2) testlist))
  (Assert (equal (weak-list-list weaklist3) testlist))
  (Assert (equal (weak-list-list weaklist4) testlist)))

(garbage-collect)

;; tests for key-assoc weak-lists
(let* ((a (cons 23 42))
       (b (cons a a))
       (testlist (list b b))
       (weaklist1 (make-weak-list 'key-assoc))
       (weaklist2 (make-weak-list 'key-assoc))
       (weaklist3 (make-weak-list 'key-assoc))
       (weaklist4 (make-weak-list 'key-assoc)))
  (set-weak-list-list weaklist1 testlist)
  (set-weak-list-list weaklist2 (list b (cons (cons 1 2) a) b))
  (set-weak-list-list weaklist3 (list b (cons a (cons 1 2)) b))
  (set-weak-list-list weaklist4 (list b (cons (cons 1 2) (cons 3 4)) b))
  (Assert (weak-list-p weaklist1))
  (Assert (eq (weak-list-type weaklist1) 'key-assoc))
  (Assert (weak-list-p weaklist2))
  (Assert (eq (weak-list-type weaklist2) 'key-assoc))
  (Assert (weak-list-p weaklist3))
  (Assert (eq (weak-list-type weaklist3) 'key-assoc))
  (Assert (weak-list-p weaklist4))
  (Assert (eq (weak-list-type weaklist4) 'key-assoc))

  (garbage-collect)

  (Assert (eq (weak-list-list weaklist1) testlist))
  (Assert (equal (weak-list-list weaklist2) testlist))
  (Assert (equal (weak-list-list weaklist3) (list b (cons a (cons 1 2)) b)))
  (Assert (equal (weak-list-list weaklist4) testlist)))

(garbage-collect)

;; tests for value-assoc weak-lists
(let* ((a (cons 23 42))
       (b (cons a a))
       (testlist (list b b))
       (weaklist1 (make-weak-list 'value-assoc))
       (weaklist2 (make-weak-list 'value-assoc))
       (weaklist3 (make-weak-list 'value-assoc))
       (weaklist4 (make-weak-list 'value-assoc)))
  (set-weak-list-list weaklist1 testlist)
  (set-weak-list-list weaklist2 (list b (cons (cons 1 2) a) b))
  (set-weak-list-list weaklist3 (list b (cons a (cons 1 2)) b))
  (set-weak-list-list weaklist4 (list b (cons (cons 1 2) (cons 3 4)) b))
  (Assert (weak-list-p weaklist1))
  (Assert (eq (weak-list-type weaklist1) 'value-assoc))
  (Assert (weak-list-p weaklist2))
  (Assert (eq (weak-list-type weaklist2) 'value-assoc))
  (Assert (weak-list-p weaklist3))
  (Assert (eq (weak-list-type weaklist3) 'value-assoc))
  (Assert (weak-list-p weaklist4))
  (Assert (eq (weak-list-type weaklist4) 'value-assoc))

  (garbage-collect)

  (Assert (eq (weak-list-list weaklist1) testlist))
  (Assert (equal (weak-list-list weaklist2) (list b (cons (cons 1 2) a) b)))
  (Assert (equal (weak-list-list weaklist3) testlist))
  (Assert (equal (weak-list-list weaklist4) testlist)))

(garbage-collect)

;; tests for full-assoc weak-lists
(let* ((a (cons 23 42))
       (b (cons a a))
       (testlist (list b b))
       (weaklist1 (make-weak-list 'full-assoc))
       (weaklist2 (make-weak-list 'full-assoc))
       (weaklist3 (make-weak-list 'full-assoc))
       (weaklist4 (make-weak-list 'full-assoc)))
  (set-weak-list-list weaklist1 testlist)
  (set-weak-list-list weaklist2 (list b (cons (cons 1 2) a) b))
  (set-weak-list-list weaklist3 (list b (cons a (cons 1 2)) b))
  (set-weak-list-list weaklist4 (list b (cons (cons 1 2) (cons 3 4)) b))
  (Assert (weak-list-p weaklist1))
  (Assert (eq (weak-list-type weaklist1) 'full-assoc))
  (Assert (weak-list-p weaklist2))
  (Assert (eq (weak-list-type weaklist2) 'full-assoc))
  (Assert (weak-list-p weaklist3))
  (Assert (eq (weak-list-type weaklist3) 'full-assoc))
  (Assert (weak-list-p weaklist4))
  (Assert (eq (weak-list-type weaklist4) 'full-assoc))

  (garbage-collect)

  (Assert (eq (weak-list-list weaklist1) testlist))
  (Assert (equal (weak-list-list weaklist2) (list b (cons (cons 1 2) a) b)))
  (Assert (equal (weak-list-list weaklist3) (list b (cons a (cons 1 2)) b)))
  (Assert (equal (weak-list-list weaklist4) testlist)))

(garbage-collect)

;; test the intended functionality of the fixpoint iteration used for marking
;; weak data structures like the ephermerons. Basically this tests gc_internals
;; to work properly but it also ensures the ephemerons behave according to the
;; specification

(let* ((inner_cons (cons 1 2))
       (weak1 (make-ephemeron inner_cons
			      (make-ephemeron inner_cons
					      (cons 1 2)
					      '(lambda (v) t))
			      '(lambda (v) t))))
  (Assert (ephemeron-ref (ephemeron-ref weak1)))
  (garbage-collect)
  ;; assure the inner ephis are still there
  (Assert (ephemeron-ref (ephemeron-ref weak1)))
  ;; delete the key reference and force cleaning up the garbage
  (setq inner_cons (cons 3 4))
  (garbage-collect)
  (Assert (not (ephemeron-ref weak1)))
)

(garbage-collect)
