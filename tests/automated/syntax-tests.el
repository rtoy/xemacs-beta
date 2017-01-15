;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
;; Maintainer: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
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

;; Test syntax related functions.
;; Right now it tests scan_words using forward-word and backward-word.
;; See test-harness.el for instructions on how to run these tests.

;;; Notation
;; W:   word constituent character.
;; NW:  non word constituent character.
;; -!-: current point.
;; EOB: end of buffer
;; BOB: beginning of buffer.

;; Algorithm of scan_words is simple.  It just searches SW and then
;; moves to NW.  When with MULE, it also stops at word boundary.  Word
;; boundary is tricky and listing all possible cases will be huge.
;; Those test are omitted here as it doesn't affect core
;; functionality.

(defun test-forward-word (string stop)
  (goto-char (point-max))
  (let ((point (point)))
    (insert string)
    (goto-char point)
    (forward-word 1)
    (Assert (eq (point) (+ point stop)))))

(with-temp-buffer
  ;; -!- W NW
  (test-forward-word "W " 1)
  (test-forward-word "WO " 2)
  ;; -!- W EOB
  (test-forward-word "W" 1)
  (test-forward-word "WO" 2)
  ;; -!- NW EOB
  (test-forward-word " " 1)
  (test-forward-word " !" 2)
  ;; -!- NW W NW
  (test-forward-word " W " 2)
  (test-forward-word " WO " 3)
  (test-forward-word " !W " 3)
  (test-forward-word " !WO " 4)
  ;; -!- NW W EOB
  (test-forward-word " W" 2)
  (test-forward-word " WO" 3)
  (test-forward-word " !W" 3)
  (test-forward-word " !WO" 4))

(defun test-backward-word (string stop)
  (goto-char (point-min))
  (insert string)
  (let ((point (point)))
    (backward-word 1)
    (Assert (eq (point) (- point stop)))))

(with-temp-buffer
  ;; NW W -!-
  (test-backward-word " W" 1)
  (test-backward-word " WO" 2)
  ;; BOB W -!-
  (test-backward-word "W" 1)
  (test-backward-word "WO" 2)
  ;; BOB NW -!-
  ;; -!-NW EOB
  (test-backward-word " " 1)
  (test-backward-word " !" 2)
  ;; NW W NW -!-
  (test-backward-word " W " 2)
  (test-backward-word " WO " 3)
  (test-backward-word " W !" 3)
  (test-backward-word " WO !" 4)
  ;; BOB W NW -!-
  (test-backward-word "W " 2)
  (test-backward-word "WO " 3)
  (test-backward-word "W !" 3)
  (test-backward-word "WO !" 4))

;; Works like test-forward-word, except for the following:
;; after <string> is inserted, the syntax-table <apply-syntax>
;; is applied to position <apply-pos>.
;; <apply-pos> can be in the form (start . end), or can be a
;; character position.
(defun test-syntax-table (string apply-pos apply-syntax stop)
  ;; We don't necessarily have syntax-table properties ...
  (when (fboundp 'lookup-syntax-properties) ; backwards compatible kludge
    ;; ... and they may not be enabled by default if we do.
    (setq lookup-syntax-properties t)
    (goto-char (point-max))
    (unless (consp apply-pos)
      (setq apply-pos `(,apply-pos . ,(+ 1 apply-pos))))
    (let ((point (point)))
      (insert string)
      (put-text-property (+ point (car apply-pos)) (+ point (cdr apply-pos))
			 'syntax-table apply-syntax)
      (goto-char point)
      (forward-word 1)
      (Assert (eq (point) (+ point stop))))))

;; test syntax-table extents
(with-temp-buffer
  ;; Apply punctuation to word
  (test-syntax-table "WO" 1 `(,(syntax-string-to-code ".")) 1)
  ;; Apply word to punctuation
  (test-syntax-table "W." 1 `(,(syntax-string-to-code "w")) 2))

;; According to Ralf Angeli in
;; http://article.gmane.org/gmane.emacs.xemacs.beta/17353:
;; Using a fresh CVS checkout of XEmacs trunk the following snippet
;; returns "1" when evaluated whereas it returns "5" in GNU Emacs 21.3,
;; CVS GNU Emacs and XEmacs 21.4.15.
;; If `set-syntax-table' is used instead of `with-syntax-table', CVS
;; XEmacs returns "5" as well, so I suppose that there is a problem in
;; `with-syntax-table' or a function called by it.

;; Fixed 2007-03-25 Olivier Galibert <20070324221053.GA48218@dspnet.fr.eu.org>
(with-temp-buffer
  (with-syntax-table (make-syntax-table)
    (insert "foo bar")
    (backward-sexp 1)
    (Assert (eql (point) 5))))

;; Test forward-comment at buffer boundaries
;; #### The second Assert fails (once interpreted, once compiled) on 21.4.9
;; with sjt's version of Andy's syntax-text-property-killer patch.
(with-temp-buffer
  (Skip-Test-Unless (fboundp 'c-mode)
		    "c-mode unavailable"
		    "comment and parse-partial-sexp tests"
    (c-mode)
    
    (insert "// comment\n")
    (forward-comment -2)
    (Assert (eq (point) (point-min)))

    (let ((point (point)))
      (insert "/* comment */")
      (goto-char point)
      (forward-comment 2)
      (Assert (eq (point) (point-max)))

      ;; this last used to crash
      (parse-partial-sexp point (point-max)))))

;; Test backward-up-list
;; Known-Bug: report = Evgeny Zacjev ca 2005-12-01, confirm = Aidan Kehoe

(with-temp-buffer
  ;; We are now using the standard syntax table.  Thus there's no need to
  ;; worry about a bogus syntax setting, eg, in a Gnus Article buffer the
  ;; bug doesn't manifest.

  ;; value of point to the immediate left of this character
  ;;       0          1           2
  ;;       1234 56789 012 34567 890 12 3456 7
  (insert "a ( \"b (c\" (\"defg\") \")\") h\n")

  ;; #### This test should check *every* position.
  (labels ((backward-up-list-moves-point-from-to (start expected-end)
             (goto-char start)
             (backward-up-list 1)
             (= (point) expected-end)))
    (Known-Bug-Expect-Failure
     ;; Evgeny's case
     (Assert (backward-up-list-moves-point-from-to 16 12)))
    (Assert (backward-up-list-moves-point-from-to 19 12))
    (Assert (backward-up-list-moves-point-from-to 20 3))
    (Known-Bug-Expect-Failure
     (Assert (backward-up-list-moves-point-from-to 22 3)))
    (Known-Bug-Expect-Error scan-error
     (Assert (backward-up-list-moves-point-from-to 23 3))
     )
    (Assert (backward-up-list-moves-point-from-to 24 3))
    ;; This is maybe a little tricky, since we don't expect the position
    ;; check to happen -- so use an illegal expected position
    ;; I don't think there's any other way for this to fail that way,
    ;; barring hardware error....
    (Check-Error-Message syntax-error
			 "Unbalanced parentheses"
			 (backward-up-list-moves-point-from-to 25 nil))
    ;; special-case check that point didn't move
    (Assert (= (point) 25))))

;; Test inspired by Alan Mackenzie in <20110806200042.GA3406@acm.acm>
;; on xemacs-beta 2011-08-06.
;; Known to fail in r5531 (#1b054bc2ac40) plus some additional patches to
;; syntax code, and passes with Alan's suggested patch ca. r5545.
;; #### The results of these tests are empirically determined, and will
;; probably change as the syntax cache is documented and repaired.
(with-temp-buffer
  ;; buffer->syntax_cache in just-initialized state.
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "just initialized")
    (Assert (= 1 (nth 1 sci)) nil "just initialized")
    (Assert (= -1 (nth 2 sci)) nil "just initialized")
    (Assert (= -1 (nth 3 sci)) nil "just initialized"))
  ;; Alan's example uses ?/ not ?, but ?/ has Ssymbol syntax, which would
  ;; mean it is treated the same as the letters by forward-sexp.
  (insert ",regexp, {")
  ;; Insertion updates markers, but not the cache boundaries.
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "after main insert")
    (Assert (= 11 (nth 1 sci)) nil "after main insert")
    (Assert (= -1 (nth 2 sci)) nil "after main insert")
    (Assert (= -1 (nth 3 sci)) nil "after main insert"))
  ;; #### Interactively inserting in fundamental mode swaps marker positions!
  ;; Why?
  (insert "}")
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "after brace insert")
    (Assert (= 12 (nth 1 sci)) nil "after brace insert")
    (Assert (= -1 (nth 2 sci)) nil "after brace insert")
    (Assert (= -1 (nth 3 sci)) nil "after brace insert"))
  ;; Motion that ignores the cache should not update the cache.
  (goto-char (point-min))
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "after movement 0")
    (Assert (= 12 (nth 1 sci)) nil "after movement 0")
    (Assert (= -1 (nth 2 sci)) nil "after movement 0")
    (Assert (= -1 (nth 3 sci)) nil "after movement 0"))
  ;; Cache should be updated and global since no syntax-table property.
  (forward-sexp 1)
  (Assert (= (point) 8) nil "after 1st forward-sexp")
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "after 1st forward-sexp")
    (Assert (= 12 (nth 1 sci)) nil "after 1st forward-sexp")
    (Assert (= 1 (nth 2 sci)) nil "after 1st forward-sexp")
    (Assert (= 12 (nth 3 sci)) nil "after 1st forward-sexp"))
  ;; Adding the text property should invalidate the cache.
  (put-text-property 1 2 'syntax-table '(7))
  (let ((sci (syntax-cache-info)))
    (Assert (= 1 (nth 0 sci)) nil "after putting property")
    (Assert (= 1 (nth 1 sci)) nil "after putting property")
    (Assert (= -1 (nth 2 sci)) nil "after putting property")
    (Assert (= -1 (nth 3 sci)) nil "after putting property"))
  (put-text-property 8 9 'syntax-table '(7))
  (goto-char (point-min))
  ;; Motion that is stopped by a syntax-table property should impose
  ;; that property's region on the cache.
  (forward-sexp 1)
  (Assert (= (point) 9) nil "after 2d forward-sexp")
  (let ((sci (syntax-cache-info)))
    (Assert (= 8 (nth 0 sci)) nil "after 2d forward-sexp")
    (Assert (= 9 (nth 1 sci)) nil "after 2d forward-sexp")
    (Assert (= 8 (nth 2 sci)) nil "after 2d forward-sexp")
    (Assert (= 9 (nth 3 sci)) nil "after 2d forward-sexp"))
  ;; Narrowing warps point but does not affect the cache.
  (narrow-to-region 10 12)
  (Assert (= 10 (point)) nil "after narrowing")
  (let ((sci (syntax-cache-info)))
    (Assert (= 8 (nth 0 sci)) nil "after narrowing")
    (Assert (= 9 (nth 1 sci)) nil "after narrowing")
    (Assert (= 8 (nth 2 sci)) nil "after narrowing")
    (Assert (= 9 (nth 3 sci)) nil "after narrowing"))
  ;; Motion that is stopped by buffer's syntax table should capture
  ;; the largest region known to not contain a change of syntax-table
  ;; property.
  (forward-sexp 1)
  (let ((sci (syntax-cache-info)))
    (Assert (= 10 (nth 0 sci)) nil "after 3d forward-sexp")
    (Assert (= 12 (nth 1 sci)) nil "after 3d forward-sexp")
    (Assert (= 10 (nth 2 sci)) nil "after 3d forward-sexp")
    (Assert (= 12 (nth 3 sci)) nil "after 3d forward-sexp"))
  (widen)
  (goto-char (point-min))
  ;; Check that we still respect the syntax table properties.
  (forward-sexp 1)
  (Assert (= 9 (point)) nil "after widening"))

;; #### Add the recipe in <yxzfymklb6p.fsf@gimli.holgi.priv> on xemacs-beta.
;; You also need to do a DELETE or type SPC to get the crash in 21.5.24.
;http://list-archive.xemacs.org/pipermail/xemacs-beta/2006-February/008430.html

;;; end of syntax-tests.el
