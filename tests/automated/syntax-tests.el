;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
;; Maintainer: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
;; Created: 1999
;; Keywords: tests

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
  (flet ((backward-up-list-moves-point-from-to (start expected-end)
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
    (Known-Bug-Expect-Failure
     (Assert (backward-up-list-moves-point-from-to 23 3)))
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

(loop
  with envvar-not-existing = (symbol-name (gensym "whatever"))
  with envvar-existing = (symbol-name (gensym "whatever"))
  with envvar-existing-val = (make-string #x10000 ?\xe1)
  with examples = 
  (list (list (format "%chome%cwhatever%c%chi-there%c$%s"
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      directory-sep-char
                      envvar-existing)
              (format "%chi-there%c%s"
                      directory-sep-char
                      directory-sep-char
                      envvar-existing-val))
        (if (memq system-type '(windows-nt cygwin32))
            '("//network-path/c$" "//network-path/c$")
          '("/network-path/c$" "/network-path/c$"))
        (list (format "/home/whoever/$%s" envvar-not-existing)
              (format "/home/whoever/$%s" envvar-not-existing))
        (list (format "/home/whoever/$%s" envvar-existing)
              (format "/home/whoever/%s" envvar-existing-val))
        (list (format "/home/whoever/${%s}" envvar-existing)
              (format "/home/whoever/%s" envvar-existing-val))
        (list (format "/home/whoever/${%s}" envvar-not-existing)
              (format "/home/whoever/${%s}" envvar-not-existing)))
  initially (progn (setenv envvar-not-existing nil t)
                   (setenv envvar-existing envvar-existing-val))
  for (pre post)
  in examples
  do 
  (Assert (string= post (substitute-in-file-name pre))))

