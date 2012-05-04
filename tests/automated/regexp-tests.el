;;; -*- coding: iso-8859-1 -*-

;; Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.
;; Copyright (C) 2010 Ben Wing.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2000
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

;; Test regular expressions.

;; NOTE NOTE NOTE: There is some domain overlap among case-tests.el,
;; regexp-tests.el and search-tests.el.  See case-tests.el.

(Check-Error-Message error "Trailing backslash"
		     (string-match "\\" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a++" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a**" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a???" "a"))
(Check-Error-Message error "Unmatched \\[ or \\[^"
		     (string-match "[" "a"))
(Check-Error-Message error "Unmatched \\[ or \\[^"
		     (string-match "[abc" "a"))
(Check-Error-Message error "Unmatched ) or \\\\)"
		     (string-match "\\)" "a"))
(Check-Error-Message error "Invalid regular expression"
		     (string-match "\\(?.\\)" "a"))
(Check-Error-Message error "Unmatched \\\\{"
		     (string-match "a\\{" "a"))
(Check-Error-Message error "Invalid content of \\\\{\\\\}"
		     (string-match "a\\{a\\}" "a"))

;; exactn

;; string-match
(with-temp-buffer
  ;; case-insensitive
  (Assert (string-match "ä" "ä"))
  (Assert (string-match "ä" "Ä"))
  (Assert (string-match "Ä" "Ä"))
  (Assert (string-match "Ä" "ä"))
  ;; case-sensitive
  (setq case-fold-search nil)
  (Assert (string-match "ä" "ä"))
  (Assert (not (string-match "ä" "Ä")))
  (Assert (string-match "Ä" "Ä"))
  (Assert (not (string-match "Ä" "ä"))))

;; Is posix-string-match passing the POSIX flag correctly?

(Assert
 (equal 
  (save-match-data
    (progn (posix-string-match "i\\|ii" "ii") (match-data)))
  '(0 2))
 "checking #'posix-string-match actually returns the longest match")

;; looking-at
(with-temp-buffer
  (insert "äÄ")
  ;; case-insensitive
  (goto-char (point-min))
  (Assert (looking-at "ä"))
  (Assert (looking-at "Ä"))
  (forward-char)
  (Assert (looking-at "ä"))
  (Assert (looking-at "Ä"))
  ;; case-sensitive
  (setq case-fold-search nil)
  (goto-char (point-min))
  (Assert (looking-at "ä"))
  (Assert (not (looking-at "Ä")))
  (forward-char)
  (Assert (not (looking-at "ä")))
  (Assert (looking-at "Ä")))

;; re-search-forward and re-search-backward
(with-temp-buffer
  (insert "äÄ")
  ;; case insensitive
  ;; forward
  (goto-char (point-min))
  ;; Avoid trivial regexp.
  (Assert (eq 2 (re-search-forward "ä\\|a" nil t)))
  (goto-char (point-min))
  (Assert (eq 2 (re-search-forward "Ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  ;; backward
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "ä\\|a" nil t)))
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "Ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "Ä\\|a" nil t)))
  ;; case sensitive
  (setq case-fold-search nil)
  ;; forward
  (goto-char (point-min))
  (Assert (eq 2 (re-search-forward "ä\\|a" nil t)))
  (goto-char (point-min))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (not (re-search-forward "ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  ;; backward
  (goto-char (point-max))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "Ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (not (re-search-backward "Ä\\|a" nil t))))

;; duplicate
(with-temp-buffer
  ;; case insensitive
  (Assert (string-match "^\\(ä\\)\\1$" "ää"))
  (Assert (string-match "^\\(ä\\)\\1$" "äÄ"))
  (Assert (string-match "^\\(ä\\)\\1$" "ÄÄ"))
  (Assert (string-match "^\\(ä\\)\\1$" "Ää"))
  (Assert (string-match "^\\(Ä\\)\\1$" "ää"))
  (Assert (string-match "^\\(Ä\\)\\1$" "äÄ"))
  (Assert (string-match "^\\(Ä\\)\\1$" "ÄÄ"))
  (Assert (string-match "^\\(Ä\\)\\1$" "Ää"))
  ;; case sensitive
  (setq case-fold-search nil)
  (Assert (string-match "^\\(ä\\)\\1$" "ää"))
  (Assert (not (string-match "^\\(ä\\)\\1$" "äÄ")))
  (Assert (not (string-match "^\\(ä\\)\\1$" "ÄÄ")))
  (Assert (not (string-match "^\\(ä\\)\\1$" "Ää")))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "ää")))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "äÄ")))
  (Assert (string-match "^\\(Ä\\)\\1$" "ÄÄ"))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "Ää"))))

;; multiple-match
;; Thanks to Manfred Bartz <MBartz@xix.com>
;; c.e.x <vn4rkkm7ouf3b5@corp.supernews.com>
;; #### Need to do repetitions of more complex regexps
;; #### WASH ME!
(with-temp-buffer
  (Assert (not (string-match "^a\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^a\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^a\\{4,4\\}$" "aaaaa")))
  (Assert (not (string-match "^[a]\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^[a]\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^[a]\\{4,4\\}$" "aaaaa")))
  (Assert (not (string-match "^\\(a\\)\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^\\(a\\)\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^\\(a\\)\\{4,4\\}$" "aaaaa")))
  ;; Use class because repetition of single char broken in 21.5.15
  (Assert (not (string-match "^[a]\\{3,5\\}$" "aa")))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaa"))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaaa"))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaaaa"))
  (Assert (not (string-match "^[a]\\{3,5\\}$" "aaaaaa")))
  (insert "\
aa
aaa
aaaa
aaaaa
aaaaaa
baaaa
")
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^a\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^a\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^a\\{4,4\\}$")))
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{4,4\\}$")))
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^\\(a\\)\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^\\(a\\)\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^\\(a\\)\\{4,4\\}$")))
  ;; Use class because repetition of single char broken in 21.5.15
  (goto-char (point-min))
  (Assert (not (looking-at "^[a]\\{3,5\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{3,5\\}$")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "b?a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "ba\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "[b]a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "\\(b\\)a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^a\\{4,4\\}$")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "[a]\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^[a]\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^[a]\\{4,4\\}$")))
  )

;; charset, charset_not
;; Not called because it takes too much time.
(defun test-regexp-charset-paranoid ()
  (let ((i 0)
	(max (expt 2 (if (featurep 'mule) 19 8)))
	(range "[a-z]")
	(range-not "[^a-z]")
	char string)
    (while (< i max)
      (when (setq char (int-to-char i))
	(setq string (char-to-string char))
	(if (or (and (<= 65 i)
		     (<= i 90))
		(and (<= 97 i)
		     (<= i 122)))
	    (progn
	      (Assert (string-match range string))
	      (Assert (not (string-match range-not string))))
	  (Assert (not (string-match range string)))
	  (Assert (string-match range-not string))))
      (setq i (1+ i)))))

;; (test-regexp-charset-paranoid)

;; charset_mule, charset_mule_not
;; Not called because it takes too much time.
(defun test-regex-charset-mule-paranoid ()
  (if (featurep 'mule)
      (let ((i 0)
	    (max (expt 2 19))
	    (range (format "[%c-%c]"
			   (make-char 'japanese-jisx0208 36 34)
			   (make-char 'japanese-jisx0208 36 42)))
	    (range-not (format "[^%c-%c]"
			       (make-char 'japanese-jisx0208 36 34)
			       (make-char 'japanese-jisx0208 36 42)))
	    (min-int (char-to-int (make-char 'japanese-jisx0208 36 34)))
	    (max-int (char-to-int (make-char 'japanese-jisx0208 36 42)))
	    char string)
	(while (< i max)
	  (when (setq char (int-to-char i))
	    (setq string (char-to-string char))
	    (if (and (<= min-int i)
		     (<= i max-int))
		(progn
		  (Assert (string-match range string))
		  (Assert (not (string-match range-not string))))
	      (Assert (not (string-match range string)))
	      (Assert (string-match range-not string))))
	  (setq i (1+ i))))))

;; (test-regex-charset-mule-paranoid)

;; Test that replace-match does not clobber registers after a failed match
(with-temp-buffer
  (insert "This is a test buffer.")
  (goto-char (point-min))
  (search-forward "this is a test ")
  (looking-at "Unmatchable text")
  (replace-match "")
  (Assert (looking-at "^buffer.$")))

;; Test that trivial regexps reset unused registers
;; Thanks to Martin Sternholm for the report.
;; xemacs-beta <5blm6h2ki5.fsf@lister.roxen.com>
(with-temp-buffer
  (insert "ab")
  (goto-char (point-min))
  (re-search-forward "\\(a\\)")
  ;; test the whole-match data, too -- one attempted fix scotched that, too!
  (Assert (string= (match-string 0) "a"))
  (Assert (string= (match-string 1) "a"))
  (re-search-forward "b")
  (Assert (string= (match-string 0) "b"))
  (Assert (string= (match-string 1) nil)))

;; Test word boundaries
(Assert (= (string-match "\\<a" " a") 1))
(Assert (= (string-match "a\\>" "a ") 0))
(Assert (= (string-match "\\ba" " a") 1))
(Assert (= (string-match "a\\b" "a ") 0))
;; should work at target boundaries
(Assert (= (string-match "\\<a" "a") 0))
(Assert (= (string-match "a\\>" "a") 0))
(Assert (= (string-match "\\ba" "a") 0))
(Assert (= (string-match "a\\b" "a") 0))
;; Check for weirdness
(Assert (not (string-match " \\> " "  ")))
(Assert (not (string-match " \\< " "  ")))
(Assert (not (string-match " \\b " "  ")))
;; but not if the "word" would be on the null side of the boundary!
(Assert (not (string-match "\\<" "")))
(Assert (not (string-match "\\>" "")))
(Assert (not (string-match " \\<" " ")))
(Assert (not (string-match "\\> " " ")))
(Assert (not (string-match "a\\<" "a")))
(Assert (not (string-match "\\>a" "a")))
;; Added Known-Bug 2002-09-09 sjt
;; Fixed bug 2003-03-21 sjt
(Assert (not (string-match "\\b" "")))
(Assert (not (string-match "\\b" " ")))
(Assert (not (string-match " \\b" " ")))
(Assert (not (string-match "\\b " " ")))

;; Character classes are broken in Mule as of 21.5.9
;; Added Known-Bug 2002-12-27
;; Fixed by Daiki Ueno 2003-03-24
(if (featurep 'mule)
    ;; note: (int-to-char 65) => ?A
    (let ((ch0 (make-char 'japanese-jisx0208 52 65))
	  (ch1 (make-char 'japanese-jisx0208 51 65)))
      (Assert (not (string-match "A" (string ch0))))
      (Assert (not (string-match "[A]" (string ch0))))
      (Assert (eq (string-match "[^A]" (string ch0)) 0))
      (Assert (not (string-match "@A" (string ?@ ch0))))
      (Assert (not (string-match "@[A]" (string ?@ ch0))))
      (Assert (eq (string-match "@[^A]" (string ?@ ch0)) 0))
      (Assert (not (string-match "@?A" (string ?@ ch0))))
      (Assert (not (string-match "A" (string ch1))))
      (Assert (not (string-match "[A]" (string ch1))))
      (Assert (eq (string-match "[^A]" (string ch1)) 0))
      (Assert (not (string-match "@A" (string ?@ ch1))))
      (Assert (not (string-match "@[A]" (string ?@ ch1))))
      (Assert (eq (string-match "@[^A]" (string ?@ ch1)) 0))
      (Assert (not (string-match "@?A" (string ?@ ch1))))
      )
  )

;; More stale match data tests.
;; Thanks to <bjacob@ca.metsci.com>.
;; These tests used to fail because we cleared match data only on success.
;; Fixed 2003-04-17.
;; Must change sense of failing tests 2003-05-09.  Too much code depends on
;; failed matches preserving match-data.
(let ((a "a"))
  (Assert (string= (progn (string-match "a" a)
			  (string-match "b" a)
			  (match-string 0 a))
		   a))
  (Assert (not (progn (string-match "a" a)
		      (string-match "b" a)
		      (match-string 1 a))))
  ;; test both for the second match is a plain string match and a regexp match
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "\\(b\\)" a)
			  (match-string 0 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "b" a)
			  (match-string 0 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "\\(b\\)" a)
			  (match-string 1 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "b" a)
			  (match-string 1 a))
		   a))
  ;; in 21.4.16, registers from num_shy_groups to num_groups were not cleared,
  ;; resulting in stale match data
  (Assert (progn (string-match "\\(a\\)" a)  
		 (string-match "\\(?:a\\)" a)  
		 (not (match-beginning 1))))
  )

;; bug identified by Katsumi Yamaoka 2004-09-03 <b9ywtzbbpue.fsf_-_@jpl.org>
;; fix submitted by sjt 2004-09-08
;; trailing comments are values from buggy 21.4.15
(let ((text "abc"))
  (Assert (eq 0 (string-match "\\(?:ab+\\)*c" text)))	; 2
  (Assert (eq 0 (string-match "^\\(?:ab+\\)*c" text)))	; nil
  (Assert (eq 0 (string-match "^\\(?:ab+\\)*" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:ab+\\)c" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:ab\\)*c" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:a+\\)*b" text)))	; nil
  (Assert (eq 0 (string-match "^\\(?:a\\)*b" text)))	; 0
)

;; per Steve Youngs 2004-09-30 <microsoft-free.87ekkjhj7t.fsf@youngs.au.com>
;; fix submitted by sjt 2004-10-07
;; trailing comments are values from buggy 21.4.pre16
(let ((text "abc"))
  (Assert (eq 0 (string-match "\\(?:a\\(b\\)\\)" text)))	; 0
  (Assert (string= (match-string 1 text) "b"))			; ab
  (Assert (null (match-string 2 text)))				; b
  (Assert (null (match-string 3 text)))				; nil
  (Assert (eq 0 (string-match "\\(?:a\\(?:b\\(c\\)\\)\\)" text)))	; 0
  (Assert (string= (match-string 1 text) "c"))			; abc
  (Assert (null (match-string 2 text)))				; ab
  (Assert (null (match-string 3 text)))				; c
  (Assert (null (match-string 4 text)))				; nil
)

;; trivial subpatterns and backreferences with shy groups
(let ((text1 "abb")
      (text2 "aba")
      (re0 "\\(a\\)\\(b\\)\\2")
      (re1 "\\(?:a\\)\\(b\\)\\2")
      (re2 "\\(?:a\\)\\(b\\)\\1")
      (re3 "\\(a\\)\\(?:b\\)\\1"))

  (Assert (eq 0 (string-match re0 text1)))
  (Assert (string= text1 (match-string 0 text1)))
  (Assert (string= "a" (match-string 1 text1)))
  (Assert (string= "b" (match-string 2 text1)))
  (Assert (null (string-match re0 text2)))

  (Check-Error-Message 'invalid-regexp "Invalid back reference"
		       (string-match re1 text1))

  (Assert (eq 0 (string-match re2 text1)))
  (Assert (string= text1 (match-string 0 text1)))
  (Assert (string= "b" (match-string 1 text1)))
  (Assert (null (match-string 2 text1)))
  (Assert (null (string-match re2 text2)))

  (Assert (null (string-match re3 text1)))
  (Assert (eq 0 (string-match re3 text2)))
  (Assert (string= text2 (match-string 0 text2)))
  (Assert (string= "a" (match-string 1 text2)))
  (Assert (null (match-string 2 text2)))
)

;; replace-regexp-in-string (regexp rep source
;;                           fixedcase literal buf-or-subexp start)

;; Currently we test the following cases:
;; where `cbuf' and `bar-or-empty' are bound below.

;; #### Tests for the various functional features (fixedcase, literal, start)
;; should be added.

(with-temp-buffer
  (flet ((bar-or-empty (subexp) (if (string= subexp "foo") "bar" "")))
    (let ((cbuf (current-buffer)))
      (dolist (test-case
               ;; REP           BUF-OR-SUBEXP   EXPECTED RESULT
               `(("bar"         nil             " bar")
                 ("bar"         ,cbuf           " bar")
                 ("bar"         0               " bar")
                 ("bar"         1               " bar foo")
                 (bar-or-empty  nil             " ")
                 (bar-or-empty  ,cbuf           " ")
                 (bar-or-empty  0               " ")
                 (bar-or-empty  1               " bar foo")))
        (Assert
         (string=
          (nth 2 test-case)
          (replace-regexp-in-string "\\(foo\\).*\\'" (nth 0 test-case)
                                    " foo foo" nil nil (nth 1 test-case)))))
      ;; #### Why doesn't this loop work right?
;       (dolist (test-case
;                ;; REP   BUF-OR-SUBEXP   EXPECTED ERROR		EXPECTED MESSAGE
;                `(;; expected message was "bufferp, symbol" up to 21.5.28
; 		 ("bar"     'symbol     wrong-type-argument	"integerp, symbol")
;                  ("bar"     -1          invalid-argument
; 						 "match data register invalid, -1")
;                  ("bar"     2           invalid-argument
; 						  "match data register not set, 2")
; 		 ))
;         (eval
; 	 `(Check-Error-Message ,(nth 2 test-case) ,(nth 3 test-case)
; 	    (replace-regexp-in-string "\\(foo\\).*\\'" ,(nth 0 test-case)
; 				      " foo foo" nil nil ,(nth 1 test-case)))))
      ;; #### Can't test the message with w-t-a, see test-harness.el.
      (Check-Error wrong-type-argument
		   (replace-regexp-in-string "\\(foo\\).*\\'"
					     "bar"
					     " foo foo" nil nil
					     'symbol))
      ;; #### Can't test the FROB (-1), see test-harness.el.
      (Check-Error-Message invalid-argument
			   "match data register invalid"
			   (replace-regexp-in-string "\\(foo\\).*\\'"
						     "bar"
						     " foo foo" nil nil
						     -1))
      ;; #### Can't test the FROB (-1), see test-harness.el.
      (Check-Error-Message invalid-argument
			   "match data register not set"
			   (replace-regexp-in-string "\\(foo\\).*\\'"
						     "bar"
						     " foo foo" nil nil
						     2))
      )))

;; Not very comprehensive tests of skip-chars-forward, skip-chars-background: 

(with-string-as-buffer-contents 
    "-]-----------------------------][]]------------------------"
  (goto-char (point-min))
  (skip-chars-forward (skip-chars-quote "-[]"))
  (Assert (= (point) (point-max)))
  (skip-chars-backward (skip-chars-quote "-[]"))
  (Assert (= (point) (point-min)))
  ;; Testing in passing for an old bug in #'skip-chars-forward where I
  ;; thought it was impossible to call it with a string containing only ?-
  ;; and ?]: 
  (Assert (= (skip-chars-forward (skip-chars-quote "-]"))
             (position ?[ (buffer-string) :test #'=)))
  ;; This used to error, incorrectly: 
  (Assert (skip-chars-quote "[-")))

;; replace-match (REPLACEMENT &optional FIXEDCASE LITERAL STRING STRBUFFER)

;; #### Write some tests!  Much functionality is implicitly tested above
;; via `replace-regexp-in-string', but we should specifically test bogus
;; combinations of STRING and STRBUFFER.

;; empty string at point
;; Thanks Julian Bradford on XEmacs Beta
;; <18652.54975.894512.880956@krk.inf.ed.ac.uk>
(with-string-as-buffer-contents "aáa"
  (goto-char (point-min))
  (Assert (looking-at "\\="))
  (Assert (= (re-search-forward "\\=") 1))
  (forward-char 1)
  (Assert (looking-at "\\="))
  (Assert (= (re-search-forward "\\=") 2))
  (forward-char 1)
  (Assert (looking-at "\\="))
  (Assert (= (re-search-forward "\\=") 3))
  (forward-char 1)
  (Assert (looking-at "\\="))
  (Assert (= (re-search-forward "\\=") 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Tests involving case-changing replace-match   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Assert (not (string-match "\\(\\.\\=\\)" ".")))
(Assert (string= "" (let ((str "test string"))
		      (if (string-match "^.*$" str)
			  (replace-match "\\U" t nil str)))))
(with-temp-buffer
  (erase-buffer)
  (insert "test string")
  (re-search-backward "^.*$")
  (replace-match "\\U" t)
  (Assert (and (bobp) (eobp))))

;; Control-1 characters were second-class citizens in regexp ranges
;; for a while there.  Addressed in Ben's Mercurial changeset
;; 2e15c29cc2b3; attempt to ensure this doesn't happen again.
(Assert (eql (string-match "[\x00-\x7f\x80-\x9f]" "a") 0))
(Assert (eql (string-match "[\x00-\x7f\x80-\x9f]" "é") nil))
;; Gave nil in 21.5 for a couple of years.
(Assert (eql (string-match "[\x00-\x7f\x80-\x9f]" "\x80") 0))
(Assert (eql (string-match "[\x00-\x7f]\\|[\x80-\x9f]" "\x80") 0))
;; Gave nil
(Assert (eql (string-match "[\x7f\x80-\x9f]" "\x80") 0))
(Assert (eql (string-match "[\x80-\x9f]" "\x80") 0))
(Assert (eql (string-match "[\x7f\x80-\x9e]" "\x80") 0))
;; Used to succeed even with the bug.
(Assert (eql (string-match "[\x7f\x80\x9f]" "\x80") 0))
(Assert (eql (string-match "[\x7e\x80-\x9f]" "\x80") 0))
(Assert (eql (string-match "[\x7f\x81-\x9f]" "\x81") 0))

;; Test character classes

;; This used not to error:
(Check-Error-Message invalid-regexp "Invalid character class name"
                     (string-match "[[:alnum12345:]]" "a"))
;; This alwayed errored, as long as character classes were turned on
(Check-Error-Message invalid-regexp "Invalid character class name"
                     (string-match "[[:alnum1234:]]" "a"))

(macrolet
    ((Assert-char-class (class matching-char non-matching-char)
       (if (and (not (featurep 'mule))
                (or (eq (car-safe matching-char) 'decode-char)
                    (eq (car-safe non-matching-char) 'decode-char)))
           ;; Don't attempt expansion if these clauses require Mule and we
           ;; don't have it.
           (return-from Assert-char-class nil)
         (setq matching-char (eval matching-char)
               non-matching-char (eval non-matching-char)))
       `(progn
         (Assert (eql (string-match ,(concat "[" class "]")
                                      ,(concat (string matching-char)
                                               (string non-matching-char)))
                      0))
         (Assert (eql (string-match ,(concat "[" class class class "]")
                                      ,(concat (string matching-char)
                                               (string non-matching-char)))
                      0))
         (Assert (eql (string-match ,(concat "[^" class "]")
                                      ,(concat (string non-matching-char)
                                               (string matching-char)))
                      0))
         (Assert (eql (string-match ,(concat "[^" class class class "]")
                                      ,(concat (string non-matching-char)
                                               (string matching-char)))
                      0))
         (Assert (eql (string-match ,(concat "[" class "]")
                                      ,(concat (string non-matching-char)
                                               (string matching-char)))
                      1))
         (Assert (eql (string-match ,(concat "[" class class class "]")
                                      ,(concat (string non-matching-char)
                                               (string matching-char)))
                      1))
         (Assert (eql (string-match ,(concat "[^" class "]")
                                      ,(concat (string matching-char)
                                               (string non-matching-char)))
                      1))
         (Assert (eql (string-match ,(concat "[^" class class class "]")
                                      ,(concat (string matching-char)
                                               (string non-matching-char)))
                      1))
         (Assert (null (string-match ,(concat "[" class "]")
                                     ,(string non-matching-char))))
         (Assert (null (string-match ,(concat "[^" class "]")
                                     ,(string matching-char))))
         (Assert (null (string-match ,(concat "[^" class
                                              (string non-matching-char) "]")
                                     ,(concat (string matching-char)
                                              (string non-matching-char)))))
         (let ((old-case-fold-search case-fold-search))
           (with-temp-buffer
             (setq case-fold-search old-case-fold-search)
             (insert-char ,matching-char 20)
             (insert-char ,non-matching-char 20)
             (goto-char (point-min))
             (Assert (eql (skip-chars-forward ,class) 20)
                     ,(format "making sure %s skips %S forward"
                              class matching-char))
             (Assert (eql (skip-chars-forward ,(concat "^" class)) 20)
                     ,(format "making sure ^%s skips %S forward"
                              class non-matching-char))
             (Assert (eql (skip-chars-backward ,(concat "^" class)) -20)
                     ,(format "making sure ^%s skips %S backward"
                              class non-matching-char))
             (Assert (eql (skip-chars-backward ,class) -20)
                     ,(format "making sure %s skips %S backward"
                              class matching-char))))))
     (Assert-never-matching (class &rest characters)
       (cons
        'progn
        (mapcan #'(lambda (character)
                    (if (or (not (eq 'decode-char (car-safe character)))
                            (featurep 'mule))
                        `((Assert (null (string-match
                                         ,(concat "[" class "]")
                                         ,(string (eval character)))))
                          (Assert (eql (string-match
                                        ,(concat "[^" class "]")
                                        ,(string (eval character)))
                                       0)))))
                characters))))
  (Assert-char-class "[:alpha:]" ?a ?0)
  (Assert-char-class "[:alpha:]" ?z ?9)
  (Assert-char-class "[:alpha:]" ?A ?0)
  (Assert-char-class "[:alpha:]" ?Z ?9)
  (Assert-char-class "[:alpha:]" ?b ?\x00)
  (Assert-char-class "[:alpha:]" ?c ?\x09)
  (Assert-char-class "[:alpha:]" ?d ?\ )
  (Assert-char-class "[:alpha:]" ?e ?\x7f)
  (Assert-char-class
   "[:alpha:]"
   (decode-char 'ucs #x0430)  ;; CYRILLIC SMALL LETTER A
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:alpha:]"
   (decode-char 'ucs #x0410)  ;; CYRILLIC CAPITAL LETTER A
   ?\x02)
  (Assert-char-class
   "[:alpha:]"
   (decode-char 'ucs #x03B2)  ;; GREEK SMALL LETTER BETA
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS

  (Assert-char-class "[:alnum:]" ?a ?.)
  (Assert-char-class "[:alnum:]" ?z ?')
  (Assert-char-class "[:alnum:]" ?A ?/)
  (Assert-char-class "[:alnum:]" ?Z ?!)
  (Assert-char-class "[:alnum:]" ?0 ?,)
  (Assert-char-class "[:alnum:]" ?9 ?\t)
  (Assert-char-class "[:alnum:]" ?b ?\x00)
  (Assert-char-class "[:alnum:]" ?c ?\x09)
  (Assert-char-class "[:alnum:]" ?d ?\   )
  (Assert-char-class "[:alnum:]" ?e ?\x7f)
  (Assert-char-class
   "[:alnum:]"
   (decode-char 'ucs #x0430)  ;; CYRILLIC SMALL LETTER A
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:alnum:]"
   (decode-char 'ucs #x0410)  ;; CYRILLIC CAPITAL LETTER A
   ?\x02)
  (Assert-char-class
   "[:alnum:]"
   (decode-char 'ucs #x03B2)  ;; GREEK SMALL LETTER BETA
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS

  (Assert-char-class "[:word:]" ?a ?.)
  (Assert-char-class "[:word:]" ?z ?')
  (Assert-char-class "[:word:]" ?A ?/)
  (Assert-char-class "[:word:]" ?Z ?!)
  (Assert-char-class "[:word:]" ?0 ?,)
  (Assert-char-class "[:word:]" ?9 ?\t)
  (Assert-char-class "[:word:]" ?b ?\x00)
  (Assert-char-class "[:word:]" ?c ?\x09)
  (Assert-char-class "[:word:]" ?d ?\   )
  (Assert-char-class "[:word:]" ?e ?\x7f)
  (Assert-char-class
   "[:word:]"
   (decode-char 'ucs #x0430)  ;; CYRILLIC SMALL LETTER A
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:word:]"
   (decode-char 'ucs #x0410)  ;; CYRILLIC CAPITAL LETTER A
   ?\x02)
  (Assert-char-class
   "[:word:]"
   (decode-char 'ucs #x03B2)  ;; GREEK SMALL LETTER BETA
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS

  (let ((case-fold-search nil))
    (Assert-char-class "[:upper:]" ?A ?a)
    (Assert-char-class "[:upper:]" ?Z ?z)
    (Assert-char-class "[:upper:]" ?B ?0)
    (Assert-char-class "[:upper:]" ?C ?9)
    (Assert-char-class "[:upper:]" ?D ?\x00)
    (Assert-char-class "[:upper:]" ?E ?\x09)
    (Assert-char-class "[:upper:]" ?F ?\ )
    (Assert-char-class "[:upper:]" ?G ?\x7f)
    (Assert-char-class
     "[:upper:]"
     (decode-char 'ucs #x0410)  ;; CYRILLIC CAPITAL LETTER A
     (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
    (Assert-char-class
     "[:upper:]"
     (decode-char 'ucs #x0392)  ;; GREEK CAPITAL LETTER BETA
     (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward

    (Assert-char-class "[:lower:]" ?a ?A)
    (Assert-char-class "[:lower:]" ?z ?Z)
    (Assert-char-class "[:lower:]" ?b ?0)
    (Assert-char-class "[:lower:]" ?c ?9)
    (Assert-char-class "[:lower:]" ?d ?\x00)
    (Assert-char-class "[:lower:]" ?e ?\x09)
    (Assert-char-class "[:lower:]" ?f ? )
    (Assert-char-class "[:lower:]" ?g ?\x7f)
    (Assert-char-class
     "[:lower:]"
     (decode-char 'ucs #x0430)  ;; CYRILLIC SMALL LETTER A
     (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
    (Assert-char-class
     "[:lower:]"
     (decode-char 'ucs #x03B2)  ;; GREEK SMALL LETTER BETA
     (decode-char 'ucs #x5357)));; kDefinition south; southern part; southward

  (let ((case-fold-search t))
    (Assert-char-class "[:upper:]" ?a ?\x00)
    (Assert-char-class "[:upper:]" ?z ?\x01)
    (Assert-char-class "[:upper:]" ?b ?{)
    (Assert-char-class "[:upper:]" ?c ?})
    (Assert-char-class "[:upper:]" ?d ?<)
    (Assert-char-class "[:upper:]" ?e ?>)
    (Assert-char-class "[:upper:]" ?f ?\ )
    (Assert-char-class "[:upper:]" ?g ?\x7f)
    (Assert-char-class
     "[:upper:]"
     (decode-char 'ucs #x0430)  ;; CYRILLIC SMALL LETTER A
     (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
    (Assert-char-class
     "[:upper:]"
     (decode-char 'ucs #x03B2)  ;; GREEK SMALL LETTER BETA
     (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward
    (Assert-char-class "[:lower:]" ?A ?\x00)
    (Assert-char-class "[:lower:]" ?Z ?\x01)
    (Assert-char-class "[:lower:]" ?B ?{)
    (Assert-char-class "[:lower:]" ?C ?})
    (Assert-char-class "[:lower:]" ?D ?<)
    (Assert-char-class "[:lower:]" ?E ?>)
    (Assert-char-class "[:lower:]" ?F ?\ )
    (Assert-char-class "[:lower:]" ?G ?\x7F)
    (Assert-char-class
     "[:lower:]"
     (decode-char 'ucs #x0410)  ;; CYRILLIC CAPITAL LETTER A
     (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
    (Assert-char-class
     "[:lower:]"
     (decode-char 'ucs #x0392)  ;; GREEK CAPITAL LETTER BETA
     (decode-char 'ucs #x5357)));; kDefinition south; southern part; southward

  (Assert-char-class "[:digit:]" ?0 ?a)
  (Assert-char-class "[:digit:]" ?9 ?z)
  (Assert-char-class "[:digit:]" ?1 ?A)
  (Assert-char-class "[:digit:]" ?2 ?Z)
  (Assert-char-class "[:digit:]" ?3 ?\x00)
  (Assert-char-class "[:digit:]" ?4 ?\x09)
  (Assert-char-class "[:digit:]" ?5 ? )
  (Assert-char-class "[:digit:]" ?6 ?\x7f)
  (Assert-char-class 
   "[:digit:]" ?7
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS
  (Assert-char-class
   "[:digit:]" ?8
   (decode-char 'ucs #x0392)) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:digit:]" ?9
   (decode-char 'ucs #x03B2)) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:digit:]" ?0
   (decode-char 'ucs #x0410)) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:digit:]" ?1
   (decode-char 'ucs #x0430)) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:digit:]" ?2
   (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:digit:]" ?3
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:digit:]" ?4
   (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward

  (Assert-char-class "[:xdigit:]" ?0 ?g)
  (Assert-char-class "[:xdigit:]" ?9 ?G)
  (Assert-char-class "[:xdigit:]" ?A ?{)
  (Assert-char-class "[:xdigit:]" ?a ?})
  (Assert-char-class "[:xdigit:]" ?1 ? )
  (Assert-char-class "[:xdigit:]" ?2 ?Z)
  (Assert-char-class "[:xdigit:]" ?3 ?\x00)
  (Assert-char-class "[:xdigit:]" ?4 ?\x09)
  (Assert-char-class "[:xdigit:]" ?5 ?\x7f)
  (Assert-char-class "[:xdigit:]" ?6 ?z)
  (Assert-char-class 
   "[:xdigit:]" ?7
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS
  (Assert-char-class
   "[:xdigit:]" ?8
   (decode-char 'ucs #x0392)) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:xdigit:]" ?9
   (decode-char 'ucs #x03B2)) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:xdigit:]" ?a
   (decode-char 'ucs #x0410)) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:xdigit:]" ?B
   (decode-char 'ucs #x0430)) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:xdigit:]" ?c
   (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:xdigit:]" ?D
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:xdigit:]" ?e
   (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward

  (Assert-char-class "[:space:]" ?\  ?0)
  (Assert-char-class "[:space:]" ?\t ?9)
  (Assert-char-class "[:space:]" ?\  ?A)
  (Assert-char-class "[:space:]" ?\t ?Z)
  (Assert-char-class "[:space:]" ?\  ?\x00)
  (Assert-char-class "[:space:]" ?\  ?\x7f)
  (Assert-char-class "[:space:]" ?\t ?a)
  (Assert-char-class "[:space:]" ?\  ?z)
  (Assert-char-class 
   "[:space:]" ?\ 
   (decode-char 'ucs #x0385)) ;; GREEK DIALYTIKA TONOS
  (Assert-char-class
   "[:space:]" ?\t
   (decode-char 'ucs #x0392)) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:space:]" ?\ 
   (decode-char 'ucs #x03B2)) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:space:]" ?\t
   (decode-char 'ucs #x0410)) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:space:]" ?\ 
   (decode-char 'ucs #x0430)) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:space:]" ?\t
   (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:space:]" ?\ 
   (decode-char 'ucs #x2116)) ;; NUMERO SIGN
  (Assert-char-class
   "[:space:]" ?\t
   (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward

  (Assert-char-class "[:print:]" ?\  ?\x00)
  (Assert-char-class "[:print:]" ?0 ?\x09)
  (Assert-char-class "[:print:]" ?9 ?\x7f)
  (Assert-char-class "[:print:]" ?A ?\x01)
  (Assert-char-class "[:print:]" ?Z ?\x02)
  (Assert-char-class "[:print:]" ?B ?\t)
  (Assert-char-class "[:print:]" ?a ?\x03)
  (Assert-char-class "[:print:]" ?z ?\x04)
  (Assert-char-class 
   "[:print:]" (decode-char 'ucs #x0385) ;; GREEK DIALYTIKA TONOS
   ?\x05)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x0392) ;; GREEK CAPITAL LETTER BETA
   ?\x06)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x03B2) ;; GREEK SMALL LETTER BETA
   ?\x07)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x0410) ;; CYRILLIC CAPITAL LETTER A
   ?\x08)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x0430) ;; CYRILLIC SMALL LETTER A
   ?\x09)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x0686) ;; ARABIC LETTER TCHEH
   ?\x0a)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x2116) ;; NUMERO SIGN
   ?\x0b)
  (Assert-char-class
   "[:print:]" (decode-char 'ucs #x5357) ;; kDefinition south; southern part; southward
   ?\x0c)

  (Assert-char-class "[:graph:]" ?!  ?\ )
  (Assert-char-class "[:graph:]" ?0 ?\x09)
  (Assert-char-class "[:graph:]" ?9 ?\x7f)
  (Assert-char-class "[:graph:]" ?A ?\x01)
  (Assert-char-class "[:graph:]" ?Z ?\x02)
  (Assert-char-class "[:graph:]" ?B ?\t)
  (Assert-char-class "[:graph:]" ?a ?\x03)
  (Assert-char-class "[:graph:]" ?z ?\x04)
  (Assert-char-class 
   "[:graph:]" (decode-char 'ucs #x0385) ;; GREEK DIALYTIKA TONOS
   ?\x05)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x0392) ;; GREEK CAPITAL LETTER BETA
   ?\x06)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x03B2) ;; GREEK SMALL LETTER BETA
   ?\x07)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x0410) ;; CYRILLIC CAPITAL LETTER A
   ?\x08)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x0430) ;; CYRILLIC SMALL LETTER A
   ?\x09)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x0686) ;; ARABIC LETTER TCHEH
   ?\x0a)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x2116) ;; NUMERO SIGN
   ?\x0b)
  (Assert-char-class
   "[:graph:]" (decode-char 'ucs #x5357) ;; kDefinition south; southern part; southward
   ?\x0c)

  (Assert-char-class "[:punct:]" ?\( ?0)
  (Assert-char-class "[:punct:]" ?. ?9)
  (Assert-char-class "[:punct:]" ?{ ?A)
  (Assert-char-class "[:punct:]" ?} ?Z)
  (Assert-char-class "[:punct:]" ?: ?\t)
  (Assert-char-class "[:punct:]" ?\; ?\x00)
  (Assert-char-class "[:punct:]" ?< ?\x09)
  (Assert-char-class "[:punct:]" ?> ?\x7f)
  (Assert-char-class "[:punct:]" ?= ?a)
  (Assert-char-class "[:punct:]" ?\? ?z)
  (Assert-char-class 
   "[:punct:]"
   (decode-char 'ucs #x0385) ;; GREEK DIALYTIKA TONOS
   ?a)
  (Assert-char-class
   "[:punct:]"
   (decode-char 'ucs #x20af)  ;; DRACHMA SIGN
   (decode-char 'ucs #x0392)) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:punct:]"
   (decode-char 'ucs #x00a7)  ;; SECTION SIGN
   (decode-char 'ucs #x03B2)) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:punct:]"
   (decode-char 'ucs #x00a8)  ;; DIAERESIS
   (decode-char 'ucs #x0410)) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:punct:]"
   (decode-char 'ucs #x0384) ;; GREEK TONOS
   (decode-char 'ucs #x0430)) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:punct:]" 
   (decode-char 'ucs #x00b7)  ;; MIDDLE DOT
   (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:punct:]" 
   (decode-char 'ucs #x2116) ;; NUMERO SIGN
   ?x)
  (Assert-char-class
   "[:punct:]"
   ?=
   (decode-char 'ucs #x5357)) ;; kDefinition south; southern part; southward

  (Assert-char-class "[:ascii:]" ?a (decode-char 'ucs #x00a7)) ;; SECTION SIGN
  (Assert-char-class "[:ascii:]" ?b (decode-char 'ucs #x00a8))  ;; DIAERESIS
  (Assert-char-class "[:ascii:]" ?c (decode-char 'ucs #x00b7))  ;; MIDDLE DOT
  (Assert-char-class "[:ascii:]" ?d (decode-char 'ucs #x0384))  ;; GREEK TONOS
  (Assert-char-class
   "[:ascii:]" ?\x00 (decode-char 'ucs #x0392)) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:ascii:]" ?\x01 (decode-char 'ucs #x03B2)) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:ascii:]" ?\t (decode-char 'ucs #x0410)) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:ascii:]" ?A (decode-char 'ucs #x0430)) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:ascii:]" ?B (decode-char 'ucs #x0686)) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:ascii:]" ?C (decode-char 'ucs #x20af)) ;; DRACHMA SIGN
  (Assert-char-class
   "[:ascii:]" ?\x7f (decode-char 'ucs #x2116)) ;; NUMERO SIGN

  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x00a7) ?a) ;; SECTION SIGN
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x00a8) ?b) ;; DIAERESIS
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x00b7) ?c) ;; MIDDLE DOT
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x0384) ?d) ;; GREEK TONOS
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x0392) ?\x00) ;; GREEK CAPITAL LETTER BETA
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x03B2) ?\x01) ;; GREEK SMALL LETTER BETA
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x0410) ?\t) ;; CYRILLIC CAPITAL LETTER A
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x0430) ?A) ;; CYRILLIC SMALL LETTER A
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x0686) ?B) ;; ARABIC LETTER TCHEH
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x20af) ?C) ;; DRACHMA SIGN
  (Assert-char-class
   "[:nonascii:]" (decode-char 'ucs #x2116) ?\x7f) ;; NUMERO SIGN

  (Assert-char-class
   "[:multibyte:]"  (decode-char 'ucs #x00a7) ?a) ;; SECTION SIGN
  (Assert-char-class
   "[:multibyte:]"  (decode-char 'ucs #x00a8) ?b) ;; DIAERESIS
  (Assert-char-class
   "[:multibyte:]"  (decode-char 'ucs #x00b7) ?c) ;; MIDDLE DOT
  (Assert-char-class
   "[:multibyte:]"  (decode-char 'ucs #x0384) ?d) ;; GREEK TONOS
  (Assert-char-class
   "[:multibyte:]"  (decode-char 'ucs #x0392)
   ?\x00) ;; GREEK CAPITAL LETTER BETA

  (Assert-never-matching
   "[:unibyte:]"
   ?\x80 ?\xe4 ?\xdf ?\xf8
   (decode-char 'ucs #x03B2) ;; GREEK SMALL LETTER BETA
   (decode-char 'ucs #x0410) ;; CYRILLIC CAPITAL LETTER A
   (decode-char 'ucs #x0430) ;; CYRILLIC SMALL LETTER A
   (decode-char 'ucs #x0686) ;; ARABIC LETTER TCHEH
   (decode-char 'ucs #x20af) ;; DRACHMA SIGN
   (decode-char 'ucs #x2116) ;; NUMERO SIGN
   (decode-char 'ucs #x5357))) ;; kDefinition south; southern part; southward

