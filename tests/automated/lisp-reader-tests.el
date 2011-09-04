;; Copyright (C) 2005 Martin Kuehl.

;; Author: Martin Kuehl <martin.kuehl@gmail.com>
;; Maintainer: Martin Kuehl <martin.kuehl@gmail.com>
;; Created: 2005
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

;; Test the lisp reader.
;; See test-harness.el for instructions on how to run these tests.

;;; Raw Strings
;;; ===========

;; Equality to "traditional" strings
;; ---------------------------------
(dolist (strings '((#r"xyz"   "xyz")	 ; no backslashes
		   (#r"\xyz"  "\\xyz")   ; backslash at start
                   (#r"\\xyz" "\\\\xyz") ; backslashes at start
                   (#r"\nxyz" "\\nxyz")  ; escape seq. at start
                   (#r"\"xyz" "\\\"xyz") ; quote at start
                   (#r"xy\z"  "xy\\z")   ; backslash in middle
                   (#r"xy\\z" "xy\\\\z") ; backslashes in middle
                   (#r"xy\nz" "xy\\nz")  ; escape seq. in middle
                   (#r"xy\"z" "xy\\\"z") ; quote in middle
                   ;;(#r"xyz\"  "xyz\\")   ; backslash at end: error
                   (#r"xyz\\" "xyz\\\\") ; backslashes at end
                   (#r"xyz\n" "xyz\\n")  ; escape seq. at end
                   (#r"xyz\"" "xyz\\\"") ; quote at end
		   (#ru"\u00ABxyz" "\u00ABxyz") ; one Unicode escape
		   (#rU"\U000000ABxyz" "\U000000ABxyz") ; another Unicode escape
		   (#rU"xyz\u00AB" "xyz\u00AB") ; one Unicode escape
                   ))
  (Assert (apply #'string= strings)))

;; Odd number of backslashes at the end
;; ------------------------------------
(dolist (string '("#r\"xyz\\\""         ; `#r"abc\"': escaped delimiter
                  "#r\"xyz\\\\\\\""     ; `#r"abc\\\"': escaped delimiter
                  ))
  (with-temp-buffer
    (insert string)
    (Check-Error end-of-file (eval-buffer))))

;; Alternate string/regex delimiters
;; ---------------------------------
(dolist (string '("#r/xyz/"             ; Perl syntax
                  "#r:ix/xyz/"          ; Extended Perl syntax
                  "#r|xyz|"             ; TeX syntax
                  "#r[xyz]"             ; (uncommon) Perl syntax
                  "#r<xyz>"             ; Perl6 syntax?
                  "#r(xyz)"             ; arbitrary santax
                  "#r{xyz}"             ; arbitrary santax
                  "#r,xyz,"             ; arbitrary santax
                  "#r!xyz!"             ; arbitrary santax
                  ))
  (with-temp-buffer
    (insert string)
    (Check-Error-Message invalid-read-syntax "unrecognized raw string"
                         (eval-buffer))))

(when (featurep 'bignum)
  ;; This failed, up to 20110501.
  (Assert (eql (1+ most-positive-fixnum)
	       (read (format "+%d" (1+ most-positive-fixnum))))
	  "checking leading + is handled properly if reading a bignum")
  ;; This never did.
  (Assert (eql (1- most-positive-fixnum)
	       (read (format "+%d" (1- most-positive-fixnum))))
	  "checking leading + is handled properly if reading a fixnum"))

;; Test print-circle.
(let ((cons '#1=(1 2 3 4 5 6 . #1#))
      (vector #2=[1 2 3 4 5 6 #2#])
      (compiled-function #3=#[(argument) "\xc2\x09\x08\"\x87"
                              [pi argument #3#] 3])
      (char-table #4=#s(char-table :type generic :data (?\u0080 #4#)))
      (hash-table #5=#s(hash-table :test eql :data (a b c #5# e f)))
      (range-table #6=#s(range-table :type start-closed-end-open
                                     :data ((#x00 #xff) hello
                                            (#x100 #x1ff) #6#
                                            (#x200 #x2ff) everyone)))
      (print-readably t)
      (print-circle t)
      deserialized-cons deserialized-vector deserialized-compiled-function
      deserialized-char-table deserialized-hash-table deserialized-range-table)
  (Assert (eq (nthcdr 6 cons) cons)
          "checking basic recursive cons read properly")
  (Assert (eq vector (aref vector (1- (length vector))))
          "checking basic recursive vector read properly")
  (Assert (eq compiled-function
              (find-if #'compiled-function-p
                       (compiled-function-constants compiled-function)))
          "checking basic recursive compiled-function read properly")
  (Check-Error wrong-number-of-arguments (funcall compiled-function 3))
  (Assert (eq char-table (get-char-table ?\u0080 char-table))
          "checking basic recursive char table read properly")
  (Assert (eq hash-table (gethash 'c hash-table))
          "checking basic recursive hash table read properly")
  (Assert (eq range-table (get-range-table #x180 range-table))
          "checking basic recursive range table read properly")
  (setf (gethash 'g hash-table) cons
        (car cons) hash-table
        deserialized-hash-table (read (prin1-to-string hash-table)))
  (Assert (not (eq deserialized-hash-table hash-table))
          "checking printing and reading hash-table creates a new object")
  (Assert (eq deserialized-hash-table (gethash 'c deserialized-hash-table))
          "checking the lisp reader handles deserialized hash-table identity")
  (Assert (eq deserialized-hash-table
              (car (gethash 'g deserialized-hash-table)))
          "checking the reader handles deserialization identity, hash-table")
  (setf (get-char-table ?a char-table) cons
        (car cons) char-table
        deserialized-char-table (read (prin1-to-string char-table)))
  (Assert (not (eq deserialized-char-table char-table))
          "checking printing and reading creates a new object")
  (Assert (eq deserialized-char-table
              (get-char-table ?\u0080 deserialized-char-table))
          "checking the lisp reader handles deserialization identity")
  (Assert (eq deserialized-char-table
              (car (get-char-table ?a deserialized-char-table)))
          "checking the lisp reader handles deserialization identity, mixed")
  (put-range-table #x1000 #x1010 cons range-table)
  (setf (car cons) range-table
        deserialized-range-table (read (prin1-to-string range-table)))
  (Assert (not (eq deserialized-range-table range-table))
          "checking printing and reading creates a new object")
  (Assert (eq deserialized-range-table
              (get-range-table #x101 deserialized-range-table))
          "checking the lisp reader handles deserialization identity")
  (Assert (eq deserialized-range-table
              (car (get-range-table #x1001 deserialized-range-table)))
          "checking the lisp reader handles deserialization identity, mixed"))

