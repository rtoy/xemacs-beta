;; Copyright (C) 2016 Free Software Foundation, Inc. -*- coding: utf-8 -*-

;; Author: Aidan Kehoe <kehoea@parhasard.net>
;; Maintainers: Aidan Kehoe <kehoea@parhasard.net>
;; Created: 2016
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

;; This file tests the code in doprnt.c, implementing #'format, especially
;; those changes to it made in autumn 2016.

;;-----------------------------------------------------
;; Basic tests, refactored from lisp-tests.el
;;-----------------------------------------------------
(Assert (string= (format "%d" 10) "10"))
(Assert (string= (format "%o" 8) "10"))
(Assert (string= (format "%b" 2) "10"))
(Assert (string= (format "%x" 31) "1f"))
(Assert (string= (format "%X" 31) "1F"))
(Assert (string= (format "%b" 0) "0"))
(Assert (string= (format "%b" 3) "11"))
;; MS-Windows uses +002 in its floating-point numbers.  #### We should
;; perhaps fix this, but writing our own floating-point support in doprnt.c
;; is very hard.
(Assert (or (string= (format "%e" 100) "1.000000e+02")
	    (string= (format "%e" 100) "1.000000e+002")))
(Assert (or (string= (format "%E" 100) "1.000000E+02")
	    (string= (format "%E" 100) "1.000000E+002")))
(Assert (or (string= (format "%E" 100) "1.000000E+02")
	    (string= (format "%E" 100) "1.000000E+002")))
(Assert (string= (format "%f" 100) "100.000000"))
(Assert (string= (format "%7.3f" 12.12345) " 12.123"))
(Assert (string= (format "%07.3f" 12.12345) "012.123"))
(Assert (string= (format "%-7.3f" 12.12345) "12.123 "))
(Assert (string= (format "%-07.3f" 12.12345) "12.123 "))
(Assert (string= (format "%g" 100.0) "100"))
(Assert (or (string= (format "%g" 0.000001) "1e-06")
	    (string= (format "%g" 0.000001) "1e-006")))
(Assert (string= (format "%g" 0.0001) "0.0001"))
(Assert (string= (format "%G" 100.0) "100"))
(Assert (or (string= (format "%G" 0.000001) "1E-06")
	    (string= (format "%G" 0.000001) "1E-006")))
(Assert (string= (format "%G" 0.0001) "0.0001"))

(Assert (string= (format "%2$d%1$d" 10 20) "2010"))
(Assert (string= (format "%-d" 10) "10"))
(Assert (string= (format "%-4d" 10) "10  "))
(Assert (string= (format "%+d" 10) "+10"))
(Assert (string= (format "%+d" -10) "-10"))
(Assert (string= (format "%+4d" 10) " +10"))
(Assert (string= (format "%+4d" -10) " -10"))
(Assert (string= (format "% d" 10) " 10"))
(Assert (string= (format "% d" -10) "-10"))
(Assert (string= (format "% 4d" 10) "  10"))
(Assert (string= (format "% 4d" -10) " -10"))
(Assert (string= (format "%0d" 10) "10"))
(Assert (string= (format "%0d" -10) "-10"))
(Assert (string= (format "%04d" 10) "0010"))
(Assert (string= (format "%04d" -10) "-010"))
(Assert (string= (format "%*d" 4 10) "  10"))
(Assert (string= (format "%*d" 4 -10) " -10"))
(Assert (string= (format "%*d" -4 10) "10  "))
(Assert (string= (format "%*d" -4 -10) "-10 "))

;; Pad chars. See also the Mule-specific tests.
(Assert (string= (format "%!\xa0*d" 4 10) "\xa0\xa0\x31\x30"))
(Assert (string= (format "%!\xa0*d" 4 -10) "\xa0-10"))
(Assert (string= (format "%!\xa0*\x64" -4 10) "10\xa0\xa0"))
(Assert (string= (format "%!\xa0*\x64" -4 -10) "-10\xa0"))
(Assert (string= (format "%#d" 10) "10"))
(Assert (string= (format "%#o" 8) "010"))
(Assert (string= (format "%#x" 16) "0x10"))
(Assert (or (string= (format "%#e" 100) "1.000000e+02")
	    (string= (format "%#e" 100) "1.000000e+002")))
(Assert (or (string= (format "%#E" 100) "1.000000E+02")
	    (string= (format "%#E" 100) "1.000000E+002")))
(Assert (string= (format "%#f" 100) "100.000000"))
(Assert (string= (format "%#g" 100.0) "100.000"))
(Assert (or (string= (format "%#g" 0.000001) "1.00000e-06")
	    (string= (format "%#g" 0.000001) "1.00000e-006")))
(Assert (string= (format "%#g" 0.0001) "0.000100000"))
(Assert (string= (format "%#G" 100.0) "100.000"))
(Assert (or (string= (format "%#G" 0.000001) "1.00000E-06")
	    (string= (format "%#G" 0.000001) "1.00000E-006")))
(Assert (string= (format "%#G" 0.0001) "0.000100000"))
(Assert (string= (format "%.1d" 10) "10"))
(Assert (string= (format "%.4d" 10) "0010"))
;; Combination of `-', `+', ` ', `0', `#', `.', `*'
(Assert (string= (format "%-04d" 10) "10  "))
(Assert (string= (format "%-*d" 4 10) "10  "))
;; #### Correctness of this behavior is questionable.
;; It might be better to signal error.
(Assert (string= (format "%-*d" -4 10) "10  "))
;; These behavior is not specified.
;; (format "%-+d" 10)
;; (format "%- d" 10)
;; (format "%-01d" 10)
;; (format "%-#4x" 10)
;; (format "%-.1d" 10)

(Assert (string= (format "%01.1d" 10) "10"))
(Assert (string= (format "%03.1d" 10) " 10"))
(Assert (string= (format "%01.3d" 10) "010"))
(Assert (string= (format "%1.3d" 10) "010"))
(Assert (string= (format "%3.1d" 10) " 10"))

;;; The following two tests used to use 1000 instead of 100,
;;; but that merely found buffer overflow bugs in Solaris sprintf().
(Assert (= 102 (length (format "%.100f" 3.14))))
(Assert (= 100 (length (format "%100f" 3.14))))

;;; Check for 64-bit cleanness on LP64 platforms.
(Assert (= (read (format "%d"  most-positive-fixnum)) most-positive-fixnum))
;; No longer true, %ld is explicitly 32-bits wide in Lisp format strings.
; (Assert (= (read (format "%ld" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%u"  most-positive-fixnum)) most-positive-fixnum))
;; See above.
; (Assert (= (read (format "%lu" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%d"  most-negative-fixnum)) most-negative-fixnum))
;; See above.
; (Assert (= (read (format "%ld" most-negative-fixnum)) most-negative-fixnum))

;; These used to crash. 
(Assert (eql (read (format "%f" 1.2e+302)) 1.2e+302))
(Assert (eql (read (format "%.1000d" 1)) 1))

;;; "%u" is undocumented, and Emacs Lisp has no unsigned type.
;;; What to do if "%u" is used with a negative number?
;;; For non-bignum XEmacsen, the most reasonable thing seems to be to print an
;;; un-read-able number.  The printed value might be useful to a human, if not
;;; to Emacs Lisp.
;;; For bignum XEmacsen, we make %u with a negative value throw an error.
(if (featurep 'bignum)
    (progn
      (Check-Error wrong-type-argument (format "%u" most-negative-fixnum))
      (Check-Error wrong-type-argument (format "%u" -1)))
  (Check-Error invalid-argument (read (format "%u" most-negative-fixnum)))
  (Check-Error invalid-argument (read (format "%u" -1))))

(Assert (equal (format "%c" ?a) "a"))
(Assert (equal (format "%c" (char-int ?A)) "A"))
(Assert (equal (format "%20c" ?A)
               (concatenate 'string (make-string 19 ?\x20) "A")))
(Assert (equal (format "%*c" 20 ?A)
               (concatenate 'string (make-string 19 ?\x20) "A")))
(Assert (equal (format "%020c" ?A)
               (concatenate 'string (make-string 19 ?0) "A")))
(Assert (equal (format "%0*c" 20 ?A)
               (concatenate 'string (make-string 19 ?0) "A")))
(Assert (equal (format "%-20c" ?A)
               (concatenate 'string "A" (make-string 19 ?\x20))))
(Assert (equal (format "%-*c" 20 ?A)
               (concatenate 'string "A" (make-string 19 ?\x20))))
(Assert (equal (format "%-020c" ?A)
               (concatenate 'string "A" (make-string 19 ?0))))
(Assert (equal (format "%-0*c" 20 ?A)
               (concatenate 'string "A" (make-string 19 ?0))))

(when (featurep 'mule)
  ;; Same tests, but for a very non-ASCII character.
  (Assert (equal (format "%c" (decode-char 'ucs #x0627))
                 (decode-coding-string "\xd8\xa7" 'utf-8)))
  (Assert (equal (format "%c" (char-int (decode-char 'ucs #x0627)))
                 (decode-coding-string "\xd8\xa7" 'utf-8)))
  (Assert (equal (format "%20c" (decode-char 'ucs #x0627))
                 (concatenate 'string (make-string 19 ?\x20)
                              (decode-coding-string "\xd8\xa7" 'utf-8))))
  (Assert (equal (format "%020c" (decode-char 'ucs #x0627))
                 (concatenate 'string (make-string 19 ?0)
                              (decode-coding-string "\xd8\xa7" 'utf-8))))
  (Assert (equal (format "%-20c" (decode-char 'ucs #x0627))
                 (concatenate 'string (decode-coding-string "\xd8\xa7" 'utf-8)
                              (make-string 19 ?\x20))))
  (Assert (equal (format "%-020c" (decode-char 'ucs #x0627))
                 (concatenate 'string (decode-coding-string "\xd8\xa7" 'utf-8)
                              (make-string 19 ?0))))

  ;; And the pad char.
  (macrolet
      ((checking-pad-chars (&rest expressions)
         (cons
          'progn
          (loop
              for ucs in expressions
              collect
              (let* ((character (decode-char 'ucs ucs))
                     ;; Macroexpanded, but not run, if CHARACTER is nil
                     (string (string (or character ?a)))) 
                (if character
                    `(progn
                      (Assert (equal (format ,(concat "%!" string "*d") 4 10)
                               ,(concat string string "10")))
                      (Assert (equal (format ,(concat "%!" string "*d") 4 -10)
                               ,(concat string "-10")))
                      (Assert (equal (format ,(concat "%!" string "4d") 10)
                               ,(concat string string "10")))
                      (Assert (equal (format ,(concat "%!" string "04d") 10)
                               "0010"))
                      (Assert (equal (format ,(concat "%!" string "-04d") 10)
                               ;; Zero flag ignored for trailing padding.
                               ,(concat "10" string string)))
                      (Assert (equal (format ,(concat "%!" string "*d") 22 10)
                               ,(concat (make-string 20 character)  "10")))
                      (Assert (equal (format ,(concat "%!" string "*d") 23 -10)
                               ,(concat (make-string 20 character)
                                        "-10"))))))))))
                  
    (checking-pad-chars
     #xa0 #xff #x4e00 #x2194 #x2122 #x0e9e #xd55c)))

(Check-Error 'syntax-error (format "%c" char-code-limit))
(Check-Error 'syntax-error (format "%c" 'a))
(Check-Error 'syntax-error (format "%c" pi)) ;; Newly fails
(if (featurep 'ratio) (Check-Error 'syntax-error (format "%c" (read "7/5"))))
(Check-Error 'syntax-error (format "%c" (1+ most-positive-fixnum)))
(Check-Error 'syntax-error (format "%.20c" ?a)) ;; Newly fails.
(Check-Error 'syntax-error (format "%.*c" 20 ?a)) ;; Newly fails.

(defun* slow-integer-to-string (integer &optional (radix 10))
  (check-type integer integer)
  (check-type radix (integer 2 16))
  (loop with minusp = (if (< integer 0)
			  "-" 
			;; Operate on the negative integer, to avoid the
			;; classical C most-negative-fixnum bug on
			;; non-bignum builds.
			(setq integer (- integer))
			nil)
        with result = nil
        until (eql integer 0)
        do (setf result (cons (cdr (assoc* (% integer radix)
                                           '((0 . ?0) (-1 . ?1)
                                             (-2 . ?2) (-3 . ?3)
                                             (-4 . ?4) (-5 . ?5)
                                             (-6 . ?6) (-7 . ?7)
                                             (-8 . ?8) (-9 . ?9)
                                             (-10 . ?A) (-11 . ?B) 
                                             (-12 . ?C) (-13 . ?D) 
                                             (-14 . ?E) (-14 . ?E) 
                                             (-15 . ?F) (-15 . ?F))))
                              result)
                 integer (/ integer radix))
        finally return (concatenate 'string minusp result)))

(defun* slow-ratio-to-string (ratio &optional (radix 10))
  (check-type ratio ratio)
  (concatenate 'string (slow-integer-to-string (numerator ratio) radix)
               "/" (slow-integer-to-string (denominator ratio) radix)))

(macrolet
    ((Assert-formatting-integers (guard &rest integers)
       (when (featurep guard)
         (cons 'progn
               (loop for integer in integers
                     collect (progn
                               (if (not (integerp integer))
                                   (setq integer (eval integer)))
                               `(progn
                                 (Assert (equal (format "%d" ,integer)
                                          ,(slow-integer-to-string integer)))
                                 (Assert (equal (format "%b" ,integer)
                                          ,(slow-integer-to-string integer
                                                                   2)))
                                 (Assert (equal (format "%o" ,integer)
                                          ,(slow-integer-to-string integer
                                                                   8)))
                                 (Assert (equal (format "%X" ,integer)
                                          ,(slow-integer-to-string integer
                                                                   16)))
                                 (Assert (equal (format "%x" ,integer)
                                          ,(downcase
                                            (slow-integer-to-string
                                             integer 16))))))))))
     (Assert-formatting-ratios (guard &rest ratios)
       (when (featurep guard)
         (cons 'progn
               (loop for ratio in ratios
                     collect (progn
                               (if (not (ratiop ratio))
                                   (setq ratio (eval ratio)))
                               `(progn
                                 (Assert (equal (format "%d" ,ratio)
                                          ,(slow-ratio-to-string ratio)))
                                 (Assert (equal (format "%b" ,ratio)
                                          ,(slow-ratio-to-string ratio 2)))
                                 (Assert (equal (format "%o" ,ratio)
                                          ,(slow-ratio-to-string ratio 8)))
                                 (Assert (equal (format "%X" ,ratio)
                                          ,(slow-ratio-to-string ratio 16)))
                                 (Assert (equal (format "%x" ,ratio)
                                          ,(downcase (slow-ratio-to-string
                                                      ratio 16)))))))))))
  (Assert-formatting-integers xemacs 1 -2 #xFFFF #x-FFFF most-positive-fixnum
                              most-negative-fixnum)
  (Assert-formatting-integers bignum (1+ most-positive-fixnum)
                              (1- most-positive-fixnum)
                              (lsh most-positive-fixnum 20)
                              (lsh most-negative-fixnum 20))
  (Assert-formatting-ratios ratio (div 1 5) (div #xFFFF #xFFFE)
                            (div most-positive-fixnum most-negative-fixnum)
                            (div (1+ most-positive-fixnum)
                                 (1- most-negative-fixnum))))

(Assert (equal (format "%d" (expt 2 32))
                 (if (natnump (1+ #x3fffffff)) "4294967296" "0")))

(Assert (equal (format "%d" 1) "1"))
(Assert (equal (format "%d" -1) "-1"))
(Assert (equal (format "%d" #xFFFF) "65535"))
(Assert (equal (format "%d" #x-FFFF) "-65535"))

(Assert (equal (format "%d" 97) "97"))
(Assert (equal (format "%d" (char-int ?A)) "65"))
(Assert (equal (format "% 20d" ?A)
               (concatenate 'string (make-string 18 ?\x20) "65")))
(Assert (equal (format "% 20d" 99)
               (concatenate 'string (make-string 18 ?\x20) "99")))
(Assert (equal (format "% *d" 20 ?A)
               (concatenate 'string (make-string 18 ?\x20) "65")))
(Assert (equal (format "% *d" 20 99)
               (concatenate 'string (make-string 18 ?\x20) "99")))
(Assert (equal (format "% *d" 20 ?A)
               (concatenate 'string (make-string 18 ?\x20) "65")))
(Assert (equal (format "%020d" ?A)
               (concatenate 'string (make-string 18 ?0) "65")))
(Assert (equal (format "%0*d" 20 ?A)
               (concatenate 'string (make-string 18 ?0) "65")))
(Assert (equal (format "%-20d" ?A)
               (concatenate 'string "65" (make-string 18 ?\x20))))
(Assert (equal (format "%-*d" 20 ?A)
               (concatenate 'string "65" (make-string 18 ?\x20))))
(Assert (equal (format "%-020d" 65)
               (concatenate 'string "65" (make-string 18 ?\x20))))
(Assert (equal (format "%-0*d" 20 ?A)
               (concatenate 'string "65" (make-string 18 ?\x20))))

(Check-Error invalid-function (format-into "hello" "hello"))
(Check-Error wrong-type-argument
             (format-into (make-string-output-stream)
                          (make-string-output-stream)))
(let ((stream (make-string-output-stream)))
  (Assert (eq stream (format-into stream "L'amour est un oiseau rebelle ")))
  (Assert (eq stream (format-into stream "que %#x ne peut apprivoiser" 0)))
  (Assert (equal (get-output-stream-string stream)
                 "L'amour est un oiseau rebelle que 0 ne peut apprivoiser"))
  (Assert (equal "que 0x0 ne peut apprivoiser"
                 (format-into 'string "que %#p ne peut apprivoiser" 0)))
  (Check-Error void-function
	       (format-into 'character "que %#p ne peut apprivoiser" 0)))

(macrolet
    ((Assert-with-format-extents (&rest list)
       (cons
        'progn
        (loop for (before control-string after argument length) in list
              with prefix = "prefixing stream output "
              with suffix = " suffixing stream output"
              collect
              `(let* ((format
                       ;; #### strip any extents the byte-compiler has
                       ;; uncleanly introduced.
                       (substring-no-properties (concatenate 'string ,before
                                                             ,control-string
                                                             ,after)))
                      (extent (make-extent ,(length before)
                                           ,(+ (length before)
                                               (length control-string))
                                           format))
                      (ee (make-extent ,(+ (length before)
                                           (length control-string) -1)
                                       ,(+ (length before)
                                           (length control-string))
                                       format))
                      (stream (make-string-output-stream))
                      result stream-result)
                (setf (extent-property ee 'start-open) t
                      (extent-property ee 'shorter) t
                      (extent-property extent 'longer) t
                      result (format format ,argument))
                (Assert (eql (length result)
                             ,(+ (length before) length (length after)))
			,(concatenate 'string "checking " control-string
				      " gives an appropriate-length result"))
                (Assert (eql (length (extent-list result)) 2)
                        ,(concatenate 'string "checking " control-string
                                      " produces only two extents"))
                (Assert (eql (extent-start-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'longer))
                               (extent-list result)))
                             ,(length before))
                         ,(concatenate 'string
                                       "checking extent start position fine, "
                                       control-string))
                (Assert (eql (extent-end-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'longer))
                               (extent-list result)))
                             ,(+ (length before) length))
                         ,(concatenate 'string
                                       "checking extent end position fine, "
                                       control-string))
                (Assert (eql (extent-start-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                    extent 'shorter))
                               (extent-list result)))
                             ,(+ (length before) length -1))
                        ,(concatenate 'string
                                      "checking length of non-stretching "
                                      "extent, control-string "
                                      control-string))
                (Assert (eql (extent-end-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'shorter))
                               (extent-list result)))
                             ,(+ (length before) length)))

                (write-sequence ,prefix stream)
                (format-into stream format ,argument)
                (write-sequence ,suffix stream)
                (setf stream-result (get-output-stream-string stream))

                (Assert (eql (length stream-result)
                             ,(+ (length prefix) (length before) length
                                 (length after) (length suffix))))
                (Assert (eql (length (extent-list stream-result)) 2)
                        ,(concatenate 'string "checking " control-string
                                      " produces only two extents"))
                (Assert (eql (extent-start-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'longer))
                               (extent-list stream-result)))
                             ,(+ (length prefix) (length before)))
                        ,(concatenate 'string
                                      "checking extent start position fine, "
                                      control-string))
                (Assert (eql (extent-end-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'longer))
                               (extent-list stream-result)))
                             ,(+ (length prefix) (length before) length))
                         ,(concatenate 'string
                                       "checking extent end position fine, "
                                       control-string))
                (Assert (eql (extent-start-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                    extent 'shorter))
                               (extent-list stream-result)))
                             ,(+ (length prefix) (length before) length -1))
                        ,(concatenate 'string
                                      "checking length of non-stretching "
                                      "extent, control-string "
                                      control-string))
                (Assert (eql (extent-end-position
                              (find-if
                               #'(lambda (extent) (extent-property
                                                   extent 'shorter))
                               (extent-list stream-result)))
                             ,(+ (length prefix) (length before) length))))))))
  (Assert-with-format-extents
   ("hello there " "%d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 20)
   ("hello there " "%.20d" " everyone" 1 20)
   ("hello there " "%020d" " everyone" 1 20)
   ("hello there " "%i" " everyone" 1 1)
   ("hello there " "%.20i" " everyone" 1 20)
   ("hello there " "%.20i" " everyone" 1 20)
   ("hello there " "%020i" " everyone" 1 20)
   ("hello there " "%d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 20)
   ("hello there " "% .20d" " everyone" 1 21)
   ("hello there " "%020d" " everyone" 1 20)
   ("hello there " "%s" " everyone" "a" 1)
   ("hello there " "%.20s" " everyone" "a" 1)
   ("hello there " "%.20s" " everyone" "aaaaabbbccddee" 14)
   ("hello there " "%020s" " everyone" "aaaaabbbccddee" 20)))

;; Following test cases are modified from Wine's printf test suite, which has
;; the following copyright notice:
;; 
;; Copyright 2002 Uwe Bonnes
;; Copyright 2004 Aneurin Price
;; Copyright 2005 Mike McCormack
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
;; 

(Implementation-Incomplete-Expect-Failure
 (Assert (equal (format "%+#23.15e" 789456123) "+7.894561230000000e+08"))
 (Assert (equal (format "%-#23.15e" 789456123) "7.894561230000000e+08 "))
 (Assert (equal (format "%#23.15e" 789456123) " 7.894561230000000e+08")))
(Assert (equal (format "%#1.1g" 789456123) "8.e+08") "checking #1.1g")
(Assert
 (equal
  (format "% .80d" 1)
  " 00000000000000000000000000000000000000000000000000000000000000000000000000000001"))
(Assert (equal (format "% d" 1) " 1") "checking sign place-holder")
(Assert (equal (format "%+ d" 1) "+1") "checking with sign flags")
(Assert (equal (format "%04c" ?1) "0001") "checking character zero-prefixed")
;; XEmacs change; %c is not a number specifier, allow trailing zeroes.
(Assert (equal (format "%-04c" ?1) "1000")
        "character zero-padded and/or not left-adjusted")
(Assert (equal (format "%#012x" 1) "0x0000000001")
        "checking hexadecimal zero-padded")
(Assert (equal (format "%#012x" 0) "000000000000") "hexadecimal zero-padded")
(Assert (equal (format "%#04.8x" 1) "0x00000001")
        "hexadecimal zero-padded precision")
(Assert (equal (format "%#04.8x" 0) "00000000")
        "hexadecimal zero-padded precision")
(Assert (equal (format "%#-08.2x" 1) "0x01    ")
        "hexadecimal zero-padded left-adjusted")
(Assert (equal (format "%#-08.2x" 0) "00      ")
        "hexadecimal zero-padded not left-adjusted")
(Assert (equal (format "%#.0x" 1) "0x1")
        "hexadecimal zero-padded zero-precision")
;; This is an intentional difference from the C standard and from what GNU
;; Emacs and Wine do. Our thinking is that we should never do a value-altering
;; truncation of a numeric value.
(Assert (equal (format "%#.0x" 0) "0")
        "hexadecimal zero-padded zero-precision")

(Assert (equal (format "%#08o" 1) "00000001") "octal zero-padded")
(Assert (equal (format "%#o" 1) "01") "octal zero-padded")
(Assert (equal (format "%#o" 0) "0") "octal zero-padded")
(Assert (equal (format "%04s" "foo") "0foo") "checking string zero-prefixed")
(Assert (equal (format "%.1s" "foo") "f") "checking precision")
(Assert (equal (format "%.*s" 1 "foo") "f") "checking precision")
(Assert (equal (format "%*s" -5 "foo") "foo  ") "checking field width < 0")
(Assert (equal (format "hello") "hello") "checking simple string")
(Assert (equal (format "%3c" ?a) "  a") "checking single char with padding")
(Assert (equal (format "%3d" 1234) "1234"))
(Assert (equal (format "%-1d" 2) "2"))
(Assert (equal (format "%2.4f" 8.6) "8.6000"))
(Assert (equal (format "%0f" 0.6) "0.600000"))
(Assert (equal (format "%.0f" 0.6) "1"))
(Assert (equal (format "%2.4e" 8.6) "8.6000e+00"))
(Assert (equal (format "% 2.4e" 8.6) " 8.6000e+00"))
(Implementation-Incomplete-Expect-Failure
 (Assert (equal (format "% 014.4e" 8.6) " 008.6000e+000")))
(Assert (equal (format "% 2.4e" -8.6) "-8.6000e+00"))
(Assert (equal (format "%+2.4e" 8.6) "+8.6000e+00"))
(Assert (equal (format "%2.4g" 8.6) "8.6"))
(Assert (equal (format "%-i" -1) "-1"))
(Assert (equal (format "%-i" 1) "1"))
(Assert (equal (format "%+i" 1) "+1"))
(Assert (equal (format "%o" 10) "12"))
(Assert (equal (format "%s" 0) "0"))
(Assert (equal (format "%s" "%%%%") "%%%%"))
(Assert (equal (format "%%0") "%0"))
(Assert (equal (format "%hx" #x12345) "2345")) ;; field size no longer ignored
(Assert (equal (format "%hhx" #x123) "23")) ;; Truncate to 8 bits
(Assert (equal (format "%lx" #x123) "123")) ;; To 32
(Assert (equal (format "%lx" #x123456789a) "3456789a")) ;; Really, to 32
(when (featurep 'bignum)
  (macrolet
      ((very-positive-bignum () (1- (expt 2 96)))
       (very-negative-bignum () (- (1- (expt 2 96)))))
    (Assert (equal (format "%hhux" (very-positive-bignum)) "ff"))
    (Assert (equal (format "%hux" (very-positive-bignum))  "ffff"))
    (Assert (equal (format "%lux" (very-positive-bignum))  "ffffffff"))
    (Assert (equal (format "%llux" (very-positive-bignum)) "ffffffffffffffff"))
    (Assert (equal (format "%hhx" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%hx" (very-positive-bignum))  "-1"))
    (Assert (equal (format "%lx" (very-positive-bignum))  "-1"))
    (Assert (equal (format "%llx" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%x" (very-positive-bignum))
                   "ffffffffffffffffffffffff"))
    (Assert (equal (format "%hhux" (very-negative-bignum)) "ff"))
    (Assert (equal (format "%hux" (very-negative-bignum))  "ffff"))
    (Assert (equal (format "%lux" (very-negative-bignum))  "ffffffff"))
    (Assert (equal (format "%llux" (very-negative-bignum)) "ffffffffffffffff"))
    (Assert (equal (format "%hhx" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%hx" (very-negative-bignum))  "-1"))
    (Assert (equal (format "%lx" (very-negative-bignum))  "-1"))
    (Assert (equal (format "%llx" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%x" (very-negative-bignum))
                   "-ffffffffffffffffffffffff"))
    (Assert (equal (format "%hhud" (very-positive-bignum)) "255"))
    (Assert (equal (format "%hud" (very-positive-bignum)) "65535"))
    (Assert (equal (format "%lud" (very-positive-bignum)) "4294967295"))
    (Assert (equal (format "%llud" (very-positive-bignum))
                   "18446744073709551615"))
    (Assert (equal (format "%hhd" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%hd" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%ld" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%lld" (very-positive-bignum)) "-1"))
    (Assert (equal (format "%d" (very-positive-bignum))
                   "79228162514264337593543950335"))
    (Assert (equal (format "%hhud" (very-negative-bignum)) "255"))
    (Assert (equal (format "%hud" (very-negative-bignum)) "65535"))
    (Assert (equal (format "%lud" (very-negative-bignum)) "4294967295"))
    (Assert (equal (format "%llud" (very-negative-bignum))
                   "18446744073709551615"))
    (Assert (equal (format "%hhd" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%hd" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%ld" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%lld" (very-negative-bignum)) "-1"))
    (Assert (equal (format "%d" (very-negative-bignum))
                   "-79228162514264337593543950335"))
    (Assert (equal (format "%hhuo" (very-positive-bignum)) "377"))
    (Assert (equal (format "%huo" (very-positive-bignum))  "177777"))
    (Assert (equal (format "%luo" (very-positive-bignum))  "37777777777"))
    (Assert (equal (format "%lluo" (very-positive-bignum))
		   "1777777777777777777777"))
    (Assert (equal (format "%hho" (very-positive-bignum))  "-1"))
    (Assert (equal (format "%ho" (very-positive-bignum))   "-1"))
    (Assert (equal (format "%lo" (very-positive-bignum))   "-1"))
    (Assert (equal (format "%llo" (very-positive-bignum))  "-1"))
    (Assert (equal (format "%o" (very-positive-bignum))
		   "77777777777777777777777777777777"))
    (Assert (equal (format "%hhuo" (very-negative-bignum)) "377"))
    (Assert (equal (format "%huo" (very-negative-bignum))  "177777"))
    (Assert (equal (format "%luo" (very-negative-bignum))  "37777777777"))
    (Assert (equal (format "%lluo" (very-negative-bignum))
		   "1777777777777777777777"))
    (Assert (equal (format "%hho" (very-negative-bignum))  "-1"))
    (Assert (equal (format "%ho" (very-negative-bignum))   "-1"))
    (Assert (equal (format "%lo" (very-negative-bignum))   "-1"))
    (Assert (equal (format "%llo" (very-negative-bignum))  "-1"))
    (Assert (equal (format "%o" (very-negative-bignum))
		   "-77777777777777777777777777777777"))
    (Assert (equal (format "%hhub" (very-positive-bignum)) "11111111"))
    (Assert (equal (format "%hub" (very-positive-bignum)) 
                   "1111111111111111"))
    (Assert (equal (format "%lub" (very-positive-bignum))
                   "11111111111111111111111111111111"))
    (Assert
     (equal
      (format "%llub" (very-positive-bignum))
      "1111111111111111111111111111111111111111111111111111111111111111"))
    (Assert (equal (format "%hhb" (very-positive-bignum))  "-1"))
    (Assert (equal (format "%hb" (very-positive-bignum))   "-1"))
    (Assert (equal (format "%lb" (very-positive-bignum))   "-1"))
    (Assert (equal (format "%llb" (very-positive-bignum))  "-1"))
    (Assert
     (equal
      (format "%b" (very-positive-bignum))
      "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"))
    (Assert (equal (format "%hhub" (very-negative-bignum)) "11111111"))
    (Assert (equal (format "%hub" (very-negative-bignum))
                   "1111111111111111"))
    (Assert (equal (format "%lub" (very-negative-bignum))
                   "11111111111111111111111111111111"))
    (Assert
     (equal (format "%llub" (very-negative-bignum))
            "1111111111111111111111111111111111111111111111111111111111111111"))
    (Assert (equal (format "%hhb" (very-negative-bignum))  "-1"))
    (Assert (equal (format "%hb" (very-negative-bignum))   "-1"))
    (Assert (equal (format "%lb" (very-negative-bignum))   "-1"))
    (Assert (equal (format "%llb" (very-negative-bignum))  "-1"))
    (Assert
     (equal
      (format "%b" (very-negative-bignum))
      "-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"))))

(macrolet
    ((check-%S (&rest list)
       (cons 'progn
             (mapcan (function*
                      (lambda ((object . representation))
                       `((Assert (equal (prin1-to-string ,(quote-maybe object))
                                        (format "%S" ,(quote-maybe object))))
                         (Assert (equal (format "%S" ,(quote-maybe object))
                                        ,representation))))) list))))
  (check-%S (1e+ . "1e+") (20000e- . "20000e-") ("hello" . "\"hello\"")
            ("hello\"everyone" . "\"hello\\\"everyone\"")
            (?\n . "?\\n") (?a . "?a") ([hello there] . "[hello there]")))

(Check-Error syntax-error (format "%I64d" 1))
(Check-Error syntax-error (format "%I32d" 1))

;; This used to crash with bignum builds.
(Check-Error (wrong-type-argument syntax-error) (format "%n" pi))

(unless (featurep 'mule)
  ;; This might work (not error) on a non-mule build. On my 11 year old 32
  ;; bit machine, I don't have enough RAM for it to succeed, whence the
  ;; conditional. It's unlikely to work on a 64 bit build.
  (Check-Error args-out-of-range (format (concat "%" (number-to-string
                                                      most-positive-fixnum)
                                                 "d") 1)))

;; end of format-tests.el
