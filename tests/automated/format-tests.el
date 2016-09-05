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
;; those changes to it made in August of 2016.

(Assert (equal (format "%c" ?a) "a"))
(Assert (equal (format "%c" (char-int ?A)) "A"))
(Assert (equal (format "% 20c" ?A)
               (concatenate 'string (make-string 19 ?\x20) "A")))
(Assert (equal (format "% *c" 20 ?A)
               (concatenate 'string (make-string 19 ?\x20) "A")))
(Assert (equal (format "%020c" ?A)
               (concatenate 'string (make-string 19 ?0) "A")))
(Assert (equal (format "%0*c" 20 ?A)
               (concatenate 'string (make-string 19 ?0) "A")))
(Assert (equal (format "% -20c" ?A)
               (concatenate 'string "A" (make-string 19 ?\x20))))
(Assert (equal (format "% -*c" 20 ?A)
               (concatenate 'string "A" (make-string 19 ?\x20))))
(Assert (equal (format "%-020c" ?A)
               (concatenate 'string "A" (make-string 19 ?0))))
(Assert (equal (format "%-0*c" 20 ?A)
               (concatenate 'string "A" (make-string 19 ?0))))

(when (featurep 'mule)
  ;; Same tests, but for a very non-ASCII character.
  (Assert (equal (format "%c" (aref "\u0627" 0)) "\u0627"))
  (Assert (equal (format "%c" (char-int (aref "\u0627" 0))) "\u0627"))
  (Assert (equal (format "% 20c" (aref "\u0627" 0))
                 (concatenate 'string (make-string 19 ?\x20) "\u0627")))
  (Assert (equal (format "%020c" (aref "\u0627" 0))
                 (concatenate 'string (make-string 19 ?0) "\u0627")))
  (Assert (equal (format "% -20c" (aref "\u0627" 0))
                 (concatenate 'string "\u0627" (make-string 19 ?\x20))))
  (Assert (equal (format "%-020c" (aref "\u0627" 0))
                 (concatenate 'string "\u0627" (make-string 19 ?0)))))

(Check-Error 'syntax-error (format "%c" char-code-limit))
(Check-Error 'syntax-error (format "%c" 'a))
(Check-Error 'syntax-error (format "%c" pi)) ;; Newly fails
(Check-Error 'syntax-error (format "%c" '7/5))
(Check-Error 'syntax-error (format "%c" (1+ most-positive-fixnum)))
(Check-Error 'syntax-error (format "%.20c" ?a)) ;; Newly fails.
(Check-Error 'syntax-error (format "%.*c" 20 ?a)) ;; Newly fails.

(defun slow-integer-to-string (integer)
  (check-type integer integer)
  (loop with minusp = (if (< integer 0)
                          (prog1 t (setq integer (- integer))))
        with result = nil
        until (eql integer 0)
        do (setf result (cons (cdr (assoc* (mod integer 10)
                                           '((0 . ?0) (1 . ?1)
                                             (2 . ?2) (3 . ?3)
                                             (4 . ?4) (5 . ?5)
                                             (6 . ?6) (7 . ?7)
                                             (8 . ?8) (9 . ?9))))
                              result)
                 integer (/ integer 10))
        finally return (concatenate 'string (if minusp "-")
                                    result)))
(macrolet
    ((Assert-formatting-integers (&rest integers)
       (cons 'progn
             (loop for integer in integers
                   collect (progn
                             (if (not (integerp integer))
                                 (setq integer (eval integer)))
                             `(Assert (equal (format "%d" ,integer)
                                       ,(slow-integer-to-string
                                         integer))))))))
  (Assert-formatting-integers 1 -2 #xFFFF #x-FFFF most-positive-fixnum
                              most-negative-fixnum))

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
               (concatenate 'string "65" (make-string 18 ?0))))
(Assert (equal (format "%-0*d" 20 ?A)
               (concatenate 'string "65" (make-string 18 ?0))))

(macrolet
    ((Assert-with-format-extents (&rest list)
       (cons
        'progn
        (loop for (before control-string after argument length) in list
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
                      result)
                (setf (extent-property ee 'start-open) t
                      (extent-property ee 'shorter) t
                      (extent-property extent 'longer) t
                      result (format format ,argument))
                (Assert (eql (length result)
                             ,(+ (length before) length (length after))))
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
                             ,(+ (length before) length))))))))
  (Assert-with-format-extents
   ("hello there " "%d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 1)
   ("hello there " "%020d" " everyone" 1 20)
   ("hello there " "%i" " everyone" 1 1)
   ("hello there " "%.20i" " everyone" 1 1)
   ("hello there " "%.20i" " everyone" 1 1)
   ("hello there " "%020i" " everyone" 1 20)
   ("hello there " "%d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 1)
   ("hello there " "%.20d" " everyone" 1 1)
   ("hello there " "%020d" " everyone" 1 20)
   ("hello there " "%s" " everyone" "a" 1)
   ("hello there " "%.20s" " everyone" "a" 1)
   ("hello there " "%.20s" " everyone" "aaaaabbbccddee" 14)
   ("hello there " "%020s" " everyone" "aaaaabbbccddee" 20)))

;; end of format-tests.el
