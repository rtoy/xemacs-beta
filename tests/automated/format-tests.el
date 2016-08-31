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

;; end of format-tests.el
