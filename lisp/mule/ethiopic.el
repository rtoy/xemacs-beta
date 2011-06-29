;;; ethiopic.el --- Support for Ethiopic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Ethiopic

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

;; Author: TAKAHASHI Naoto <ntakahas@etl.go.jp>
;;         modified by MORIOKA Tomohiko <morioka@jaist.ac.jp> for XEmacs.

;;; Code:

;; Ethiopic characters (Amahric and Tigrigna).
(make-charset 'ethiopic "Ethiopic characters"
	      '(dimension
		2
		registries ["Ethiopic-Unicode"]
		chars 94
		columns 2
		direction l2r
		final ?3
		graphic 0
		short-name "Ethiopic"
		long-name "Ethiopic characters"
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETHIOPIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-category ?E "Ethiopic (Ge'ez) character.")
(modify-category-entry 'ethiopic ?E)

(define-ccl-program ccl-encode-ethio-font
  '(0
    ;; In:  R0:ethiopic (not checked)
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    ((r1 -= 33)
     (r2 -= 33)
     (r1 *= 94)
     (r2 += r1)
     (if (r2 < 256)
	 (r1 = #x12)
       (if (r2 < 448)
	   ((r1 = #x13) (r2 -= 256))
	 ((r1 = #xfd) (r2 -= 208))
	 ))))
  "CCL program to encode an Ethiopic code to code point of Ethiopic font.")

(set-charset-ccl-program 'ethiopic 'ccl-encode-ethio-font)

(set-language-info-alist
 "Ethiopic" '((setup-function . setup-ethiopic-environment-internal)
	      (exit-function . exit-ethiopic-environment)
	      (charset ethiopic)
	      (coding-system iso-2022-7bit)
	      (coding-priority iso-2022-7bit)
	      ;; (input-method . "ethiopic")
	      (features ethio-util)
	      (sample-text . "$(3$Q#U!.(B")
	      (documentation . t)))

;; In a more ideal world, we could set the default face fallback from here
;; to use one of the misc-fixed sizes that handles Ethiopic.

;;; ethiopic.el ends here
