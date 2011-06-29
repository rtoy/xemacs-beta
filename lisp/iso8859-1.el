;;; iso8859-1.el --- Set case table for Latin 1

;; Copyright (C) 1992, 1997, 2006 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Created: 19-aug-92
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with:  Not in FSF. 

;;; Commentary:

;; Sets the case table for the ISO-8859/1 character set.
;; Provides ascii-case-table, for use in environments where multilingual
;; case-insensitive processing is inappropriate.

;;; Code:

(defvar ascii-case-table
  (loop
    for lower from (char-int ?a) to (char-int ?z)
    and upper from (char-int ?A) to (char-int ?Z)
    with table = (make-case-table)
    do (put-case-table-pair (coerce upper 'character)
                            (coerce lower 'character)
                            table)
    finally return table)
  "Case table for the ASCII character set.")

(loop
  for (upper lower)
  in '((?\xC0 ?\xE0) ;; A WITH GRAVE
       (?\xC1 ?\xE1) ;; A WITH ACUTE
       (?\xC2 ?\xE2) ;; A WITH CIRCUMFLEX
       (?\xC3 ?\xE3) ;; A WITH TILDE
       (?\xC4 ?\xE4) ;; A WITH DIAERESIS
       (?\xC5 ?\xE5) ;; A WITH RING ABOVE
       (?\xC6 ?\xE6) ;; AE
       (?\xC7 ?\xE7) ;; C WITH CEDILLA
       (?\xC8 ?\xE8) ;; E WITH GRAVE
       (?\xC9 ?\xE9) ;; E WITH ACUTE
       (?\xCA ?\xEA) ;; E WITH CIRCUMFLEX
       (?\xCB ?\xEB) ;; E WITH DIAERESIS
       (?\xCC ?\xEC) ;; I WITH GRAVE
       (?\xCD ?\xED) ;; I WITH ACUTE
       (?\xCE ?\xEE) ;; I WITH CIRCUMFLEX
       (?\xCF ?\xEF) ;; I WITH DIAERESIS
       (?\xD0 ?\xF0) ;; ETH
       (?\xD1 ?\xF1) ;; N WITH TILDE
       (?\xD2 ?\xF2) ;; O WITH GRAVE
       (?\xD3 ?\xF3) ;; O WITH ACUTE
       (?\xD4 ?\xF4) ;; O WITH CIRCUMFLEX
       (?\xD5 ?\xF5) ;; O WITH TILDE
       (?\xD6 ?\xF6) ;; O WITH DIAERESIS
       (?\xD8 ?\xF8) ;; O WITH STROKE
       (?\xD9 ?\xF9) ;; U WITH GRAVE
       (?\xDA ?\xFA) ;; U WITH ACUTE
       (?\xDB ?\xFB) ;; U WITH CIRCUMFLEX
       (?\xDC ?\xFC) ;; U WITH DIAERESIS
       (?\xDD ?\xFD) ;; Y WITH ACUTE
       (?\xDE ?\xFE)) ;; THORN
  with case-table = (standard-case-table)
  do (put-case-table-pair upper lower case-table))

;; Everything Latin-1 and above should be displayed as its character value
;; by default.
(setq-default ctl-arrow #xA0)

(when (and (compiled-function-p (symbol-function 'char-width))
	   (not (featurep 'mule)))
  (defalias 'char-width
    (let ((constantly (constantly 1)))
     (make-byte-code (compiled-function-arglist constantly)
		     (compiled-function-instructions constantly)
		     (compiled-function-constants constantly)
		     (compiled-function-stack-depth constantly)
		     (compiled-function-doc-string
		      (symbol-function 'char-width))))))

;; Shouldn't be necessary, but one file in the packages uses it:
(provide 'iso8859-1) 

;;; iso8859-1.el ends here
