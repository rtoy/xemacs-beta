;;; iso8859-1.el --- Set case table for Latin 1

;; Copyright (C) 1992, 1997, 2006 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Created: 19-aug-92
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
    do (put-case-table-pair (coerce lower 'character)
                            (coerce upper 'character)
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

;; Shouldn't be necessary, but one file in the packages uses it:
(provide 'iso8859-1) 

;;; iso8859-1.el ends here
