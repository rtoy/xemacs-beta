;;; hebrew-hooks.el --- pre-loaded support for Hebrew.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Mule 2.3.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HEBREW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax of Hebrew characters
(loop for c from 96 to 122
      do (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w"))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 32) "w") ; no-break space

(make-coding-system
 'iso-8859-8 'iso2022
 "MIME ISO-8859-8"
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   no-iso6429 t
   mnemonic "MIME/Hbrw"
))

(make-coding-system
 'ctext-hebrew 'iso2022
 "Coding-system of Hebrew."
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   mnemonic "CText/Hbrw"
))

;;(add-hook 'quail-package-alist '("hebrew" "quail/hebrew"))

(define-language-environment 'hebrew
  "Hebrew"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'iso-8859-8)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-file-coding-system 'iso-8859-8)
    (setq terminal-coding-system    'iso-8859-8)
    (setq keyboard-coding-system    'iso-8859-8)
;;    (setq-default quail-current-package
;;                  (assoc "hebrew" quail-package-alist))))
    ))
