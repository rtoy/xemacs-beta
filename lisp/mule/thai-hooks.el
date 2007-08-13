;;; thai-hooks.el --- pre-loaded support for Thai.

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

;; Thai TSCII
;; We are now supporting TIS620 (Thai) as an official character set.
;(make-charset 'thai "Thai TSCII"
;	      '(registry "TIS620"
;		dimension 1
;		chars 94
;		final ?1
;		graphic 0
;		))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THAI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'tis620 'iso2022
 "Coding-system used for ASCII(MSB=0) & TIS620(MSB=1)."
 '(charset-g0 ascii
   charset-g1 thai-tis620
   no-ascii-cntl t
   mnemonic "TIS620"
   post-read-conversion thai-compose-region
   pre-write-conversion decompose-region
   ))

(add-hook 'quail-package-alist '("thai" "quail/thai"))

(define-language-environment 'thai
  "Thai"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'tis620)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-buffer-file-coding-system 'tis620)))
    ;;(setq-default quail-current-package (assoc "thai" quail-package-alist))))
