;;; ethiopic-hooks.el --- pre-loaded support for Ethiopic.

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

;; Ethiopic
(make-charset 'ethiopic "Ethiopic"
	      '(registry "Ethio"
		dimension 2
		chars 94
		final ?2
		graphic 0
		))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETHIOPIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-category ?E "Ethiopic (Ge'ez) character.")
(modify-category-entry 'ethiopic ?E)

(define-ccl-program ccl-ethiopic
  '(((r0 -= #x21)
     (r1 -= #x21)
     (r0 *= 94)
     (r1 += r0)
     (if (r1 < 256) (r0 = 0) ((r1 -= 256) (r0 = 1))))))

(set-charset-ccl-program 'ethiopic ccl-ethiopic)

(add-hook 'quail-package-alist '("ethio" "quail-ethio"))

(define-language-environment 'ethiopic
  "Ethiopic"
  #'(lambda ()
      (setq-default quail-current-package
		    (assoc "ethio" quail-package-alist))))
