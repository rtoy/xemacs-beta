;;; cyrillic-hooks.el --- pre-loaded support for Cyrillic.

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

;; For syntax of Cyrillic
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'iso-8859-5 'iso2022
 "MIME ISO-8859-5"
 '(charset-g0 ascii
   charset-g1 cyrillic-iso8859-5
   charset-g2 t
   charset-g3 t
   mnemonic "ISO8/Cyr"
   ))

;;(add-hook 'quail-package-alist '("jcuken" "quail/cyrillic"))
;;(add-hook 'quail-package-alist '("macedonian" "quail/cyrillic"))
;;(add-hook 'quail-package-alist '("serbian" "quail/cyrillic"))
;;(add-hook 'quail-package-alist '("beylorussian" "quail/cyrillic"))
;;(add-hook 'quail-package-alist '("ukrainian" "quail/cyrillic"))
;;(add-hook 'quail-package-alist '("yawerty" "quail/cyrillic"))

(define-language-environment 'cyrillic
  "Cyrillic"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'iso-8859-5)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-buffer-file-coding-system 'iso-8859-5)
    (setq terminal-coding-system 'iso-8859-5)
    (setq keyboard-coding-system 'iso-8859-5)
;;    (setq-default quail-current-package
;;		  (assoc "yawerty" quail-package-alist))))
    ))
