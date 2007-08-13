;;; greek-hooks.el --- pre-loaded support for Greek.

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

;; For syntax of Greek
(loop for c from 54 to 126
      do (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w"))
(modify-syntax-entry (make-char 'greek-iso8859-7 32) "w") ; no-break space
(modify-syntax-entry ?,F7(B ".")
(modify-syntax-entry ?,F;(B ".")
(modify-syntax-entry ?,F=(B ".")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GREEK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'iso-8859-7 'iso2022 "MIME ISO-8859-7"
 '(charset-g0 ascii
   charset-g1 greek-iso8859-7
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Grk"
   ))

;;(add-hook 'quail-package-alist '("greek" "quail/greek"))

(define-language-environment 'greek
  "Greek"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'iso-8859-7)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-buffer-file-coding-system 'iso-8859-7)
    (setq terminal-coding-system 'iso-8859-7)
    (setq keyboard-coding-system 'iso-8859-7)
;;  (setq-default quail-current-package
;;		  (assoc "greek" quail-package-alist))))
    ))
