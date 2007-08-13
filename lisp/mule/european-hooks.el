;;; european-hooks.el --- pre-loaded support for European languages.

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

;; For syntax of Latin-1 characters.
(loop for c from 64 to 127		; from ',A@(B' to ',A(B'
      do (modify-syntax-entry (make-char 'latin-1 c) "w"))

(modify-syntax-entry (make-char 'latin-1 32) "w") ; no-break space
(modify-syntax-entry ?,AW(B "_")
(modify-syntax-entry ?,Aw(B "_")

;; For syntax of Latin-2
(loop for c in '(?,B!(B ?,B#(B ?,B%(B ?,B&(B ?,B)(B ?,B*(B ?,B+(B ?,B,(B ?,B.(B ?,B/(B ?,B1(B ?,B3(B ?,B5(B ?,B6(B ?,B9(B ?,B:(B ?,B;(B ?,B<(B)
      do (modify-syntax-entry c "w"))

(loop for c from 62 to 126
      do (modify-syntax-entry (make-char 'latin-2 c) "w"))

(modify-syntax-entry (make-char 'latin-2 32) "w") ; no-break space
(modify-syntax-entry ?,BW(B ".")
(modify-syntax-entry ?,Bw(B ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EUROPEANS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'iso-8859-2 'iso2022 "MIME ISO-8859-2"
 '(charset-g0 ascii
   charset-g1 latin-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"
   ))

(make-coding-system
 'iso-8859-3 'iso2022 "MIME ISO-8859-3"
 '(charset-g0 ascii
   charset-g1 latin-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"
   ))

(make-coding-system
 'iso-8859-4 'iso2022 "MIME ISO-8859-4"
 '(charset-g0 ascii
   charset-g1 latin-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"
   ))

(make-coding-system
 'iso-8859-9 'iso2022 "MIME ISO-8859-9"
 '(charset-g0 ascii
   charset-g1 latin-5
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-5"
   ))

(add-hook 'quail-package-alist '("latin-1" "quail/latin"))
(add-hook 'quail-package-alist '("latin-2" "quail/latin"))
(add-hook 'quail-package-alist '("latin-3" "quail/latin"))
(add-hook 'quail-package-alist '("latin-4" "quail/latin"))
(add-hook 'quail-package-alist '("latin-5" "quail/latin"))

(define-language-environment 'european
  "European (for Latin-1 through Latin-5)"
  (lambda ()
    ;(set-coding-category-system 'iso-8-designate 'iso-8859-1)
    ;(set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-file-coding-system    'binary) ; iso-8859-1
    (setq locale-coding-system         'binary) ; iso-8859-1
    (setq process-input-coding-system  'binary) ; iso-8859-1
    (setq process-output-coding-system 'binary) ; iso-8859-1
    (set-default-file-coding-system    'binary) ; iso-8859-1
    (set-file-coding-system-for-read   'binary) ; iso-8859-1
    ;(setq display-coding-system 'iso-8859-1)
    ;(setq keyboard-coding-system 'iso-8859-1)
    (setq-default quail-current-package
                  (assoc "latin-1" quail-package-alist))))
