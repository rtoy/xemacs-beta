;;; european-hooks.el --- pre-loaded support for European languages.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1997 MORIOKA Tomohiko

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

;;; Commentary:

;; Synched up with: Mule 2.3.

;; Modification to sync with Emacs 20.1 is not finished yet.

;;; Code:

;; For syntax of Latin-1 characters.
(loop for c from 64 to 127		; from 'À' to 'ÿ'
      do (modify-syntax-entry (make-char 'latin-iso8859-1 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-1 32) "w") ; no-break space
(modify-syntax-entry ?× "_")
(modify-syntax-entry ?÷ "_")

;; For syntax of Latin-2
(loop for c in '(?-B¡-A ?-B£-A ?-B¥-A ?-B¦-A ?-B©-A ?-Bª-A ?-B«-A ?-B¬-A ?-B®-A ?-B¯-A ?-B±-A ?-B³-A ?-Bµ-A ?-B¶-A ?-B¹-A ?-Bº-A ?-B»-A ?-B¼)-A
      do (modify-syntax-entry c "w"))

(loop for c from 62 to 126
      do (modify-syntax-entry (make-char 'latin-iso8859-2 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-2 32) "w") ; no-break space
(modify-syntax-entry ?-B×-A ".")
(modify-syntax-entry ?-B÷-A ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EUROPEANS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-coding-system
 'iso-8859-2 'iso2022 "MIME ISO-8859-2"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"
   ))

(make-coding-system
 'iso-8859-3 'iso2022 "MIME ISO-8859-3"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"
   ))

(make-coding-system
 'iso-8859-4 'iso2022 "MIME ISO-8859-4"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"
   ))

(make-coding-system
 'iso-8859-9 'iso2022 "MIME ISO-8859-9"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-9
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-5"
   ))

(register-input-method "European"
		       '("quail-latin-1" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-2" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-3" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-4" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-5" quail-use-package "quail/latin"))

(define-language-environment 'european
  "European (for Latin-1 through Latin-5)"
  (lambda ()
    ;;(set-coding-category-system 'iso-8-designate 'iso-8859-1)
    ;;(set-coding-priority-list '(iso-8-designate iso-8-1))
    ;;(setq locale-coding-system            'no-conversion) ; iso-8859-1
    (set-default-buffer-file-coding-system  'no-conversion) ; iso-8859-1
    (set-buffer-file-coding-system-for-read 'no-conversion) ; iso-8859-1
    ;;(setq display-coding-system 'iso-8859-1)
    ;;(setq keyboard-coding-system 'iso-8859-1)
    (setq-default quail-current-package
                  (assoc "latin-1" quail-package-alist))
    ))

(set-language-info "English" 'tutorial "TUTORIAL")

(register-input-method "French"
		       '("quail-latin-1" quail-use-package "quail/latin"))
(register-input-method "French"
		       '("quail-latin-1" quail-use-package "quail/latin"))

;;; european-hooks.el ends here
