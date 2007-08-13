;;; korean-hooks.el --- pre-loaded support for Korean.

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
;;; KOREAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax of Korean characters.
(loop for row from 33 to  34  do (modify-syntax-entry `[korean-ksc5601 ,row] "."))
(loop for row from 35 to  37  do (modify-syntax-entry `[korean-ksc5601 ,row] "w"))
(loop for row from 38 to  41  do (modify-syntax-entry `[korean-ksc5601 ,row] "."))
(loop for row from 42 to 126  do (modify-syntax-entry `[korean-ksc5601 ,row] "w"))

;; EGG specific setup
;(if (featurep 'egg)
;    (progn
;      (load "its-hangul")
;      (setq its:*standard-modes*
;	    (cons (its:get-mode-map "hangul") its:*standard-modes*))
;      (setq-default its:*current-map* (its:get-mode-map "hangul"))))

(add-hook 'quail-package-alist '("hangul"    "quail/hangul"))
(add-hook 'quail-package-alist '("hangul3"   "quail/hangul3"))
(add-hook 'quail-package-alist '("hanja-jis" "quail/hanja-jis"))
(add-hook 'quail-package-alist '("hanja-ksc" "quail/hanja-ksc"))

(make-coding-system
 'euc-korea 'iso2022
 "Coding-system of Korean EUC (Extended Unix Code)."
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   mnemonic "EUC/Kor"
   eol-type lf
   ))

(copy-coding-system 'euc-korea 'euc-kr)

(make-coding-system
 'iso-2022-kr 'iso2022
 "Coding-System used for communication with mail in Korea."
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   force-g1-on-output t
   seven t
   lock-shift t
   mnemonic "Mail/Kor"
   eol-type lf
   ))

(copy-coding-system 'iso-2022-kr 'korean-mail)

(make-coding-system
 'iso-2022-int-1 'iso2022
"ISO-2022-INT-1"
 '(charset-g0 ascii
   charset-g1 korean-ksc5601
   short t
   seven t
   lock-shift t
   mnemonic "ISO7/Kor"
   ))

(define-language-environment 'korean
  "Korean"
  (lambda ()
    (set-coding-category-system 'iso-8-2 'euc-korea)
    (set-coding-priority-list '(iso-8-2 iso-8-designate))
    (set-default-file-coding-system 'iso-2022-kr)
    (setq-default quail-current-package
                  (assoc "hangul" quail-package-alist))))
