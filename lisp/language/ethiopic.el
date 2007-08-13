;;; ethiopic.el --- Support for Ethiopic

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Ethiopic

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

;; Author: TAKAHASHI Naoto <ntakahas@etl.go.jp>
;;         modified by MORIOKA Tomohiko <morioka@jaist.ac.jp> for XEmacs.

;;; Code:

;; Ethiopic
(make-charset 'ethiopic "Ethiopic"
	      '(registry "Ethio"
		dimension 2
		chars 94
		final ?3
		graphic 0
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETHIOPIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-category ?E "Ethiopic (Ge'ez) character.")
(modify-category-entry 'ethiopic ?E)

;; (define-ccl-program ccl-encode-ethio-font
;;   '(0
;;     ;; In:  R0:ethiopic (not checked)
;;     ;;      R1:position code 1
;;     ;;      R2:position code 2
;;     ;; Out: R1:font code point 1
;;     ;;      R2:font code point 2
;;     ((r1 -= 33)
;;      (r2 -= 33)
;;      (r1 *= 94)
;;      (r2 += r1)
;;      (if (r2 < 256)
;;          (r1 = ?\x12)
;;        (if (r2 < 448)
;;            ((r1 = ?\x13) (r2 -= 256))
;;          ((r1 = ?\xfd) (r2 -= 208))
;;          ))))
;;   "CCL program to encode an Ehitopic code to code point of Ehitopic font.")
(define-ccl-program ccl-encode-ethio-font
  '(((r0 -= #x21)
     (r1 -= #x21)
     (r0 *= 94)
     (r1 += r0)
     (if (r1 < 256) (r0 = 0) ((r1 -= 256) (r0 = 1))))))

;; (setq font-ccl-encoder-alist
;;       (cons (cons "ethiopic" ccl-encode-ethio-font) font-ccl-encoder-alist))
(set-charset-ccl-program 'ethiopic ccl-encode-ethio-font)

(register-input-method
 "Ethiopic" '("quail-ethio" quail-use-package "quail/ethiopic"))

(defun setup-ethiopic-environment ()
  "Setup multilingual environment for Ethiopic."
  (interactive)
  (setq primary-language "Ethiopic")

  (setq default-input-method '("Ethiopic" . "quail-ethio"))

  ;;
  ;;  key bindings
  ;;
  (define-key global-map [f4] 'sera-to-fidel-buffer)
  (define-key global-map [S-f4] 'sera-to-fidel-region)
  (define-key global-map [C-f4] 'sera-to-fidel-marker)
  (define-key global-map [f5] 'fidel-to-sera-buffer)
  (define-key global-map [S-f5] 'fidel-to-sera-region)
  (define-key global-map [C-f5] 'fidel-to-sera-marker)
  (define-key global-map [f6] 'ethio-modify-vowel)
  (define-key global-map [f7] 'ethio-replace-space)
  (define-key global-map [f8] 'ethio-input-special-character)
  (define-key global-map [S-f2] 'ethio-replace-space) ; as requested

  (add-hook
   'rmail-mode-hook
   '(lambda ()
      (define-key rmail-mode-map [C-f4] 'sera-to-fidel-mail)
      (define-key rmail-mode-map [C-f5] 'fidel-to-sera-mail)))

  (add-hook
   'mail-mode-hook
   '(lambda ()
      (define-key mail-mode-map [C-f4] 'sera-to-fidel-mail)
      (define-key mail-mode-map [C-f5] 'fidel-to-sera-mail)))
  )

(defun describe-ethiopic-support ()
  "Describe how Emacs supports Ethiopic."
  (interactive)
  (describe-language-support-internal "Ethiopic"))

(set-language-info-alist
 "Ethiopic" '((setup-function . setup-ethiopic-environment)
	      (describe-function . describe-ethiopic-support)
	      (charset . (ethiopic))
	      (sample-text . "$(3$O#U!.(B")
	      (documentation . nil)))

;; for XEmacs (will be obsoleted)
(define-language-environment 'ethiopic
  "Ethiopic"
  #'(lambda ()
      (setq-default quail-current-package
		    (assoc "ethio" quail-package-alist))))

;;; ethiopic.el ends here
