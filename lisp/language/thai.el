;;; thai.el --- Support for Thai

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Thai

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

;;; Commentary:

;; For Thai, the character set TIS620 is supported.

;;; Code:

(define-language-environment 'thai
  "Thai"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'tis620)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    (set-default-buffer-file-coding-system 'tis620)
    ;; (setq-default quail-current-package (assoc "thai" quail-package-alist))
    ))

;; (make-coding-system
;;  'th-tis620 2 ?T
;;  "Coding-system used for ASCII(MSB=0) & TIS620(MSB=1)."
;;  '((ascii t) (thai-tis620 t) nil nil
;;    nil ascii-eol))
;; (put 'th-tis620 'post-read-conversion 'thai-post-read-conversion)
;; (put 'th-tis620 'pre-write-conversion 'thai-pre-write-conversion)

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

;;(define-coding-system-alias 'th-tis620 'tis620)

(register-input-method
 "Thai" '("quail-thai" quail-use-package "quail/thai"))

(defun setup-thai-environment ()
  "Setup multilingual environment (MULE) for Thai."
  (interactive)
  (setup-english-environment)
  (setq coding-category-iso-8-1 'th-tis620)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1))

  (setq-default buffer-file-coding-system 'th-tis620)

  (setq default-input-method '("Thai" . "quail-thai"))
  )

(defun describe-thai-support ()
  "Describe how Emacs supports Thai."
  (interactive)
  (describe-language-support-internal "Thai"))

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (setup-function . setup-thai-environment)
	  (describe-function . describe-thai-support)
	  (charset . (thai-tis620))
	  (coding-system . (th-tis620))
	  (sample-text . "Thai (,T@RIRd7B(B)		,TJ0GQ1J04U1$0CQ1:(B, ,TJ0GQ1J04U10$h1P(B")
	  (documentation . nil)))

;;; thai.el ends here
