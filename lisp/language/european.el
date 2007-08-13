;;; european.el --- Support for European languages

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, European

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

;; For Europeans, five character sets ISO8859-1,2,3,4,9 are supported.

;;; Code:

;; For syntax of Latin-1 characters.
(loop for c from 64 to 127              ; from '.AN@' to 'N'
      do (modify-syntax-entry (make-char 'latin-iso8859-1 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-1 32) "w") ; no-break space
(modify-syntax-entry ?NW "_")
(modify-syntax-entry ?Nw "_")

;; For syntax of Latin-2
(loop for c in '(?.BN! ?N# ?N% ?N& ?N) ?N* ?N+ ?N, ?N. ?N/ ?N1 ?N3 ?N5 ?N6 ?N9 ?N: ?N; ?N<)
      do (modify-syntax-entry c "w"))

(loop for c from 62 to 126
      do (modify-syntax-entry (make-char 'latin-iso8859-2 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-2 32) "w") ; no-break space
(modify-syntax-entry ?NW ".")
(modify-syntax-entry ?Nw ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EUROPEANS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-language-environment 'european
  "European (for Latin-1 through Latin-5)"
  (lambda ()
    (set-coding-category-system 'iso-8-designate 'iso-8859-1)
    (set-coding-priority-list '(iso-8-designate iso-8-1))
    ;;(setq locale-coding-system            'no-conversion) ; iso-8859-1
    (set-default-buffer-file-coding-system  'no-conversion) ; iso-8859-1
    ;;(set-buffer-file-coding-system-for-read 'no-conversion) ; iso-8859-1
    ;;(setq display-coding-system 'iso-8859-1)
    ;;(setq keyboard-coding-system 'iso-8859-1)
    ;; (setq-default quail-current-package
    ;;               (assoc "latin-1" quail-package-alist))
    ))

;; (make-coding-system
;;  'iso-8859-1 2 ?X
;;  "Coding-system used in X as Compound Text Encoding."
;;  '((ascii t) (latin-iso8859-1 t) nil nil
;;    nil ascii-eol ascii-cntl))

;; CTEXT is an alias for ISO-8859-1
;; (define-coding-system-alias 'iso-8859-1 'ctext)

;; (make-coding-system
;;  'iso-8859-2 2 ?2 "MIME ISO-8859-2"
;;  '((ascii t) (latin-iso8859-2 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'iso-8859-2 'iso2022 "MIME ISO-8859-2"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"
   ))

;; (make-coding-system
;;  'iso-8859-3 2 ?3 "MIME ISO-8859-3"
;;  '((ascii t) (latin-iso8859-3 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'iso-8859-3 'iso2022 "MIME ISO-8859-3"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"
   ))

;; (make-coding-system
;;  'iso-8859-4 2 ?4 "MIME ISO-8859-4"
;;  '((ascii t) (latin-iso8859-4 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'iso-8859-4 'iso2022 "MIME ISO-8859-4"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"
   ))

;; (make-coding-system
;;  'iso-8859-9 2 ?9 "MIME ISO-8859-9"
;;  '((ascii t) (latin-iso8859-9 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil))

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

(defun setup-european-environment ()
  "Setup multilingual environment (MULE) for European languages users.
It actually reset MULE to the default status, and
set quail-latin-1 as the default input method to be selected.
See also the documentation of setup-english-environment."
  (setup-english-environment)
  (setq default-input-method '("European" . "quail-latin-1")))

(defun describe-european-support ()
  "Describe how Emacs support European languages."
  (interactive)
  (describe-language-support-internal "European"))

(set-language-info-alist
 "European" '((setup-function . setup-european-environment)
	      (describe-function . describe-european-support)
	      (charset . (ascii latin-iso8859-1 latin-iso8859-2
			  latin-iso8859-3 latin-iso8859-4 latin-iso8859-9))
	      (coding-system . (iso-8859-1 iso-8859-2 iso-8859-3
				iso-8859-4 iso-8859-9))
	      (sample-text
	       . "Hello, Hej, Tere, Hei, Bonjour, Gr.AN|N_ Gott, Ciao, N!Hola!")
	      (documentation . "\
Almost all of European languages are supported by the character sets and
coding systems listed below.
To input them, LEIM (Libraries for Emacs Input Methods) should have been
installed.")
	      ))

(let ((languages '("French" "German" "Spanish" "Italian"
		   ;; We have to list much more European langauges here.
		   ))
      (val '("quail-latin-1" quail-use-package "quail/latin")))
  (while languages
    (register-input-method (car languages) val)
    (setq languages (cdr languages))))

(register-input-method "Esperanto"
		       '("quail-esperanto" quail-use-package "quail/latin"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Italian"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-italian" "quail-itln"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Spanish"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-spanish" "quail-spnsh"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "French"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-french" "quail-frnch" "quail-azerty"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "German"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-german" "quail-grmn"))

(register-input-method "Scandinavian"
		       '("quail-scandinavian" quail-use-package "quail/latin"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Icelandic"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-icelandic" "quail-iclndc"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Danish"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-danish" "quail-dnsh"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Norwegian"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-norwegian" "quail-nrwgn"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Swedish"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-swedish" "quail-swdsh"))

(mapcar (function
	 (lambda (method)
	   (register-input-method "Finish"
				  (cons method
					'(quail-use-package "quail/latin")))
	   ))
	'("quail-finnish" "quail-fnnsh"))

(register-input-method "Turkish"
		       '("quail-turkish" quail-use-package "quail/latin"))

;;; european.el ends here
