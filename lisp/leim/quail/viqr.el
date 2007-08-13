;; quail/viqr.el -- Quail packages for inputting Vietnamese with VIQR system

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, input method, latin

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'quail)
(require 'language/viet-util)

;; `viet-viqr-alist' is an alist of Vietnamese characters vs
;; corresponding VIQR strings.  We create Quail map which maps VIQR
;; strings to corresponding Vietnamese characters.

(defmacro viet-quail-define-rules ()
  (cons 'quail-define-rules
	(let ((l viet-viqr-alist)
	      rules)
	  (while l
	    (setq rules (cons (list (cdr (car l)) (car (car l))) rules))
	    (setq l (cdr l)))
	  rules)))

(quail-define-package "quail-viqr" "Vietnamese" "VIQR" t
		      "Vietnamese inputting method with VIQR mnemonic system:

    effect   | postfix | examples
 ------------+---------+----------
    breve    |    (    | a( -> ,1e(B
  circumflex |    ^    | a^ -> ,1b(B
    horn     |    +    | o+ -> ,1=(B
 ------------+---------+----------
    acute    |    '    | a' -> ,1a(B
    grave    |    `    | a` -> ,1`(B
  hook above |    ?    | a? -> ,1d(B
    tilde    |    ~    | a~ -> ,1c(B
   dot below |    .    | a. -> ,1U(B
 ------------+---------+----------
    d bar    |   dd    | dd -> ,1p(B
 ------------+---------+----------
  no compose |    \    | a\. -> a.
 ------------+---------+----------
  combination|   (~    | a(~ -> ,1G(B
" nil t t nil nil t)


(viet-quail-define-rules)

;;; quail/viqr.el ends here