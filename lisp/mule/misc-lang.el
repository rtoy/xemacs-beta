;;; misc-lang.el --- support for miscellaneous languages (characters) -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001 Ben Wing.

;; Keywords: multilingual, character set, coding system

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IPA (International Phonetic Alphabet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IPA characters for phonetic symbols.
(make-charset 'ipa "IPA (International Phonetic Association)"
	      '(dimension
		1
		registries ["MuleIPA"]
		chars 96
		columns 1
		direction l2r
		final ?0
		graphic 1
		short-name "IPA"
		long-name "IPA"))

;; XEmacs; these are Latin, it's not useful to put word boundaries between
;; them and ASCII.
(modify-category-entry 'ipa ?l nil t)

;; XEmacs; why are these Latin? See the following:
;;
;; (let ((scripts
;;        (mapcar #'(lambda (character)
;;                    (car
;;                     (split-string
;;                      (cadr (assoc "Name" (describe-char-unicode-data
;;                                           character))))))
;;                (loop
;;                  for i from 33 to 127
;;                  if (not (eql -1 (char-to-unicode (make-char 'ipa i))))
;;                  nconc (list (make-char 'ipa i))))))
;;   (mapcar #'(lambda (script)
;;               (cons script (count script scripts :test #'equal)))
;;           (remove-duplicates scripts :test #'equal)))
;; => (("GREEK" . 1) ("LATIN" . 55) ("MODIFIER" . 3))


;;; misc-lang.el ends here
