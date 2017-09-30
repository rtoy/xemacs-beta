;;; thai.el --- support for Thai -*- coding: utf-8; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 2010 Ben Wing.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Thai

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

;;; Synched up with: Emacs 21.1 (language/thai.el).

;;; Commentary:

;;; Code:

;; XEmacs; drop the thai-iso8859-11 charset, which is identical to thai-tis620
;; for our purposes, and confuses the digit-char checks in the test suite on
;; non-unicode-internal.

(make-coding-system
 'tis-620 'multibyte "Thai/TIS620"
 '(charsets (ascii thai-tis620)
   mnemonic "Thai"
   post-read-conversion thai-post-read-conversion
   documentation "8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1)"
   aliases (iso-8859-11 thai-tis620 th-tis620 tis620)))

(set-language-info-alist
 "Thai" '((tutorial . "TUTORIAL.th")
	  (charset thai-tis620 thai)
	  (coding-system thai-tis620)
	  (coding-priority thai-tis620)
	  (nonascii-translation . thai-tis620)
	  ;;(input-method . "thai-kesmanee")@@#### not working in XEmacs quail
	  (unibyte-display . thai-tis620)
	  (features thai-util)
	  (sample-text 
	   . (thai-compose-string
	      (copy-sequence "Thai (ภาษาไทย)	สวัสดีครับ / สวัสดีค่ะ")))))

(put-char-table 'thai-tis620
		'(("\\c0\\c4\\|\\c0\\(\\c2\\|\\c3\\)\\c4?" .
		   thai-composition-function))
		composition-function-table)

(provide 'thai)

;;; thai.el ends here
