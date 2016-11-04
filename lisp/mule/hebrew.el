;;; hebrew.el --- Support for Hebrew -*- coding: utf-8; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 2010 Ben Wing.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Hebrew

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

;;; Commentary:

;;  ISO 8859-8 (Hebrew) support.

;;; Code:

;; Syntax of Hebrew characters
(loop for c from 96 to 122
      do (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w"))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 32) "w") ; no-break space


(make-coding-system
 'iso-8859-8 'multibyte "ISO 8859-8 (Hebrew)"
 '(charsets (ascii control-1 hebrew-iso8859-8)
   mnemonic "MIME/Hbrw"))

(make-coding-system
 'ctext-hebrew 'iso2022
 "ISO-8859-8-E (Hebrew, explicit directional coding)"
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   mnemonic "CText/Hbrw"
   ))

(set-language-info-alist
 "Hebrew" '((charset hebrew-iso8859-8 hebrew)
	    (coding-system iso-8859-8)
	    (coding-priority iso-8859-8)
            ;; Not available in packages. 
	    ;; (input-method . "hebrew")
	    (sample-text . "Hebrew	שלום")
	    (documentation . "Right-to-left writing is not yet supported.")
	    ))

;;; hebrew.el ends here
