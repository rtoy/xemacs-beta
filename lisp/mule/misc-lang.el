;;; misc-lang.el --- support for miscellaneous languages (characters) -*- coding: utf-8; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2010 Ben Wing.

;; Keywords: multilingual, character set, coding system

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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IPA (International Phonetic Alphabet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fuck this garbage.  Rather than have bogus charsets like this,
;; just use the Unicode support.
;;; IPA characters for phonetic symbols.
;(make-internal-charset
; 'ipa "IPA (International Phonetic Association)"
; '(dimension
;   1
;   registries ["MuleIPA"]
;   chars 96
;   final ?0
;   graphic 1
;   unicode-map ("unicode/mule-ucs/ipa.txt")
;   short-name "IPA"
;   long-name "IPA"
;   ))

;;; misc-lang.el ends here
