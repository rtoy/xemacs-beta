;;; arabic.el --- pre-loaded support for Arabic. -*- coding: utf-8; -*-

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2002, 2010 Ben Wing.

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

;; Synched up with: Mule 2.3, FSF 21.1.

;;; Code:

;; ISO 8859-6 is such a useless character set that it seems a waste of
;; codespace to dump it. Let me count the ways: 
;; 
;; 1. It doesn't support Persian or Urdu, let alone Sinhalese, despite
;;    plenty of unallocated code points.
;;
;; 2. It doesn't encode all the vowel diacritics (the Harakaat) despite that
;;    they are necessary, even for the Arabs, for basic things like
;;    dictionary entries, children's books, and occasional disambiguation.
;;
;; 3. The Arabs don't use it, they use Windows-1256, which also supports
;;    Persian, at least, as well as the French characters necessary in
;;    Lebanon and North Africa.

;; But; it's necessary for input on X11, so we continue to dump it for now.

(make-coding-system
 'iso-8859-6 'multibyte "ISO 8859-6 (Arabic)"
 '(charsets (ascii control-1 arabic-iso8859-6)
   mnemonic "ArISO"))

;; windows-1256 et al. in mule/windows.el

;; The Mac Arabic coding systems don't have defined MIME names. 

;; #### Decide what to do about the syntax of the Arabic punctuation. 

;;; arabic.el ends here
