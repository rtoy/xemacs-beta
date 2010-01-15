;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Aidan Kehoe <kehoea@parhasard.net>
;; Maintainers: Aidan Kehoe <kehoea@parhasard.net>
;; Created: 2006
;; Keywords: tests

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Previously, we reacted badly to invalid sequences of non-UTF-8 octets
;; when handling UTF-8 encode in ISO 2022.

(when (featurep 'mule)
  ;; This used to crash, at least in debug builds:
  (Assert (decode-coding-string 
           (string ?\33 ?\45 ?\107 ?\306 ?\222 ?\215 ?\306)
           'iso-2022-jp)))
