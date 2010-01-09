;;; windows.el --- Support Windows code pages

;; Copyright (C) 2005, 2010 Ben Wing.

;; Keywords: multilingual, Windows

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

;; There's a file in GNU Emacs called international/codepage.el, but it
;; has nothing to do with this file.  It uses CCL for conversion, which
;; we don't need.  It's too annoying to put all the charsets for the
;; various code pages in individual language-specific files, and at some
;; point soon most of the info in those language files won't be necessary
;; because it will be derived from Unicode tables.

;Make a one-dimension Windows charset corresponding to a specified code page.
;CODEPAGE is the number of the code page.  SCRIPT is a symbol indicating the
;writing system of the code page, e.g. `latin' or `cyrillic'.  NAME is a
;string describing the code page, e.g. \"Eastern Europe\" (code page 1250).
;There should be a CP###.TXT file in the directory
;etc/unicode/unicode-consortium/VENDORS/MICSFT/WINDOWS.
(defun make-one-dimension-windows-charset (codepage script name)
  (make-internal-charset
   (intern (format "%s-windows-%s" script num))
   (format "Windows code page %s (%s)" num name)
   `(dimension
     1
     chars 128
     unicode-map (,(format "unicode/unicode-consortium/VENDORS/MICSFT/WINDOWS/CP%d.TXT" num) #x80)
     short-name ,(format "Windows %s (%s)" num name)
     long-name ,(format "Windows code page %s (%s)" num name)
     )))

;Make a two-dimension Windows charset corresponding to a specified code
;page.  CODEPAGE is the number of the code page.  SCRIPT is a symbol
;indicating the writing system of the code page, e.g. `chinese'.  NAME is a
;string describing the code page, e.g. \"Simplified Chinese\" (code page
;936).  The charset has characters in the range (L1, L2) - (H1, H2),
;inclusive.  There should be a CP###.TXT file in the directory
;etc/unicode/unicode-consortium/VENDORS/MICSFT/WINDOWS.
(defun make-two-dimension-windows-charset (codepage script name l1 l2 h1 h2)
  (make-internal-charset
   (intern (format "%s-windows-%s" script num))
   (format "Windows code page %s (%s)" num name)
   `(dimension
     2
     chars (,(1+ (- h1 l1)) ,(1+ (- h2 l2)))
     offset (,l1 ,l2)
     unicode-map (,(format "unicode/unicode-consortium/VENDORS/MICSFT/WINDOWS/CP%d.TXT" num) #x8000)
     short-name ,(format "Windows %s (%s)" num name)
     long-name ,(format "Windows code page %s (%s)" num name)
     )))
