;;; windows.el --- Support Windows code pages

;; Copyright (C) 2005 Ben Wing.

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
  
(let ((charsets '((874 thai "Thai")
		  (1250 latin "Eastern Europe")
		  (1251 cyrillic "Cyrillic")
		  (1252 latin "ANSI")
		  (1253 greek "Greek")
		  (1254 latin "Turkish")
		  (1255 hebrew "Hebrew")
		  (1256 arabic "Arabic")
		  (1257 latin "Baltic Rim")
		  (1258 latin "Vietnamese"))))
  (loop for (num script name) in charsets do
    (make-charset (intern (format "%s-windows-%s" script num))
		  (format "Windows code page %s (%s)" num name)
		  `(dimension
		    1
		    chars 128
	            unicode-map (,(format "unicode/unicode-consortium/CP%d.TXT" num) #x80)
		    short-name ,(format "Windows %s (%s)" num name)
		    long-name ,(format "Windows code page %s (%s)" num name)
		    ))))

(let ((charsets '((932 japanese "Japanese" #x81 #x40 #xfe #xfe)
		  (936 chinese "Simplified Chinese" #x81 #x40 #xfe #xfe)
		  (949 korean "Korean" #x81 #x41 #xfe #xfe)
		  (950 chinese "Traditional Chinese" #xa1 #x40 #xfe #xfe)
		  )))
  (loop for (num script name l1 l2 h1 h2) in charsets do
    (make-charset (intern (format "%s-windows-%s" script num))
		  (format "Windows code page %s (%s)" num name)
		  `(dimension
		    2
		    chars (,(1+ (- h1 l1)) ,(1+ (- h2 l2)))
		    offset (,l1 ,l2)
		    short-name ,(format "Windows %s (%s)" num name)
		    long-name ,(format "Windows code page %s (%s)" num name)
		    ))))


