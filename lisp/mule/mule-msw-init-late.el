;;; mule-msw-init-late.el --- initialization code for MS Windows under MULE
;;; Copyright (C) 2001, 2002 Ben Wing.

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; mapping between XEmacs charsets and code pages.  something like this
;; will might around once all the Unicode code is written, so we know how
;; to choose the right font.  (perhaps "code pages" will become "Unicode
;; subranges"; they're more or less equivalent under Windows from a font
;; perspective.) But ...  in reality, we can just query the charset for its
;; Unicode ranges, and the "charset ID" is not a good indicator of what a
;; particular font supports; e.g. there's no charset ID at all for Indian
;; fonts, but Windows clearly supports them. (The docs say that Indian
;; support is "all Unicode"; i.e. charset ID's are on their way out.  I
;; guess we're supposed to query the font for what ranges it supports, and
;; what its preferred range is.)

(let ((l '((ascii . 1252)
	   (latin-iso8859-2 . 1250)
	   (cyrillic-iso8859-5 . 1251)
	   (latin-iso8859-1 . 1252)
	   (greek-iso8859-7 . 1253) 
	   (latin-iso8859-9 . 1254)
	   (hebrew-iso8859-8 . 1255)
	   ;; (arabic-iso8859-6 . 1256)
	   (latin-iso8859-4 . 1257)
	   (vietnamese-viscii-lower . 1258)
	   (vietnamese-viscii-upper . 1258)
	   ;; (thai-tis620 . 874)
	   (latin-jisx0201 . 932)
	   (katakana-jisx0201 . 932)
	   ;; (japanese-jisx0208-1978 . 932)
	   (japanese-jisx0208 . 932)
	   (japanese-jisx0212 . 932)
	   (chinese-gb2312 . 936)
	   (korean-ksc5601 . 949)
	   (chinese-big5-1 . 950)
	   (chinese-big5-2 . 950))))
  (while l
    (let ((charset (car (car l)))
	  (code-page (cdr (car l))))
    (mswindows-set-charset-code-page charset code-page)
    (setq l (cdr l)))))
