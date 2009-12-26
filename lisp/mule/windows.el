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
  

