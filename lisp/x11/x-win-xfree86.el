;;; x-win-xfree86.el --- runtime initialization for XFree86 servers
;; Copyright (C) 1995 Sun Microsystems, Inc.
;; Copyright (C) 1995 Ben Wing.

;; Author: Ben Wing
;; Keywords: terminals

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

;;; Commentary:

;; This file is loaded by x-win.el at run-time when we are sure that XEmacs
;; is running on the display of something running XFree86 (Linux,
;; NetBSD, FreeBSD, and perhaps other Intel Unixen).

;;; #### bleck!!! Use key-translation-map!

;; For no obvious reason, shift-F1 is called F13, although Meta-F1 and
;; Control-F1 have normal names.

(let ((mapping '((f13 . (shift f1))
		 (f14 . (shift f2))
		 (f15 . (shift f3))
		 (f16 . (shift f4))
		 (f17 . (shift f5))
		 (f18 . (shift f6))
		 (f19 . (shift f7))
		 (f20 . (shift f8))
		 (f21 . (shift f9))
		 (f22 . (shift f10))
		 (f23 . (shift f11))
		 (f24 . (shift f12)))))

  ;; now define them and also the control, meta, and meta-control versions.
  (while mapping
    (let* ((foo (caar mapping))
	   (bar (cdar mapping))
	   (foo (if (listp foo) foo (list foo)))
	   (bar (if (listp bar) bar (list bar))))
      (let ((mods '(() (control) (meta) (meta control))))
	(while mods
	  (let ((k1 (vector (append (car mods) foo)))
		(k2 (vector (append (car mods) bar))))
	    (define-key global-map k1 k2))
	  (setq mods (cdr mods))))
      (setq mapping (cdr mapping)))))
