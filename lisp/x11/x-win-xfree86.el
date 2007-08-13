;;; x-win-xfree86.el --- runtime initialization for XFree86 servers
;; Copyright (C) 1995 Sun Microsystems, Inc.
;; Copyright (C) 1995 Ben Wing.

;; Author: Ben Wing
;; Author: Martin Buchholz (rewritten to use function-key-map)
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

;;; #### Counter-bleck!! We shouldn't override a user binding for F13.
;;; So we use function-key-map for now.
;;; When we've implemented a fallback-style equivalent of
;;; keyboard-translate-table, we'll use that instead. (mrb)

;; For no obvious reason, shift-F1 is called F13, although Meta-F1 and
;; Control-F1 have normal names.

(loop for (x-key key sane-key) in
  '(("F13" f13 f1)
    ("F14" f14 f2)
    ("F15" f15 f3)
    ("F16" f16 f4)
    ("F17" f17 f5)
    ("F18" f18 f6)
    ("F19" f19 f7)
    ("F20" f20 f8)
    ("F21" f21 f9)
    ("F22" f22 f10)
    ("F23" f23 f11)
    ("F24" f24 f12))
  do
  (when (and (x-keysym-on-keyboard-p x-key)
	     (not (x-keysym-on-keyboard-sans-modifiers-p x-key)))
    ;; define also the control, meta, and meta-control versions.
    (loop for mods in '(() (control) (meta) (meta control)) do
      (define-key function-key-map `[(,@mods ,key)] `[(shift ,@mods ,sane-key)])
      )))

;; (let ((mapping '((f13 . (shift f1))
;; 		 (f14 . (shift f2))
;; 		 (f15 . (shift f3))
;; 		 (f16 . (shift f4))
;; 		 (f17 . (shift f5))
;; 		 (f18 . (shift f6))
;; 		 (f19 . (shift f7))
;; 		 (f20 . (shift f8))
;; 		 (f21 . (shift f9))
;; 		 (f22 . (shift f10))
;; 		 (f23 . (shift f11))
;; 		 (f24 . (shift f12)))))
;;
;;   ;; now define them and also the control, meta, and meta-control versions.
;;   (while mapping
;;     (let* ((foo (caar mapping))
;; 	   (bar (cdar mapping))
;; 	   (foo (if (listp foo) foo (list foo)))
;; 	   (bar (if (listp bar) bar (list bar))))
;;       (let ((mods '(() (control) (meta) (meta control))))
;; 	(while mods
;; 	  (let ((k1 (vector (append (car mods) foo)))
;; 		(k2 (vector (append (car mods) bar))))
;; 	    (define-key global-map k1 k2))
;; 	  (setq mods (cdr mods))))
;;       (setq mapping (cdr mapping)))))
