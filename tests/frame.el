;;; Test geometry setting for frames.
;;;
;;; Copyright (C) 1997 Martin Buchholz
;;;
;; This file is part of XEmacs.
;; 
;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;; 
;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

(defmacro check-frame-geometry (xx yy)
  `(loop for frame in (list nil (selected-frame))
	 do
	 (assert (eq (frame-property frame 'top)  ,yy))
	 (assert (eq (frame-property frame 'left) ,xx))
	 (assert (eq (frame-property frame 'top)  ,yy))
	 (assert (eq (frame-property frame 'left) ,xx))
	 (loop for plist in
	       (list (frame-properties)
		     (frame-properties nil)
		     (frame-properties (selected-frame)))
	       do
	       (assert (eq (plist-get plist 'top)  ,yy))
	       (assert (eq (plist-get plist 'left) ,xx)))))

(loop for (x y) in '((0 0) (1 1) (3 3) (9 9) (10 20) (20 40) (40 80) (100 200))
      do
      (loop for frame in (list nil (selected-frame))
	    do
	    (set-frame-properties frame `(left ,x top ,y))
	    (check-frame-geometry x y)
	    (set-frame-property frame 'top (+ y 3))
	    (check-frame-geometry x (+ y 3))
	    (set-frame-property frame 'left (+ x 3))
	    (check-frame-geometry (+ x 3) (+ y 3))))
