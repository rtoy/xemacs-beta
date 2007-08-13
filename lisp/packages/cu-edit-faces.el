;;; edit-faces.el -- interactive face editing mode

;; Copyright (C) 1997 Jens Lautenbacher
;; 
;; This file is part of XEmacs.
;; 
;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF.

;;; Just another TTPC (Totally Trivial Piece of Code (TM)). All the
;;; needed functionality for editing faces is already in custom.el. So
;;; why don't use it, you may ask. OK, here I am...

(require 'custom)
(require 'cl)

;;;###autoload
(defun cu-edit-faces ()
  (interactive)
  (let (tmp-list elem)
    (put 'available-faces 'custom-group nil)
    (setq tmp-list (sort (face-list)
 			'(lambda (one two)
 			   (if (string< (symbol-name one)
 					(symbol-name two)) t
 			     nil))))
    (while (setq elem (pop tmp-list))
      (custom-add-to-group 'available-faces elem 'custom-face))
    (message "Please stand by while generating list of faces...")
	   (customize 'available-faces)))

(provide 'cu-edit-faces)

;;; cu-edit-faces.el ends here.
