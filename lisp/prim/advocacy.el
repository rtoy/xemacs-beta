;;; advocacy.el -- blatant XEmacs self promotion
;; Copyright (C) 1996 Miranova Systems, Inc.
;; Copyright (C) 1996 Chuck Thompson <cthomp@xemacs.org>

;; Original Author:  Steve L Baur <steve@miranova.com>

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

;;;###autoload
(defvar xemacs-praise-sound-file "sounds/im_so_happy.au"
  "The name of an audio file containing something to play
when praising XEmacs")

;;;###autoload
(defvar xemacs-praise-message
  "All Hail XEmacs!\n"
  "What to praise XEmacs with")

;;;###autoload
(defun praise-be-unto-xemacs (&optional arg)
  "All Hail XEmacs!"
  (interactive "_p")
  (save-window-excursion
    (let ((count (if (null arg)
		     0
		   arg))
 	  (max-faces (length (face-list))))
      (with-output-to-temp-buffer "*Praise*"
	(set-buffer "*Praise*")
        (if (glyphp xemacs-logo)
            (let ((p (point)))
              (insert "\n")
              (indent-to (startup-center-spaces xemacs-logo))
              (set-extent-begin-glyph (make-extent (point) (point)) xemacs-logo)
              (insert "\n\n")))
	(while (> count 0)
	  (progn
	    (insert-face xemacs-praise-message
			 (get-face (nth (random max-faces) (face-list))))
	    (setq count (- count 1))))))
    (let ((sound-file
	   (or (and (file-exists-p xemacs-praise-sound-file)
		    xemacs-praise-sound-file)
	       (and (file-exists-p
		     (concat data-directory xemacs-praise-sound-file))
		    (concat data-directory xemacs-praise-sound-file)))))
      (if (and (device-sound-enabled-p) sound-file)
	  (progn
	    (sit-for 0)
	    (play-sound-file sound-file))
	(sit-for 10)))))

;;;###autoload
(defun praise-be-unto-emacs (&optional arg)
  (interactive "_p")
  (error "Obsolete function.  Use `praise-be-unto-xemacs'."))
(make-obsolete 'praise-be-unto-emacs "use praise-be-unto-xemacs")

;;;###autoload
(defun all-hail-xemacs (&optional arg)
  "All Hail XEmacs!"
  (interactive "_p")
  (praise-be-unto-xemacs arg))

;;;###autoload
(defun all-hail-emacs (&optional arg)
  (interactive "_p")
  (error "Obsolete function.  Use `all-hail-xemacs'."))
(make-obsolete 'all-hail-emacs "use all-hail-xemacs")
