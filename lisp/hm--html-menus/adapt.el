;;; $Id: adapt.el,v 1.1.1.1 1996/12/18 03:34:31 steve Exp $
;;;
;;; Copyright (C) 1993, 1994, 1995  Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	General functions to port Lucid Emacs to GNU Emacs 19.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load directories.
;;;


(defun adapt-xemacsp ()
  "Returns non nil if the editor is the XEmacs."
  (or (string-match "Lucid" emacs-version)
      (string-match "XEmacs" emacs-version)))


(defun adapt-lemacsp ()
  "Returns non nil if the editor is the XEmacs.
Old version, use `adapt-xemacsp' instead of this."
  (or (string-match "Lucid" emacs-version)
      (string-match "XEmacs" emacs-version)))


(defun adapt-emacs19p ()
  "Returns non nil if the editor is the GNU Emacs 19."
  (and 
   (not (adapt-xemacsp))
   (string= (substring emacs-version 0 2) "19")))

;;; Functions, which doesn't exist in both emacses

(defun adapt-region-active-p ()
  "Returns t, if a region is active."
  (if (adapt-xemacsp)
      (mark)
    mark-active))


(if (adapt-emacs19p)
    (progn
      (load-library "lucid")

      (load-library "lmenu")

      (if window-system
	  (require 'font-lock)
	)

      (make-face 'font-lock-comment-face)

      (defun read-number (prompt &optional integers-only)
	"Reads a number from the minibuffer."
	(interactive)
	(let ((error t)
	      (number nil))
	  (if integers-only
	      (while error
		(let ((input-string (read-string prompt)))
		  (setq number (if (string= "" input-string)
				   nil
				 (read input-string)))
		  (if (integerp number)
		      (setq error nil))))
	    (while error
	      (let ((input-string (read-string prompt)))
		(setq number (if (string= "" input-string)
				 nil
			       (read input-string)))		
		(if (numberp number)
		    (setq error nil)))))
	  number))

      (defvar original-read-string-function nil
	"Points to the original Emacs 19 function read-string.")

      (if (not original-read-string-function)
	  (fset 'original-read-string-function
		(symbol-function 'read-string)))

      (defun read-string (prompt &optional initial-contents history)
	"Return a string from the minibuffer, prompting with string PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list."
	(read-from-minibuffer prompt initial-contents nil nil history))

      (defun make-extent (beg end &optional buffer)
	(make-overlay beg end buffer))

      (defun set-extent-property (extent prop value)
	(if (eq prop 'duplicable)
	    (cond ((and value (not (overlay-get extent prop)))
		   ;; If becoming duplicable, 
		   ;; copy all overlay props to text props.
		   (add-text-properties (overlay-start extent)
					(overlay-end extent)
					(overlay-properties extent)
					(overlay-buffer extent)))
		  ;; If becoming no longer duplicable, remove these text props.
		  ((and (not value) (overlay-get extent prop))
		   (remove-text-properties (overlay-start extent)
					   (overlay-end extent)
					   (overlay-properties extent)
					   (overlay-buffer extent))))
	  ;; If extent is already duplicable, put this property
	  ;; on the text as well as on the overlay.
	  (if (overlay-get extent 'duplicable)
	      (put-text-property  (overlay-start extent)
				  (overlay-end extent)
				  prop value (overlay-buffer extent))))
	(overlay-put extent prop value))
      
      (defun set-extent-face (extent face)
	(set-extent-property extent 'face face))
      
      (defun delete-extent (extent)
	(set-extent-property extent 'duplicable nil)
	(delete-overlay extent))
      
;      (defun make-extent (from to &optional buffer)
;	"Make extent for range [FROM, TO) in BUFFER -- BUFFER defaults to 
;current buffer.  Insertions at point TO will be outside of the extent;
;insertions at FROM will be inside the extent (and the extent will grow.).
;This is only a simple emulation of the Lucid Emacs extents !"
;	(list 'extent from to buffer))
;
;      (defun set-extent-face (extent face)
;	"Make the given EXTENT have the graphic attributes specified by FACE.
;This is only a simple emulation of the Lucid Emacs extents !"
;	(put-text-property (car (cdr extent))
;			   (car (cdr (cdr extent)))
;			   'face
;			   face
;			   (car (cdr (cdr (cdr extent))))))
;
;      (defun delete-extent (extent_obj)
;	"Remove EXTENT from its buffer; this does not modify the buffer's text,
;only its display properties.
;This is only a simple emulation of the Lucid Emacs extents !"
;	(remove-text-properties (car (cdr extent_obj))
;				(car (cdr (cdr extent_obj)))
;				(list 'face nil)
;				(car (cdr (cdr (cdr extent_obj))))))
;      

      (if (not (fboundp 'emacs-pid))
	  (defun emacs-pid ()
	    "Return the process ID of Emacs, as an integer.
This is a dummy function for old versions of the Emacs 19.
You should install a new version, which has `emacs-pid' implemented."
	    0)
	)

      (if (not (fboundp 'facep))
	  (defun facep (object)
	    "Whether OBJECT is a FACE.
It's only a dummy function in the Emacs 19, which returns always nil."
	    nil))
      
;      (if (not (fboundp 'set-extent-property))
;	  (defun set-extent-property (extent  property value)
;	    "Change a property of an extent.
;Only a dummy version in Emacs 19."))

      ))
    

(provide 'adapt)
