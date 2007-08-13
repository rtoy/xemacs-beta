;;; $Id: adapt.el,v 1.4 1997/05/29 23:49:41 steve Exp $
;;;
;;; Copyright (C) 1993 - 1997  Heiko Muenkel
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
;;;	General functions to port XEmacs functions to GNU Emacs 19.
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

;;; Functions, which don't exist in both emacs versions

(defun adapt-region-active-p ()
  "Returns t, if a region is active."
  (if (adapt-xemacsp)
      (mark)
    mark-active))

(if (not (fboundp 'file-remote-p))
    (defun file-remote-p (file)
      "Test wether file resides on the local system.
The special value 'unknown is returned if no remote file acess package
has been loaded."
      (if (not (featurep 'ange-ftp))
	  (require 'ange-ftp))
      (if (not (fboundp 'ange-ftp-ftp-p))
	  nil ; better than nothing, if no ange-ftp-ftp-p exists
	(ange-ftp-ftp-path file))))


;;; Functions, which don't exist in the Emacs 19
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

      (if (not (fboundp 'region-active-p))
	  (defun region-active-p ()
	    "Non-nil iff the region is active.
If `zmacs-regions' is true, this is equivalent to `region-exists-p'.
Otherwise, this function always returns false."
	    (adapt-region-active-p)))

      (if (not (fboundp 'next-command-event))
	  (defun next-command-event (&optional event prompt)
	    "Unlike the XEmacs version it reads the next event, if
it is a command event or not.

Return the next available \"user\" event.
 Pass this object to `dispatch-event' to handle it.

 If EVENT is non-nil, it should be an event object and will be filled in
 and returned; otherwise a new event object will be created and returned.
 If PROMPT is non-nil, it should be a string and will be displayed in the
 echo area while this function is waiting for an event.

 The event returned will be a keyboard, mouse press, or mouse release event.
 If there are non-command events available (mouse motion, sub-process output,
 etc) then these will be executed (with `dispatch-event') and discarded.  This
 function is provided as a convenience; it is equivalent to the lisp code

	 (while (progn
		  (next-event event prompt)
		  (not (or (key-press-event-p event)
			   (button-press-event-p event)
			   (button-release-event-p event)
			   (misc-user-event-p event))))
	    (dispatch-event event))"
	    (message prompt)
	    (or event
		(read-event))))

      (if (not (fboundp 'button-event-p))
	  (defun button-event-p (obj)
	    "True if OBJ is a button-press or button-release event object."
	    (and (eventp obj)
		 (or (eq 'mouse-1 (event-basic-type obj))
		     (eq 'mouse-2 (event-basic-type obj))
		     (eq 'mouse-3 (event-basic-type obj))
		     (eq 'down-mouse-1 (event-basic-type obj))
		     (eq 'down-mouse-2 (event-basic-type obj))
		     (eq 'down-mouse-3 (event-basic-type obj))
		     (eq 'up-mouse-1 (event-basic-type obj))
		     (eq 'up-mouse-2 (event-basic-type obj))
		     (eq 'up-mouse-3 (event-basic-type obj))
		     (eq 'drag-mouse-1 (event-basic-type obj))
		     (eq 'drag-mouse-2 (event-basic-type obj))
		     (eq 'drag-mouse-3 (event-basic-type obj))
		     ))))

      (if (not (fboundp 'button-drag-event-p))
	  (defun button-drag-event-p (obj)
	    "True if OBJ is a mouse-button-drag event object."
	    (and (button-event-p obj)
		 (member 'drag (event-modifiers obj)))))

      (if (not (fboundp 'button-press-event-p))
	  (defun button-press-event-p (obj)
	    "True if OBJ is a mouse-button-press event object."
	    (and (button-event-p obj)
		 (member 'down (event-modifiers obj)))))

      (if (not (fboundp 'button-release-event-p))
	  (defun button-release-event-p (obj)
	    "True if OBJ is a mouse-button-release event object."
	    (and (button-event-p obj)
		 (not (button-press-event-p obj)))))

      (if (not (fboundp 'button-click-event-p))
	  (defun button-click-event-p (obj)
	    "True if OBJ is a click event obkect."
	    ;; only for the Emacs 19
	    ;; doesn't exist and can't (?) exist in the XEmacs
	    (and (button-event-p obj)
		 (member 'click (event-modifiers obj)))))
      
      (if (not (fboundp 'mouse-event-p))
	  (defun mouse-event-p (obj)
	    "True if OBJ is a button-press, button-release, or mouse-motion event object."
	    (or (button-event-p obj)
		(member 'drag (event-modifiers obj)))))

      (if (not (fboundp 'event-window))
	  (defun event-window (event)
	    "Return the window of the given mouse EVENT.
This may be nil if the event occurred in the border or over a toolbar.
The modeline is considered to be in the window it represents.

If the EVENT is a mouse drag event, then the end event window is returned."
	    (if (button-drag-event-p event)
		(and (listp event)
		     (third event)
		     (listp (third event))
		     (windowp (car (third event)))
		     (car (third event)))
	      (and (eventp event)
		   (listp event)
		   (second event)
		   (listp (second event))
		   (windowp (car (second event)))
		   (car (second event))))))

;		   (listp (cdr event))
;		   (listp (car (cdr event)))
;		   (windowp (car (car (cdr event))))
;		   (car (car (cdr event))))))

      (if (not (fboundp 'event-buffer))
	  (defun event-buffer (event)
	    "Given a mouse-motion, button-press, or button-release event,
return the buffer on which that event occurred.  This will be nil for 
non-mouse events.  If event-over-text-area-p is nil, this will also be nil."
	    (if (button-event-p event)
		(window-buffer (event-window event)))))


      (if (not (fboundp 'event-closest-point))
	  (defun event-closest-point (event)
	    "Return the character position of the given mouse EVENT.
If the EVENT did not occur over a window or over text, return the
closest point to the location of the EVENT.  If the Y pixel position
overlaps a window and the X pixel position is to the left of that
window, the closest point is the beginning of the line containing the
Y position.  If the Y pixel position overlaps a window and the X pixel
position is to the right of that window, the closest point is the end
of the line containing the Y position.  If the Y pixel position is
above a window, return 0.  If it is below a window, return the value
of (window-end).

If the EVENT is a drag event, the event-end will be used."
	    (if (button-drag-event-p event)
		(posn-point (event-end event))
	      (posn-point (event-start event)))))

      (if (not (fboundp 'add-minor-mode))
	  (defun add-minor-mode (toggle 
				 name 
				 &optional 
				 keymap 
				 after 
				 toggle-fun)
	    "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.
TOGGLE is a symbol whose value as a variable specifies whether the
minor mode is active.  NAME is the name that should appear in the
modeline (it should be a string beginning with a space).  KEYMAP is a
keymap to make active when the minor mode is active.  AFTER is the
toggling symbol used for another minor mode.  If AFTER is non-nil,
then it is used to position the new mode in the minor-mode alists.

TOGGLE-FUN is only a dummy variable in the Emacs 19. In the XEmacs
it has the following description:
TOGGLE-FUN specifies an interactive function that is called to toggle
the mode on and off; this affects what happens when button2 is pressed
on the mode, and when button3 is pressed somewhere in the list of
modes.  If TOGGLE-FUN is nil and TOGGLE names an interactive function,
TOGGLE is used as the toggle function.

Example:  (add-minor-mode 'view-minor-mode \" View\" view-mode-map)

WARNING: THIS FUNCTION ISN'T READ YET."
	    (if after
		(add-minor-mode-1 toggle name keymap after)
	      (if (not (assq toggle minor-mode-alist))
		  (progn
		    (setq minor-mode-alist
			  (cons (list toggle name)
				minor-mode-alist))))
	      (if (not (assq toggle minor-mode-map-alist))
		  (progn
		    (setq minor-mode-map-alist
			  (cons (cons toggle keymap)
				minor-mode-map-alist))))
	      ))
	)

      (if (not (fboundp 'redraw-modeline))
	  (defalias 'redraw-modeline 'force-mode-line-update))

      (if (not (fboundp 'mouse-track))
	  (defalias 'mouse-track 'mouse-drag-region))

      (if (not (fboundp 'windows-of-buffer))
	  (defun windows-of-buffer (&optional buffer)
	    "Returns a list of windows that have BUFFER in them.
If BUFFER is not specified, the current buffer will be used."
	    (get-buffer-window-list buffer)))

      (if (not (boundp 'help-selects-help-window))
	  (defvar help-selects-help-window t
	    "*If nil, use the \"old Emacs\" behavior for Help buffers.
This just displays the buffer in another window, rather than selecting
the window."))

      (if (not (fboundp 'with-displaying-help-buffer))
	  (defun with-displaying-help-buffer (thunk)
	    (let ((winconfig (current-window-configuration))
		  (was-one-window (one-window-p))
		  (help-not-visible
		   (not (and (windows-of-buffer "*Help*") ;shortcut
			     (member (selected-frame)
				     (mapcar 'window-frame
					     (windows-of-buffer "*Help*")))))))
	      (prog1 (with-output-to-temp-buffer "*Help*"
		       (prog1 (funcall thunk)
			 (save-excursion
			   (set-buffer standard-output)
			   (help-mode))))
		(let ((helpwin (get-buffer-window "*Help*")))
		  (if helpwin
		      (progn
			(save-excursion
			  (set-buffer (window-buffer helpwin))
			  ;;If the *Help* buffer is already displayed on this
			  ;; frame, don't override the previous configuration
;			  (if help-not-visible
;			      (set-frame-property
;			       (selected-frame)
;			       'help-window-config winconfig)))
			  )
			(if help-selects-help-window
			    (select-window helpwin))
			(cond ((eq helpwin (selected-window))
			       (message
				(substitute-command-keys
				 "\\[scroll-up] to scroll the help."
				 )))
			      (was-one-window
			       (message
				(substitute-command-keys
				 "\\[scroll-other-window] to scroll the help."
				 )))
			      (t
			       (message
				(substitute-command-keys
				 "\\[scroll-other-window] to scroll the help."
				 )))))))))))

      (if (not (fboundp 'set-extent-mouse-face))
	  (defun set-extent-mouse-face (extent face)
	    "Set the face used to highlight EXTENT when the mouse passes over it.
FACE can also be a list of faces, and all faces listed will apply,
with faces earlier in the list taking priority over those later in the
list.

In the Emacs 19, the argument FACE could not be a list of faces."
	    (put-text-property (overlay-start extent)
			       (overlay-end extent)
			       'mouse-face face)
	    ))


      (if (not (fboundp 'read-directory-name))
	  (defalias 'read-directory-name 'read-file-name))

	
      ))


(provide 'adapt)
