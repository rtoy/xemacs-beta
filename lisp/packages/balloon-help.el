;;; Balloon help for XEmacs (requires 19.12 or later)
;;; Copyright (C) 1995 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@wonderworks.com

;;; Synched up with: Not in FSF.

;; Balloon help pops up a small frame to display help text
;; relating to objects that the mouse cursor passes over.
;; 
;; Installation:
;;
;; Byte-compile the file balloon-help.el (with M-x byte-compile-file)
;; and put the .elc file in a directory in your load-path.  Add the
;; following line to your .emacs:
;;
;; (require 'balloon-help)
;;
;; For 19.12 users:
;;    If you are using fvwm, [tv]twm or ol[v]wm, you can also add
;;    the following lines to various configuration file to use
;;    minimal decorations on the balloon help frames.
;;
;;    In .emacs:
;;       (setq balloon-help-frame-name "balloon-help")
;;
;;    For ol[v]wm use this in .Xdefaults:
;;       olvwm.NoDecor: balloon-help
;;         or
;;       olwm.MinimalDecor: balloon-help
;;
;;    For fvvm use this in your .fvwmrc:
;;       NoTitle balloon-help
;;    or
;;       Style "balloon-help" NoTitle, NoHandles, BorderWidth 0
;;
;;    For twm use this in your .twmrc:
;;       NoTitle { "balloon-help" }
;; 
;; Under 19.13 and later versions the balloon-help frame uses a
;; transient window that is not normally decorated by window
;; managers.  So the window manager directives should not be
;; needed for XEmacs 19.13 and beyond.

(provide 'balloon-help)

(defvar balloon-help-version "1.02"
  "Version string for Balloon Help.")

(defvar balloon-help-mode t
  "*Non-nil means Balloon help mode is enabled.")

(defvar balloon-help-timeout 1500
  "*Display help after this many milliseconds of mouse inactivity.")

(defvar balloon-help-foreground "black"
  "*The foreground color for displaying balloon help text.")

(defvar balloon-help-background "rgb:c0/c0/c0"
  "*The background color for the balloon help frame.")

(defvar balloon-help-background-pixmap ""
  "*The background pixmap for the balloon help frame.")

(defvar balloon-help-font "fixed"
  "*The font for displaying balloon help text.")

(defvar balloon-help-border-color "black"
  "*The color for displaying balloon help frame's border.")

(defvar balloon-help-use-sound nil
  "*Non-nil value means play a sound to herald the appearance
and disappearance of the help frame.

`balloon-help-appears' will be played when the frame appears.
`balloon-help-disappears' will be played when the frame disappears.

See the documentation for the function load-sound-file to see how
define sounds.")

(defvar balloon-help-frame-name nil
  "*The frame name to use for the frame to display the balloon help.")

;;;
;;; End of user variables.
;;;

(defvar mouse-motion-hook mouse-motion-handler
  "Hooks to be run whenever the user moves the mouse.
Each hook is called with one argument, the mouse motion event.")

(defun mouse-motion-hook (event)
  "Run the hooks attached to mouse-motion-hook."
  (run-hook-with-args 'mouse-motion-hook event))

(setq mouse-motion-handler 'mouse-motion-hook)

(defvar balloon-help-frame nil
  "Balloon help is displayed in this frame.")

(defvar balloon-help-help-object nil
  "Object that the mouse is over that has a help property, nil otherwise.")

(defvar balloon-help-help-object-x nil
  "Last horizontal mouse position over balloon-help-help-object.")

(defvar balloon-help-help-object-y nil
  "Last vertical mouse position over balloon-help-help-object.")

(defvar balloon-help-buffer nil
  "Buffer used to display balloon help.")

(defvar balloon-help-timeout-id nil
  "Timeout id for the balloon help timeout.")

(defvar balloon-help-display-pending nil
  "Non-nil value means the help frame will be visible as soon
as the X server gets around to displaying it.  Nil means it
will be invisible as soon as the X server decides to hide it.")

(defvar balloon-help-bar-cursor nil)

(defun balloon-help-mode (&optional arg)
  "Toggle Balloon Help mode.
With arg, turn Balloon Help mode on iff arg is positive.

With Balloon Help enabled, a small frame is displayed whenever
the mouse rests on an object that has a help property of some
kind.  The text of that help property is displayed in the frame.

For extents, the 'balloon-help' property is
checked.

For toolbar buttons, the help-string slot of the toolbar button
is checked.

If the value is a string, it is used as the help message.

If the property's value is a symbol, it is assumed to be the name
of a function and it will be called with one argument, the object
under the mouse, and the return value of that function will be
used as the help message."
  (interactive "P")
  (setq balloon-help-mode (or (and arg (> (prefix-numeric-value arg) 0))
			      (and (null arg) (null balloon-help-mode))))
  (if (null balloon-help-mode)
      (balloon-help-undisplay-help)))

(defun balloon-help-displayed ()
  (and (frame-live-p balloon-help-frame)
       (frame-visible-p balloon-help-frame)))

(defun balloon-help-motion-hook (event)
  (cond
   ((null balloon-help-mode) t)
   ((button-press-event-p event)
    (setq balloon-help-help-object nil)
    (if balloon-help-timeout-id
	(disable-timeout balloon-help-timeout-id))
    (if (balloon-help-displayed)
	(balloon-help-undisplay-help)))
   (t
    (let* ((buffer (event-buffer event))
	   (frame (event-frame event))
	   (point (and buffer (event-point event)))
	   (glyph-extent (event-glyph-extent event))
	   (glyph-extent (if (and glyph-extent
				  (extent-property glyph-extent
						   'balloon-help))
			     glyph-extent))
	   (extent (and point
			(extent-at point buffer 'balloon-help)))
	   (button (event-toolbar-button event))
	   (button (if (and button (toolbar-button-help-string button))
		       button
		     nil))
	   (object (or glyph-extent extent button))
	   (id balloon-help-timeout-id))
      (if (null object)
	  (if (and balloon-help-frame
		   (not (eq frame balloon-help-frame)))
	      (progn
		(setq balloon-help-help-object nil)
		(if id
		    (disable-timeout id))
		(if (balloon-help-displayed)
		    (balloon-help-undisplay-help))))
	(let* ((params (frame-parameters frame))
	       (top (cdr (assq 'top params)))
	       (left (cdr (assq 'left params)))
	       (xtop-toolbar-height
		(if (specifier-instance top-toolbar)
		    (specifier-instance top-toolbar-height)
		  0))
	       (xleft-toolbar-width
		(if (specifier-instance left-toolbar)
		    (specifier-instance left-toolbar-width)
		  0))
	       (menubar-height (if current-menubar 22 0)))
	  (setq balloon-help-help-object-x
		(+ left xleft-toolbar-width (event-x-pixel event))
		balloon-help-help-object-y
		(+ top xtop-toolbar-height menubar-height
		   (event-y-pixel event))))
	(cond ((eq frame balloon-help-frame) t)
	      ((eq object balloon-help-help-object)
	       (if (balloon-help-displayed)
		   (balloon-help-move-help-frame)))
	      ((balloon-help-displayed)
	       (setq balloon-help-help-object object)
	       (balloon-help-display-help))
	      (t
	       (setq balloon-help-help-object object)
	       (if id
		   (disable-timeout id))
	       (setq balloon-help-timeout-id
		     (add-timeout (/ balloon-help-timeout 1000.0)
				  (function balloon-help-display-help)
				  nil)))))))))

(defun balloon-help-pre-command-hook (&rest ignored)
  (setq balloon-help-help-object nil)
  (if (balloon-help-displayed)
      (balloon-help-undisplay-help)))

(fset 'balloon-help-post-command-hook 'balloon-help-pre-command-hook)
(fset 'balloon-help-mouse-leave-frame-hook 'balloon-help-pre-command-hook)
(fset 'balloon-help-deselect-frame-hook 'balloon-help-pre-command-hook)

(defun balloon-help-display-help (&rest ignored)
  (setq balloon-help-timeout-id nil)
  (if balloon-help-help-object
      (let* ((object balloon-help-help-object)
	     (help (or (and (extent-live-p object)
			    (extent-property object 'balloon-help))
		       (and (toolbar-button-p object)
			    (toolbar-button-help-string object))
		       (and (stringp object) object))))
	;; if help is non-null and is not a string, run it as
	;; function to produuce the help string.
	(if (or (null help) (not (symbolp help)))
	    nil
	  (condition-case data
	      (setq help (funcall help object))
	    (error
	     (setq help (format "help function signaled: %S" data)))))
	(if (stringp help)
	    (save-excursion
	      (if (not (bufferp balloon-help-buffer))
		  (setq balloon-help-buffer
			(get-buffer-create " *balloon-help*")))
	      (if (not (frame-live-p balloon-help-frame))
		  (setq balloon-help-frame (balloon-help-make-help-frame)))
	      (setq bar-cursor t)
	      (set-buffer balloon-help-buffer)
	      (erase-buffer)
	      (insert help)
	      (if (not (bolp))
		  (insert ?\n))
	      ;; help strings longer than 2 lines have the last
	      ;; line stolen by the minibuffer, so make sure the
	      ;; last line is blank.  Make the top line blank for
	      ;; some symmetry.
	      (if (< 2 (count-lines (point-min) (point-max)))
		  (progn
		    (insert ?\n)
		    ;; add a second blank line at the end to
		    ;; prevent the modeline bar from clipping the
		    ;; descenders of the last line of text.
		    (insert ?\n)
		    (goto-char (point-min))
		    (insert ?\n)))
	      ;; cursor will be at point-min because we're just
	      ;; moving point which doesn't affect window-point
	      ;; when the window isn't selected.  Indent
	      ;; everything so that the cursor will be over a
	      ;; space.  The 1-pixel bar cursor will be
	      ;; completely invisible this way.
 	      (indent-rigidly (point-min) (point-max) 1)
	      (balloon-help-move-help-frame)
	      (balloon-help-resize-help-frame)
	      (balloon-help-expose-help-frame))))))

(defun balloon-help-undisplay-help ()
  (setq bar-cursor balloon-help-bar-cursor)
  (balloon-help-hide-help-frame))

(defun balloon-help-hide-help-frame ()
  (if (balloon-help-displayed)
      (progn
	(make-frame-invisible balloon-help-frame)
	(if (and balloon-help-use-sound balloon-help-display-pending)
	    (play-sound 'balloon-help-disappears))
	(setq balloon-help-display-pending nil))))

(defun balloon-help-expose-help-frame ()
  (if (not (balloon-help-displayed))
      (progn
	(make-frame-visible balloon-help-frame)
	(if (and balloon-help-use-sound (null balloon-help-display-pending))
	    (play-sound 'balloon-help-appears))
	(setq balloon-help-display-pending t))))

(defun balloon-help-resize-help-frame ()
  (save-excursion
    (set-buffer balloon-help-buffer)
    (let ((longest 0)
	  (lines 0)
	  (done nil)
	  (window-min-height 1)
	  (window-min-width 1))
      (goto-char (point-min))
      (while (not done)
	(end-of-line)
	(setq longest (max longest (current-column))
	      done (not (= 0 (forward-line))))
	(and (not done) (setq lines (1+ lines))))
      (set-frame-size balloon-help-frame (+ 1 longest) lines))))

(defun balloon-help-make-help-frame ()
  (save-excursion
    (setq balloon-help-bar-cursor bar-cursor)
    (set-buffer balloon-help-buffer)
    (set-buffer-menubar nil)
    (let* ((x (balloon-help-compute-help-frame-x-location))
	   (y (balloon-help-compute-help-frame-y-location))
	   (window-min-height 1)
	   (window-min-width 1)
	   (frame (make-frame (list
			       '(initially-unmapped . t)
			       ;; try to evade frame decorations
			       (cons 'name (or balloon-help-frame-name
					       "xclock"))
			       '(border-width . 2)
			       (cons 'border-color balloon-help-border-color)
			       (cons 'top y)
			       (cons 'left x)
			       (cons 'popup (selected-frame))
			       '(width . 3)
			       '(height . 1)))))
      (set-face-font 'default balloon-help-font frame)
      (set-face-foreground 'default balloon-help-foreground frame)
      (set-face-background 'default balloon-help-background frame)
      (set-face-background-pixmap 'default balloon-help-background-pixmap
				  frame)
      (set-window-buffer (frame-selected-window frame) balloon-help-buffer)
      (set-specifier has-modeline-p (cons frame nil))
      (set-specifier top-toolbar-height (cons frame 0))
      (set-specifier left-toolbar-width (cons frame 0))
      (set-specifier right-toolbar-width (cons frame 0))
      (set-specifier bottom-toolbar-height (cons frame 0))
      (set-specifier top-toolbar (cons frame nil))
      (set-specifier left-toolbar (cons frame nil))
      (set-specifier right-toolbar (cons frame nil))
      (set-specifier bottom-toolbar (cons frame nil))
      (set-specifier scrollbar-width (cons frame 0))
      (set-specifier scrollbar-height (cons frame 0))
      (set-specifier modeline-shadow-thickness (cons frame 0))
      (set-face-background 'modeline balloon-help-background frame)
      frame )))

(defun balloon-help-compute-help-frame-x-location ()
  (max 0 (+ 32 balloon-help-help-object-x)))

(defun balloon-help-compute-help-frame-y-location ()
  (max 0 (+ 48 balloon-help-help-object-y)))

(defun balloon-help-move-help-frame ()
  (let ((x (balloon-help-compute-help-frame-x-location))
	(y (balloon-help-compute-help-frame-y-location)))
    (set-frame-position balloon-help-frame x y)))

(add-hook 'mouse-motion-hook 'balloon-help-motion-hook)
(add-hook 'pre-command-hook 'balloon-help-pre-command-hook)
(add-hook 'post-command-hook 'balloon-help-post-command-hook)
(add-hook 'mouse-leave-frame-hook 'balloon-help-mouse-leave-frame-hook)
(add-hook 'deselect-frame-hook 'balloon-help-deselect-frame-hook)
