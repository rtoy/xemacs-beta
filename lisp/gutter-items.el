;;; gutter-items.el --- Gutter content for XEmacs.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: frames, extensions, internal, dumped

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
;; along with Xmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Gutter-specific buffers tab code

(defvar gutter-buffers-tab nil
  "A tab widget in the gutter for displaying buffers.
Do not set this. Use `set-glyph-image' to change the properties of the tab.")

(defcustom gutter-buffers-tab-visible-p
  (gutter-element-visible-p default-gutter-visible-p 'buffers-tab)
  "Whether the buffers tab is globally visible. 
This option should be set through the options menu."
  :group 'buffers-tab
  :type 'boolean
  :set #'(lambda (var val)
	   (set-gutter-element-visible-p default-gutter-visible-p 
					 'buffers-tab val)
	   (setq gutter-buffers-tab-visible-p val)))

(defcustom gutter-buffers-tab-enabled t
  "*Whether to enable support for buffers tab in the gutter.
This is different to `gutter-buffers-tab-visible-p' which still runs hooks
even when the gutter is invisible."
  :group 'buffers-tab
  :type 'boolean)

(defvar gutter-buffers-tab-orientation 'top
  "Where the buffers tab currently is. Do not set this.")

(defun add-tab-to-gutter ()
  "Put a tab control in the gutter area to hold the most recent buffers."
  (setq gutter-buffers-tab-orientation (default-gutter-position))
  (let* ((gutter-string (copy-sequence "\n"))
	 (gutter-buffers-tab-extent (make-extent 0 1 gutter-string)))
    (set-extent-begin-glyph gutter-buffers-tab-extent
			    (setq gutter-buffers-tab 
				  (make-glyph)))
    ;; Nuke all existing tabs
    (remove-gutter-element top-gutter 'buffers-tab)
    (remove-gutter-element bottom-gutter 'buffers-tab)
    (remove-gutter-element left-gutter 'buffers-tab)
    (remove-gutter-element right-gutter 'buffers-tab)
    ;; Put tabs into all devices that will be able to display them
    (mapcar
     #'(lambda (x)
	 (when (valid-image-instantiator-format-p 'tab-control x)
	   (cond ((eq gutter-buffers-tab-orientation 'top)
		  ;; This looks better than a 3d border
		  (set-specifier top-gutter-border-width 0 'global x)
		  (set-gutter-element top-gutter 'buffers-tab 
				      gutter-string 'global x))
		 ((eq gutter-buffers-tab-orientation 'bottom)
		  (set-specifier bottom-gutter-border-width 0 'global x)
		  (set-gutter-element bottom-gutter 'buffers-tab
				      gutter-string 'global x))
		 ((eq gutter-buffers-tab-orientation 'left)
		  (set-specifier left-gutter-border-width 0 'global x)
		  (set-gutter-element left-gutter 'buffers-tab
				      gutter-string 'global x))
		 ((eq gutter-buffers-tab-orientation 'right)
		  (set-specifier right-gutter-border-width 0 'global x)
		  (set-gutter-element right-gutter 'buffers-tab
				      gutter-string 'global x))
		 )))
     (console-type-list))))

(defun update-tab-in-gutter (frame &optional force-selection)
  "Update the tab control in the gutter area."
    ;; dedicated frames don't get tabs
  (unless (or (window-dedicated-p (frame-selected-window frame))
	      (frame-property frame 'popup))
    (when (specifier-instance default-gutter-visible-p frame)
      (unless (and gutter-buffers-tab
		   (eq (default-gutter-position)
		       gutter-buffers-tab-orientation))
	(add-tab-to-gutter))
      (when (valid-image-instantiator-format-p 'tab-control frame)
	(let ((items (buffers-tab-items nil frame force-selection)))
	  (when items
	    (set-glyph-image
	     gutter-buffers-tab
	     (vector 'tab-control :descriptor "Buffers" :face buffers-tab-face
		     :orientation gutter-buffers-tab-orientation
		     (if (or (eq gutter-buffers-tab-orientation 'top)
			     (eq gutter-buffers-tab-orientation 'bottom))
			 :pixel-width :pixel-height)
		     (if (or (eq gutter-buffers-tab-orientation 'top)
			     (eq gutter-buffers-tab-orientation 'bottom))
			 '(gutter-pixel-width) '(gutter-pixel-height)) 
		     :items items)
	     frame)
	    ;; set-glyph-image will not make the gutter dirty
	    (set-gutter-dirty-p gutter-buffers-tab-orientation)))))))

;; A myriad of different update hooks all doing slightly different things
(add-one-shot-hook 
 'after-init-hook
 #'(lambda ()
     ;; don't add the hooks if the user really doesn't want them
     (when gutter-buffers-tab-enabled
       (add-hook 'create-frame-hook 
		 #'(lambda (frame)
		     (when gutter-buffers-tab (update-tab-in-gutter frame t))))
       (add-hook 'buffer-list-changed-hook 'update-tab-in-gutter)
       (add-hook 'default-gutter-position-changed-hook
		 #'(lambda ()
		     (when gutter-buffers-tab
		       (mapc #'update-tab-in-gutter (frame-list)))))
       (add-hook 'gutter-element-visibility-changed-hook
		 #'(lambda (prop visible-p)
		     (when (and (eq prop 'buffers-tab) visible-p)
		       (mapc #'update-tab-in-gutter (frame-list)))))
       (update-tab-in-gutter (selected-frame) t))))

;;
;; progress display
;; ripped off from message display
;;
(defcustom progress-feedback-use-echo-area nil
  "*Whether progress gauge display should display in the echo area.
If NIL then progress gauges will be displayed with whatever native widgets
are available on the current console. If non-NIL then progress display will be
textual and displayed in the echo area."
  :type 'boolean
  :group 'gutter)

(defvar progress-glyph-height 24
  "Height of the progress gauge glyph.")

(defvar progress-feedback-popup-period 0.5
  "The time that the progress gauge should remain up after completion")

(defcustom progress-feedback-style 'large
  "*Control the appearance of the progress gauge.
If 'large, the default, then the progress-feedback text is displayed
above the gauge itself. If 'small then the gauge and text are arranged
side-by-side."
  :group 'gutter
  :type '(choice (const :tag "large" large)
		 (const :tag "small" small)))

;; private variables
(defvar progress-text-instantiator [string :data ""])
(defvar progress-layout-glyph (make-glyph))
(defvar progress-layout-instantiator nil)

(defvar progress-gauge-instantiator
  [progress-gauge
   :value 0
   :pixel-height (eval progress-glyph-height)
   :pixel-width 250
   :descriptor "Progress"])

(defun set-progress-feedback-instantiator (&optional locale)
  (cond
   ((eq progress-feedback-style 'small)
    (setq progress-glyph-height 16)
    (setq progress-layout-instantiator
	  `[layout
	    :orientation horizontal
	    :margin-width 4
	    :items (,progress-gauge-instantiator
		    [button
		     :pixel-height (eval progress-glyph-height)
		     ;; 'quit is special and acts "asynchronously".
		     :descriptor "Stop" :callback 'quit]
		    ,progress-text-instantiator)])
    (set-glyph-image progress-layout-glyph progress-layout-instantiator
		     locale))
   (t 
    (setq progress-glyph-height 24)
    (setq progress-layout-instantiator
	  `[layout 
	    :orientation vertical :margin-width 4
	    :horizontally-justify left :vertically-justify center
	    :items (,progress-text-instantiator
		    [layout 
		     :orientation horizontal
		     :items (,progress-gauge-instantiator
			     [button 
			      :pixel-height (eval progress-glyph-height)
			      :descriptor " Stop "
			      ;; 'quit is special and acts "asynchronously".
			      :callback 'quit])])])
    (set-glyph-image progress-layout-glyph progress-layout-instantiator
		     locale))))

(defvar progress-abort-glyph (make-glyph))

(defun set-progress-abort-instantiator (&optional locale)
  (set-glyph-image progress-abort-glyph
		   `[layout :orientation vertical
			    :horizontally-justify left :vertically-justify center
			    :items (,progress-text-instantiator
				    [layout
				     :margin-width 4
				     :pixel-height progress-glyph-height
				     :orientation horizontal])]
		   locale))

(defvar progress-stack nil
  "An alist of label/string pairs representing active progress gauges.
The first element in the list is currently displayed in the gutter area.
Do not modify this directly--use the `progress-feedback' or
`display-progress-feedback'/`clear-progress-feedback' functions.")

(defun progress-feedback-displayed-p (&optional return-string frame)
  "Return a non-nil value if a progress gauge is presently displayed in the
gutter area.  If optional argument RETURN-STRING is non-nil,
return a string containing the message, otherwise just return t."
  (let ((buffer (get-buffer-create " *Gutter Area*")))
    (and (< (point-min buffer) (point-max buffer))
	 (if return-string
	     (buffer-substring nil nil buffer)
	   t))))

;;; Returns the string which remains in the echo area, or nil if none.
;;; If label is nil, the whole message stack is cleared.
(defun clear-progress-feedback (&optional label frame no-restore)
  "Remove any progress gauge with LABEL from the progress gauge-stack,
erasing it from the gutter area if it's currently displayed there.
If a message remains at the head of the progress-stack and NO-RESTORE
is nil, it will be displayed.  The string which remains in the gutter
area will be returned, or nil if the progress-stack is now empty.
If LABEL is nil, the entire progress-stack is cleared.

Unless you need the return value or you need to specify a label,
you should just use (progress nil)."
  (if (or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	  progress-feedback-use-echo-area)
      (clear-message label frame nil no-restore)
    (or frame (setq frame (selected-frame)))
    (remove-progress-feedback label frame)
    (let ((inhibit-read-only t))
      (erase-buffer (get-buffer-create " *Gutter Area*")))
    (if no-restore
	nil			; just preparing to put another msg up
      (if progress-stack
	  (let ((oldmsg (cdr (car progress-stack))))
	    (raw-append-progress-feedback oldmsg nil frame)
	    oldmsg)
	;; nothing to display so get rid of the gauge
	(set-specifier bottom-gutter-border-width 0 frame)
	(set-gutter-element-visible-p bottom-gutter-visible-p 
				      'progress nil frame)))))

(defun progress-feedback-clear-when-idle (&optional label)
  (add-one-shot-hook 'pre-idle-hook
		     `(lambda ()
			(clear-progress-feedback ',label))))

(defun remove-progress-feedback (&optional label frame)
  ;; If label is nil, we want to remove all matching progress gauges.
  (while (and progress-stack
	      (or (null label)	; null label means clear whole stack
		  (eq label (car (car progress-stack)))))
    (setq progress-stack (cdr progress-stack)))
  (let ((s  progress-stack))
    (while (cdr s)
      (let ((msg (car (cdr s))))
	(if (eq label (car msg))
	    (progn
	      (setcdr s (cdr (cdr s))))
	  (setq s (cdr s)))))))

(defun progress-feedback-dispatch-non-command-events ()
  ;; don't allow errors to hose things
  (condition-case t 
      ;; (sit-for 0) is too agressive and cause more display than we
      ;; want.
      (dispatch-non-command-events)
    nil))

(defun append-progress-feedback (label message &optional value frame)
  (or frame (setq frame (selected-frame)))
  ;; Add a new entry to the message-stack, or modify an existing one
  (let* ((top (car progress-stack))
	 (tmsg (cdr top)))
    (if (eq label (car top))
	(progn
	  (setcdr top message)
	  (if (equal tmsg message)
	      (progn 
		(set-instantiator-property progress-gauge-instantiator :value value)
		(set-progress-feedback-instantiator (frame-selected-window frame)))
	    (raw-append-progress-feedback message value frame))
	  (redisplay-gutter-area))
      (push (cons label message) progress-stack)
      (raw-append-progress-feedback message value frame))
    (progress-feedback-dispatch-non-command-events)
    ;; either get command events or sit waiting for them
    (when (eq value 100)
;      (sit-for progress-feedback-popup-period nil)
      (clear-progress-feedback label))))

(defun abort-progress-feedback (label message &optional frame)
  (if (or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	  progress-feedback-use-echo-area)
      (display-message label (concat message "aborted.") frame)
    (or frame (setq frame (selected-frame)))
    ;; Add a new entry to the message-stack, or modify an existing one
    (let* ((top (car progress-stack))
	   (inhibit-read-only t))
      (if (eq label (car top))
	  (setcdr top message)
	(push (cons label message) progress-stack))
      (unless (equal message "")
	(insert-string message (get-buffer-create " *Gutter Area*"))
	(let* ((gutter-string (copy-sequence "\n"))
	       (ext (make-extent 0 1 gutter-string)))
	  ;; do some funky display here.
	  (set-extent-begin-glyph ext progress-abort-glyph)
	  ;; fixup the gutter specifiers
	  (set-gutter-element bottom-gutter 'progress gutter-string frame)
	  (set-specifier bottom-gutter-border-width 2 frame)
	  (set-instantiator-property progress-text-instantiator :data message)
	  (set-progress-abort-instantiator (frame-selected-window frame))
	  (set-specifier bottom-gutter-height 'autodetect frame)
	  (set-gutter-element-visible-p bottom-gutter-visible-p 
					'progress t frame)
	  ;; we have to do this so redisplay is up-to-date and so
	  ;; redisplay-gutter-area performs optimally.
	  (redisplay-gutter-area)
	  (sit-for progress-feedback-popup-period nil)
	  (clear-progress-feedback label frame)
	  (set-extent-begin-glyph ext progress-layout-glyph)
	  (set-gutter-element bottom-gutter 'progress gutter-string frame)
	  )))))

(defun raw-append-progress-feedback (message &optional value frame)
  (unless (equal message "")
    (let* ((inhibit-read-only t)
	  (val (or value 0))
	  (gutter-string (copy-sequence "\n"))
	  (ext (make-extent 0 1 gutter-string)))
      (insert-string message (get-buffer-create " *Gutter Area*"))
      ;; do some funky display here.
      (set-extent-begin-glyph ext progress-layout-glyph)
      ;; fixup the gutter specifiers
      (set-gutter-element bottom-gutter 'progress gutter-string frame)
      (set-specifier bottom-gutter-border-width 2 frame)
      (set-instantiator-property progress-gauge-instantiator :value val)
      (set-progress-feedback-instantiator (frame-selected-window frame))

      (set-instantiator-property progress-text-instantiator :data message)
      (set-progress-feedback-instantiator (frame-selected-window frame))
      (if (and (eq (specifier-instance bottom-gutter-height frame)
		   'autodetect)
	       (gutter-element-visible-p bottom-gutter-visible-p
					 'progress frame))
	  ;; if the gauge is already visible then just draw the gutter
	  ;; checking for user events
	  (progn
	    (redisplay-gutter-area)
	    (progress-feedback-dispatch-non-command-events))
	;; otherwise make the gutter visible and redraw the frame
	(set-specifier bottom-gutter-height 'autodetect frame)
	(set-gutter-element-visible-p bottom-gutter-visible-p
				      'progress t frame)
	;; we have to do this so redisplay is up-to-date and so
	;; redisplay-gutter-area performs optimally. This may also
	;; make sure the frame geometry looks ok.
	(progress-feedback-dispatch-non-command-events)
	(redisplay-frame frame)
	))))

(defun display-progress-feedback (label message &optional value frame)
  "Display a progress gauge and message in the bottom gutter area.
 First argument LABEL is an identifier for this message.  MESSAGE is
the string to display.  Use `clear-progress-feedback' to remove a labelled
message."
  (cond ((eq value 'abort)
	 (abort-progress-feedback label message frame))
	((or (not (valid-image-instantiator-format-p 'progress-gauge frame))
	     progress-feedback-use-echo-area)
	 (display-message label 
	   (concat message (if (eq value 100) "done."
			     (make-string (/ value 5) ?.)))
	   frame))
	(t
	 (append-progress-feedback label message value frame))))

(defun current-progress-feedback (&optional frame)
  "Return the current progress gauge in the gutter area, or nil.
The FRAME argument is currently unused."
  (cdr (car progress-stack)))

;;; may eventually be frame-dependent
(defun current-progress-feedback-label (&optional frame)
  (car (car progress-stack)))

(defun progress-feedback (fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing progress gauge."
  (save-excursion
    (if (and (null fmt) (null args))
	(prog1 nil
	  (clear-progress-feedback nil))
      (let ((str (apply 'format fmt args)))
	(display-progress-feedback 'progress str value)
	str))))

(defun progress-feedback-with-label (label fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
First argument LABEL is an identifier for this progress gauge.  The rest of the
arguments are the same as to `format'."
  ;; #### sometimes the buffer gets changed temporarily. I don't know
  ;; why this is, so protect against it.
  (save-excursion
    (if (and (null fmt) (null args))
	(prog1 nil
	  (clear-progress-feedback label nil))
      (let ((str (apply 'format fmt args)))
	(display-progress-feedback label str value)
	str))))

(provide 'gutter-items)
;;; gutter-items.el ends here.
