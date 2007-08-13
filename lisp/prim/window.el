;;; window.el --- XEmacs window commands aside from those written in C.
;; Keywords: extensions

;; Copyright (C) 1985, 1989, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;;; Synched up with: FSF 19.30.

;;;; Window tree functions.

(defun one-window-p (&optional nomini all-frames device)
  "Returns non-nil if the selected window is the only window (in its frame).
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.

The optional arg ALL-FRAMES t means count windows on all frames.
If it is `visible', count windows on all visible frames.
ALL-FRAMES nil or omitted means count only the selected frame, 
plus the minibuffer it uses (which may be on another frame).
ALL-FRAMES = 0 means count windows on all visible and iconified frames.
If ALL-FRAMES is any other value, count only the selected frame.

If optional third argument DEVICE is nil or omitted, count frames
on all devices.
If a device, count frames only on that device.
If a device type, count frames only on devices of that type.
Otherwise, count frames only on the selected device."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) all-frames device))))

(defun walk-windows (proc &optional minibuf all-frames device)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, when a separate minibuffer frame is active,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.  But if the
minibuffer does not count, only windows from WINDOW's frame count.

ALL-FRAMES is the optional third argument.
ALL-FRAMES nil or omitted means cycle within the frames as specified above.
ALL-FRAMES = `visible' means include windows on all visible frames.
ALL-FRAMES = 0 means include windows on all visible and iconified frames.
ALL-FRAMES = t means include windows on all frames including invisible frames.
Anything else means restrict to WINDOW's frame.

If optional fourth argument DEVICE is nil or omitted, include frames
on all devices.
If a device, include frames only on that device.
If a device type, include frames only on devices of that type.
Otherwise, include frames only on the selected device."
  ;; If we start from the minibuffer window, don't fail to come back to it.
  (if (window-minibuffer-p (selected-window))
      (setq minibuf t))
  ;; Note that, like next-window & previous-window, this behaves a little 
  ;; strangely if the selected window is on an invisible frame: it hits
  ;; some of the windows on that frame, and all windows on visible frames.
  (let* ((walk-windows-start (selected-window))
	 (walk-windows-current walk-windows-start))
    (while (progn
	     (setq walk-windows-current
		   (next-window walk-windows-current minibuf all-frames
				device))
	     (funcall proc walk-windows-current)
	     (not (eq walk-windows-current walk-windows-start))))))
;; The old XEmacs definition of the above clause.  It's more correct in
;; that it will never hit a window that's already been hit even if you
;; do something odd like `delete-other-windows', but has the problem
;; that it conses. (This may be called repeatedly, from lazy-lock
;; for example.)
;  (let* ((walk-windows-history nil)
;	 (walk-windows-current (selected-window)))
;    (while (progn
;	     (setq walk-windows-current
;		   (next-window walk-windows-current minibuf all-frames
;				device))
;	     (not (memq walk-windows-current walk-windows-history)))
;      (setq walk-windows-history (cons walk-windows-current
;				       walk-windows-history))
;      (funcall proc walk-windows-current))))

(defun minibuffer-window-active-p (window)
  "Return t if WINDOW (a minibuffer window) is now active."
  (eq window (active-minibuffer-window)))

(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY."
  (list 'let
	'((save-selected-window-window (selected-window)))
	(list 'unwind-protect
	      (cons 'progn body)
	      (list 'and
		    (list 'window-live-p 'save-selected-window-window)
		    (list 'select-window 'save-selected-window-window)))))

(defun count-windows (&optional minibuf)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda (w)
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun balance-windows ()
  "Makes all visible windows the same height (approximately)."
  (interactive)
  (let ((count -1) levels newsizes size)
        ;FSFmacs
	;;; Don't count the lines that are above the uppermost windows.
	;;; (These are the menu bar lines, if any.)
	;(mbl (nth 1 (window-edges (frame-first-window (selected-frame))))))
    ;; Find all the different vpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (save-window-excursion
      (let (tops (prev-top -2))
	(walk-windows (function (lambda (w)
                        (setq tops (cons (nth 1 (window-pixel-edges w))
                                         tops))))
		      'nomini)
	(setq tops (sort tops '<))
	(while tops
	  (if (> (car tops) (1+ prev-top))
	      (setq prev-top (car tops)
		    count (1+ count)))
	  (setq levels (cons (cons (car tops) count) levels))
	  (setq tops (cdr tops)))
	(setq count (1+ count))))
    ;; Subdivide the frame into that many vertical levels.
    ;FSFmacs (setq size (/ (- (frame-height) mbl) count))
    (setq size (/ (window-pixel-height (frame-root-window)) count))
    (walk-windows (function (lambda (w)
                    (select-window w)
                    (let ((newtop (cdr (assq (nth 1 (window-pixel-edges))
                                             levels)))
                          (newbot (or (cdr (assq
					    (+ (window-pixel-height)
					       (nth 1 (window-pixel-edges)))
					    levels))
                                      count)))
                      (setq newsizes
                            (cons (cons w (* size (- newbot newtop)))
                                  newsizes)))))
		  'nomini)
    (walk-windows (function (lambda (w)
			      (select-window w)
			      (let ((newsize (cdr (assq w newsizes))))
				(enlarge-window
				 (/ (- newsize (window-pixel-height))
				    (face-height 'default))))))
                  'nomini)))

;;; I think this should be the default; I think people will prefer it--rms.
(defvar split-window-keep-point t
  "*If non-nil, split windows keeps the original point in both children.
This is often more convenient for editing.
If nil, adjust point in each of the two windows to minimize redisplay.
This is convenient on slow terminals, but point can move strangely.")

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
The uppermost window gets ARG lines and the other gets the rest.
Negative arg means select the size of the lowermost window instead.
With no argument, split equally or close to it.
Both windows display the same buffer now current.

If the variable split-window-keep-point is non-nil, both new windows
will get the same value of point as the current window.  This is often
more convenient for editing.

Otherwise, we chose window starts so as to minimize the amount of
redisplay; this is convenient on slow terminals.  The new selected
window is the one that the current value of point appears in.  The
value of point can change if the text around point is hidden by the
new modeline.

Programs should probably use split-window instead of this."
  (interactive "P")
  (let ((old-w (selected-window))
	(old-point (point))
	(size (and arg (prefix-numeric-value arg)))
	new-w bottom)
    (and size (< size 0) (setq size (+ (window-height) size)))
    (setq new-w (split-window nil size))
    (or split-window-keep-point
	(progn
	  (save-excursion
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
	    (vertical-motion (window-height))
	    (set-window-start new-w (point))
	    (if (> (point) (window-point new-w))
		(set-window-point new-w (point)))
	    (vertical-motion -1)
	    (setq bottom (point)))
	  (if (<= bottom (point))
	      (set-window-point old-w (1- bottom)))
	  (if (< (window-start new-w) old-point)
	      (progn
		(set-window-point new-w old-point)
		(select-window new-w)))))
    new-w))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets ARG columns.
Negative arg means select the size of the rightmost window instead.
No arg means split equally."
  (interactive "P")
  (let ((size (and arg (prefix-numeric-value arg))))
    (and size (< size 0)
	 (setq size (+ (window-width) size)))
    (split-window nil size t)))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink the WINDOW to be as small as possible to display its contents.
Do not shrink to less than `window-min-height' lines.
Do nothing if the buffer contains more lines than the present window height,
or if some of the window's contents are scrolled out of view,
or if the window is not the full width of the frame,
or if the window is the only window of its frame."
  (interactive)
  (or window (setq window (selected-window)))
  (save-excursion
    (set-buffer (window-buffer window))
    (let* ((w (selected-window))	;save-window-excursion can't win
	   (buffer-file-name buffer-file-name)
	   (p (point))
	   (n 0)
	   (ignore-final-newline
	    ;; If buffer ends with a newline, ignore it when counting height
	    ;; unless point is after it.
	    (and (not (eobp))
		 (eq ?\n (char-after (1- (point-max))))))
	   (buffer-read-only nil)
	   (modified (buffer-modified-p))
	   (buffer (current-buffer))
	   (mini (frame-property (window-frame window) 'minibuffer))
	   (edges (window-pixel-edges (selected-window))))
      (if (and (< 1 (let ((frame (selected-frame)))
		      (select-frame (window-frame window))
		      (unwind-protect
			  (count-windows)
			(select-frame frame))))
	       ;; check to make sure that we don't have horizontally
	       ;; split windows
	       (eq (frame-highest-window (window-frame window) 0)
		   (frame-highest-window (window-frame window) -1))
	       (pos-visible-in-window-p (point-min) window)
	       (not (eq mini 'only))
	       (or (not mini) (eq mini t)
		   (< (nth 3 edges)
		      (nth 1 (window-pixel-edges mini)))
		   (> (nth 1 edges)
		      ;FSFmacs (frame-property (window-frame window)
		      ;			       'menu-bar-lines params)
		      0)))
	  (unwind-protect
	      (let ((shrinkee (or window w)))
		(set-buffer (window-buffer shrinkee))
		(goto-char (point-min))
		(while (pos-visible-in-window-p
			(- (point-max)
			   (if ignore-final-newline 1 0))
			shrinkee)
		  ;; defeat file locking... don't try this at home, kids!
		  (setq buffer-file-name nil)
		  (insert ?\n) (setq n (1+ n)))
		(if (> n 0)
		    (shrink-window (min (1- n)
					(- (window-height shrinkee)
					   window-min-height))
				   nil
				   shrinkee)))
	    (delete-region (point-min) (point))
	    (set-buffer-modified-p modified)
	    (goto-char p)
	    ;; Make sure we unbind buffer-read-only
	    ;; with the proper current buffer.
	    (set-buffer buffer))))))

(defun backward-other-window (arg &optional all-frames device)
  "Select the ARG'th different window on this frame, going backwards.
This is just like calling `other-window' with the arg negated."
  (interactive "p")
  (other-window (- arg) all-frames device))

(defun windows-of-buffer (&optional buffer)
  "Returns a list of windows that have BUFFER in them.
If BUFFER is not specified, the current buffer will be used."
  (or (bufferp buffer)
      (if (stringp buffer)
	  (setq buffer (or (get-buffer buffer)
			   (get-file-buffer buffer)))
	(setq buffer (current-buffer))))
  (let* ((firstwin (next-window nil nil t))
	 (wind firstwin) 
	 (done nil)
	 window-list)
    (while (not done)
      (if (eq (window-buffer wind) buffer)
	  (setq window-list (append window-list (list wind))))
      (setq wind (next-window wind nil t))
      (setq done (eq wind firstwin)))
    window-list))

(defun buffer-in-multiple-windows-p (&optional buffer)
  "Returns t if BUFFER is in multiple windows.
If BUFFER is not specified, the current buffer will be used."
  (setq buffer (or buffer
		   (get-buffer buffer)
		   (get-file-buffer buffer)
		   (current-buffer)))
  (> (length (windows-of-buffer buffer)) 1))

(defun window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, beginning with WINDOW.
FRAME and WINDOW default to the selected ones.  
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active."
  (setq window (or window (selected-window))
	frame (or frame (selected-frame)))
  (if (not (eq (window-frame window) frame))
      (error "Window must be on frame."))
  (let ((current-frame (selected-frame))
	list)
    (unwind-protect
	(save-window-excursion
	  (select-frame frame)
	  (walk-windows
	   (function (lambda (cur-window)
		       (if (not (eq window cur-window))
			   (setq list (cons cur-window list)))))
	   minibuf)
	  (setq list (cons window list)))
      (select-frame current-frame))))

;; We used to have set-window-dedicated-p as an obsolete version
;; of set-window-buffer-dedicated, but it really makes more sense
;; this way.

(make-obsolete 'set-window-buffer-dedicated 'set-window-dedicated-p)
(defun set-window-buffer-dedicated (window buffer)
  "Make WINDOW display BUFFER and be dedicated to that buffer.
Then Emacs will not automatically change which buffer appears in WINDOW.
If BUFFER is nil, make WINDOW not be dedicated (but don't change which
buffer appears in it currently)."
  (if (bufferp buffer)
      (set-window-buffer window (get-buffer-create buffer)))
  (set-window-dedicated-p window (not (null buffer))))


;; The window-config stack is stored as a list in frame property
;; 'window-config-stack, with the most recent element at the front.
;; When you pop off an element, the popped off element gets put at the
;; front of frame property 'window-config-unpop-stack, so you can
;; retrieve it using unpop-window-configuration.

(defvar window-config-stack-max 16
  "*Maximum size of window configuration stack.
Start discarding off end if it gets this big.")

(defun window-config-stack (&optional frame)
  (or frame (setq frame (selected-frame)))
  (let ((stack (frame-property frame 'window-config-stack)))
    (if stack
	(set-undoable-stack-max stack window-config-stack-max)
      (progn
	(setq stack (make-undoable-stack window-config-stack-max))
	(set-frame-property frame 'window-config-stack stack)))
    stack))

(defun push-window-configuration (&optional config)
  "Push the current window configuration onto the window-config stack.
If CONFIG is specified, push it instead of the current window configuration.
Each frame has its own window-config stack."
  (interactive)
  (let ((wc (or config (current-window-configuration)))
	(stack (window-config-stack)))
    (if (or (= 0 (undoable-stack-a-length stack))
	    (not (equal (undoable-stack-a-top stack) wc)))
	(progn
	  (undoable-stack-push stack wc)
	  ;; kludge.
	  (if (featurep 'toolbar)
	      (set-specifier-dirty-flag default-toolbar))))))

(defun pop-window-configuration ()
  "Pop the top window configuration off the window-config stack and set it.
Before setting the new window configuration, the current window configuration
 is pushed onto the \"unpop\" stack.
`unpop-window-configuration' undoes what this function does.
Each frame has its own window-config and \"unpop\" stack."
  (interactive)
  (let ((stack (window-config-stack))
	(wc (current-window-configuration))
	popped)
    (condition-case nil
	(progn
	  (setq popped (undoable-stack-pop stack))
	  (while (equal popped wc)
	    (setq popped (undoable-stack-pop stack)))
	  (undoable-stack-push stack wc)
	  (undoable-stack-undo stack)
	  (set-window-configuration popped)
	  ;; probably not necessary:
	  (if (featurep 'toolbar)
	      (set-specifier-dirty-flag default-toolbar))
	  popped)
      (trunc-stack-bottom
       (error "Bottom of window config stack")))))

(defun unpop-window-configuration ()
  "Undo the effect of the most recent `pop-window-configuration'.
This does exactly the inverse of what `pop-window-configuration' does:
 i.e. it pops a window configuration off of the \"unpop\" stack and
 pushes the current window configuration onto the window-config stack.
Each frame has its own window-config and \"unpop\" stack."
  (interactive)
  (let ((stack (window-config-stack))
	(wc (current-window-configuration))
	popped)
    (condition-case nil
	(progn
	  (setq popped
		(progn
		  (undoable-stack-redo stack)
		  (undoable-stack-pop stack)))
	  (while (equal popped wc)
	    (setq popped
		  (progn
		    (undoable-stack-redo stack)
		    (undoable-stack-pop stack))))
	  (undoable-stack-push stack wc)
	  (set-window-configuration popped)
	  ;; probably not necessary:
	  (if (featurep 'toolbar)
	      (set-specifier-dirty-flag default-toolbar))
	  popped)
      (trunc-stack-bottom
       (error "Top of window config stack")))))


;;;;;;;;;;;;; display-buffer, moved here from C.  Hallelujah.

(defvar display-buffer-function nil
  "If non-nil, function to call to handle `display-buffer'.
It will receive three args: the same as those to `display-buffer'.")

(defvar pre-display-buffer-function nil
  "If non-nil, function that will be called from `display-buffer'
as the first action.  It will receive three args: the same as those
to `display-buffer'.
This function may be used to select an appropriate frame for the buffer,
for example.  See also the variable `display-buffer-function', which may
be used to completely replace the `display-buffer' function.
If the return value of this function is non-nil, it should be a frame,
and that frame will be used to display the buffer.")

(defvar pop-up-frames nil
  "*Non-nil means `display-buffer' should make a separate frame.")

(defvar pop-up-frame-function nil
  "Function to call to handle automatic new frame creation.
It is called with no arguments and should return a newly created frame.

A typical value might be `(lambda () (new-frame pop-up-frame-alist))'
where `pop-up-frame-alist' would hold the default frame parameters.")

(defvar special-display-buffer-names nil
  "*List of buffer names that should have their own special frames.
Displaying a buffer whose name is in this list makes a special frame for it
using `special-display-function'.

An element of the list can be a cons cell instead of just a string.
Then the car should be a buffer name, and the cdr specifies frame
parameters for creating the frame for that buffer.
More precisely, the cdr is passed as the second argument to
the function found in `special-display-function', when making that frame.
See also `special-display-regexps'.")

(defvar special-display-regexps nil
  "*List of regexps saying which buffers should have their own special frames.
If a buffer name matches one of these regexps, it gets its own frame.
Displaying a buffer whose name is in this list makes a special frame for it
using `special-display-function'.

An element of the list can be a cons cell instead of just a string.
Then the car should be the regexp, and the cdr specifies frame
parameters for creating the frame for buffers that match.
More precisely, the cdr is passed as the second argument to
the function found in `special-display-function', when making that frame.
See also `special-display-buffer-names'.")

(defvar special-display-function nil
  "Function to call to make a new frame for a special buffer.
It is called with two arguments, the buffer and optional buffer specific
data, and should return a window displaying that buffer.
The default value makes a separate frame for the buffer,
using `special-display-frame-alist' to specify the frame parameters.

A buffer is special if its is listed in `special-display-buffer-names'
or matches a regexp in `special-display-regexps'.")

(defvar same-window-buffer-names nil
  "*List of buffer names that should appear in the selected window.
Displaying one of these buffers using `display-buffer' or `pop-to-buffer'
switches to it in the selected window, rather than making it appear
in some other window.

An element of the list can be a cons cell instead of just a string.
Then the car must be a string, which specifies the buffer name.
This is for compatibility with `special-display-buffer-names';
the cdr of the cons cell is ignored.

See also `same-window-regexps'.")

(defvar same-window-regexps nil
  "*List of regexps saying which buffers should appear in the selected window.
If a buffer name matches one of these regexps, then displaying it
using `display-buffer' or `pop-to-buffer' switches to it
in the selected window, rather than making it appear in some other window.

An element of the list can be a cons cell instead of just a string.
Then the car must be a string, which specifies the buffer name.
This is for compatibility with `special-display-buffer-names';
the cdr of the cons cell is ignored.

See also `same-window-buffer-names'.")

(defvar pop-up-windows t
  "*Non-nil means display-buffer should make new windows.")

(defvar split-height-threshold 500
 "*display-buffer would prefer to split the largest window if this large.
If there is only one window, it is split regardless of this value.")

(defvar split-width-threshold 500
  "*display-buffer would prefer to split the largest window if this large.
If there is only one window, it is split regardless of this value.")

;; Deiconify the frame containing the window WINDOW, then return WINDOW.

(defun display-buffer-1 (window)
  (if (frame-iconified-p (window-frame window))
      (make-frame-visible (window-frame window)))
  window)

;; Can you believe that all of this crap was formerly in C?
;; Praise Jesus that it's not there any more.

(defun display-buffer (buffer &optional not-this-window-p override-frame)
  "Make BUFFER appear in some window on the current frame, but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window in the current frame,
just uses that one, unless the window is the selected window and
NOT-THIS-WINDOW-P is non-nil (interactively, with prefix arg).

If BUFFER has a dedicated frame, display on that frame instead of
the current frame, unless OVERRIDE-FRAME is non-nil.

If OVERRIDE-FRAME is non-nil, display on that frame instead of
the current frame (or the dedicated frame).

If `pop-up-windows' is non-nil, always use the
current frame and create a new window regardless of whether the
buffer has a dedicated frame, and regardless of whether
OVERRIDE-FRAME was specified.

If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.

Returns the window displaying BUFFER."
  (interactive "BDisplay buffer:\nP")

  (let ((wconfig (current-window-configuration))
	(result
	 ;; We just simulate a `return' in C.  This function is way ugly
	 ;; and does `returns' all over the place and there's no sense
	 ;; in trying to rewrite it to be more Lispy.
	 (catch 'done
	   (let (window old-frame target-frame explicit-frame)
	     (setq old-frame (or (last-nonminibuf-frame) (selected-frame)))
	     (setq buffer (get-buffer buffer))
	     (check-argument-type 'bufferp buffer)

	     (setq explicit-frame
		   (if pre-display-buffer-function
		       (funcall pre-display-buffer-function buffer
				not-this-window-p
				override-frame)))

	     ;; Give the user the ability to completely reimplement
	     ;; this function via the `display-buffer-function'.
	     (if display-buffer-function
		 (throw 'done
			(funcall display-buffer-function buffer
				 not-this-window-p
				 override-frame)))

	     ;; If the buffer has a dedicated frame, that takes
	     ;; precedence over the current frame, and over what the
	     ;; pre-display-buffer-function did.
	     (let ((dedi (buffer-dedicated-frame buffer)))
	       (if (frame-live-p dedi) (setq explicit-frame dedi)))

	     ;; if override-frame is supplied, that takes precedence over
	     ;; everything.  This is gonna look bad if the
	     ;; pre-display-buffer-function raised some other frame
	     ;; already.
	     (if override-frame
		 (progn
		   (check-argument-type 'frame-live-p override-frame)
		   (setq explicit-frame override-frame)))

	     (setq target-frame
		   (or explicit-frame
		       (last-nonminibuf-frame)
		       (selected-frame)))

	     ;; If we have switched frames, then set not-this-window-p
	     ;; to false.  Switching frames means that selected-window
	     ;; is no longer the same as it was on entry -- it's the
	     ;; selected-window of target_frame instead of old_frame,
	     ;; so it's a fine candidate for display.
	     (if (not (eq old-frame target-frame))
		 (setq not-this-window-p nil))
	
	     ;; if it's in the selected window, and that's ok, then we're done.
	     (if (and (not not-this-window-p)
		      (eq buffer (window-buffer (selected-window))))
		 (throw 'done (display-buffer-1 (selected-window))))

	     ;; See if the user has specified this buffer should appear
	     ;; in the selected window.
	
	     (if not-this-window-p
		 nil
	  
	       (if (or (member (buffer-name buffer) same-window-buffer-names)
		       (assoc (buffer-name buffer) same-window-buffer-names))
		   (progn
		     (switch-to-buffer buffer)
		     (throw 'done (display-buffer-1 (selected-window)))))
	  
	       (let ((tem same-window-regexps))
		 (while tem
		   (let ((car (car tem)))
		     (if (or
			  (and (stringp car)
			       (string-match car (buffer-name buffer)))
			  (and (consp car) (stringp (car car))
			       (string-match (car car) (buffer-name buffer))))
			 (progn
			   (switch-to-buffer buffer)
			   (throw 'done (display-buffer-1
					 (selected-window))))))
		   (setq tem (cdr tem)))))
	
	     ;; If pop-up-frames, look for a window showing BUFFER on
	     ;; any visible or iconified frame.  Otherwise search only
	     ;; the current frame.
	     (if (and (not explicit-frame)
		      (or pop-up-frames (not (last-nonminibuf-frame))))
		 (setq target-frame 0))
	
	     ;; Otherwise, find some window that it's already in, and
	     ;; return that, unless that window is the selected window
	     ;; and that isn't ok.  What a contorted mess!
	     (setq window (get-buffer-window buffer target-frame))
	     (if (and window
		      (or (not not-this-window-p)
			  (not (eq window (selected-window)))))
		 (throw 'done (display-buffer-1 window)))

	     ;; Certain buffer names get special handling.
	     (if special-display-function
		 (progn
		   (if (member (buffer-name buffer)
			       special-display-buffer-names)
		       (throw 'done (funcall special-display-function buffer)))

		   (let ((tem (assoc (buffer-name buffer)
				     special-display-buffer-names)))
		     (if tem
			 (throw 'done (funcall special-display-function
					       buffer (cdr tem)))))

		   (let ((tem special-display-regexps))
		     (while tem
		       (let ((car (car tem)))
			 (if (and (stringp car)
				  (string-match car (buffer-name buffer)))
			     (throw 'done
				    (funcall special-display-function buffer)))
			 (if (and (consp car)
				  (stringp (car car))
				  (string-match (car car)
						(buffer-name buffer)))
			     (throw 'done (funcall
					   special-display-function buffer
					   (cdr car)))))
		       (setq tem (cdr tem))))))

	     ;; If there are no frames open that have more than a minibuffer,
	     ;; we need to create a new frame.
	     (if (or pop-up-frames
		     (null (last-nonminibuf-frame)))
		 (progn
		   (setq window (frame-selected-window
				 (funcall pop-up-frame-function)))
		   (set-window-buffer window buffer)
		   (throw 'done (display-buffer-1 window))))

	     ;; Otherwise, make it be in some window, splitting if
	     ;; appropriate/possible.  Do not split a window if we are
	     ;; displaying the buffer in a different frame than that which
	     ;; was current when we were called.  (It is already in a
	     ;; different window by virtue of being in another frame.)
	     (if (or (and pop-up-windows (eq target-frame old-frame))
		     (eq 'only (frame-property (selected-frame) 'minibuffer))
		     ;; If the current frame is a special display frame,
		     ;; don't try to reuse its windows.
		     (window-dedicated-p (frame-root-window (selected-frame))))
		 (progn
		   (if (eq 'only (frame-property (selected-frame) 'minibuffer))
		       (setq target-frame (last-nonminibuf-frame)))

		   ;; Don't try to create a window if would get an error with
		   ;; height.
		   (if (< split-height-threshold (* 2 window-min-height))
		       (setq split-height-threshold (* 2 window-min-height)))

		   ;; Same with width.
		   (if (< split-width-threshold (* 2 window-min-width))
		       (setq split-width-threshold (* 2 window-min-width)))

		   ;; If the frame we would try to split cannot be split,
		   ;; try other frames.
		   (if (frame-property (if (null target-frame)
					   (selected-frame)
					 (last-nonminibuf-frame))
				       'unsplittable)
		       (setq window
			     ;; Try visible frames first.
			     (or (get-largest-window 'visible)
				 ;; If that didn't work, try iconified frames.
				 (get-largest-window 0)
				 (get-largest-window t)))
		     (setq window (get-largest-window target-frame)))

		   ;; If we got a tall enough full-width window that
		   ;; can be split, split it.
		   (if (and window
			    (not (frame-property (window-frame window)
						 'unsplittable))
			    (>= (window-height window) split-height-threshold)
			    (or (>= (window-width window)
				    split-width-threshold)
				(and (window-leftmost-p window)
				     (window-rightmost-p window))))
		       (setq window (split-window window))
		     (let (upper lower other)
		       (setq window (get-lru-window target-frame))
		       ;; If the LRU window is selected, and big enough,
		       ;; and can be split, split it.
		       (if (and window
				(not (frame-property (window-frame window) 
						     'unsplittable))
				(or (eq window (selected-window))
				    (not (window-parent window)))
				(>= (window-height window)
				    (* 2 window-min-height)))
			   (setq window (split-window window)))
		       ;; If get-lru-window returned nil, try other approaches.
		       ;; Try visible frames first.
		       (or window
			   (setq window (or (get-largest-window 'visible)
					    ;; If that didn't work, try
					    ;; iconified frames.
					    (get-largest-window 0)
					    ;; Try invisible frames.
					    (get-largest-window t)
					    ;; As a last resort, make
					    ;; a new frame.
					    (frame-selected-window
					     (funcall
					      pop-up-frame-function)))))
		       ;; If window appears above or below another,
		       ;; even out their heights.
		       (if (window-previous-child window)
			   (setq other (window-previous-child window)
				 upper other
				 lower window))
		       (if (window-next-child window)
			   (setq other (window-next-child window)
				 lower other
				 upper window))
		       ;; Check that OTHER and WINDOW are vertically arrayed.
		       (if (and other
				(not (= (nth 1 (window-pixel-edges other))
					(nth 1 (window-pixel-edges window))))
				(> (window-pixel-height other)
				   (window-pixel-height window)))
			   (enlarge-window (- (/ (+ (window-height other)
						    (window-height window))
						 2)
					      (window-height upper))
					   nil upper)))))

	       (setq window (get-lru-window target-frame)))

	     ;; Bring the window's previous buffer to the top of the MRU chain.
	     (if (window-buffer window)
		 (save-excursion
		   (save-selected-window
		     (select-window window)
		     (record-buffer (window-buffer window)))))

	     (set-window-buffer window buffer)

	     (display-buffer-1 window)))))
    (or (equal wconfig (current-window-configuration))
	(push-window-configuration wconfig))
    result))
