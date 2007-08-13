;;; mouse.el --- window system-independent mouse support.
;; Keywords: hardware

;; Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems
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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not synched with FSF.  Almost completely divergent.

(provide 'mouse)

(global-set-key 'button1 'mouse-track)
(global-set-key '(shift button1) 'mouse-track-adjust)
(global-set-key '(control button1) 'mouse-track-insert)
(global-set-key '(control shift button1) 'mouse-track-delete-and-insert)
(global-set-key '(meta button1) 'mouse-track-do-rectangle)

(global-set-key 'button2 'mouse-yank)

(defvar mouse-track-rectangle-p nil
  "*If true, then dragging out a region with the mouse selects rectangles
instead of simple start/end regions.")

(defvar mouse-yank-at-point nil
  "*If non-nil, the function `mouse-yank' will yank text at the cursor location.
Otherwise, the cursor will be moved to the location of the pointer click before
text is inserted.")

(defvar mouse-yank-function 'yank	; x11/x-mouse changes this...
  "Function that is called upon by `mouse-yank' to actually insert text.")


(defun mouse-select ()
  "Select Emacs window the mouse is on."
  (interactive "@"))

(defun mouse-delete-window ()
  "Delete the Emacs window the mouse is on."
  (interactive "@")
  (delete-window))

(defun mouse-keep-one-window ()
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (interactive "@")
  (delete-other-windows))

(defun mouse-select-and-split ()
  "Select Emacs window mouse is on, then split it vertically in half."
  (interactive "@")
  (split-window-vertically nil))

(defun mouse-set-point (event)
  "Select Emacs window mouse is on, and move point to mouse position."
  (interactive "@e")
  (let ((window (event-window event))
	(pos (event-point event))
	(close-pos (event-closest-point event)))
    (or window (error "not in a window"))
    (select-window window)
    (if (and pos (> pos 0))
	;; If the event was over a text char, it's easy.
	(goto-char (max (min pos (point-max)) (point-min)))
      (if (and close-pos (> close-pos 0))
	  (goto-char (max (min close-pos (point-max)) (point-min)))
	;; When the event occurs outside of the frame directly to the
	;; left or right of a modeline, close-point is nil, but
	;; event-over-modeline is also nil.  That will drop us to this
	;; point.  So instead of erroring, just return nil.
	nil))))

(defun mouse-yank (event)
  "Paste text with the mouse.
If the variable `mouse-yank-at-point' is nil, then pasting occurs at the
location of the click; otherwise, pasting occurs at the current cursor
location."
  (interactive "e")
  (and (not mouse-yank-at-point)
       (mouse-set-point event))
  (funcall mouse-yank-function))

(defun click-inside-extent-p (click extent)
  "Returns non-nil if the button event is within the bounds of the primary
selection-extent, nil otherwise."
  ;; stig@hackvan.com
  (let ((ewin (event-window click))
	(epnt (event-point click)))
    (and ewin
	 epnt
	 extent
	 (eq (window-buffer ewin)
	     (extent-object extent))
	 (extent-start-position extent)
	 (> epnt (extent-start-position extent))
	 (> (extent-end-position extent) epnt))))

(defun click-inside-selection-p (click)
  (or (click-inside-extent-p click primary-selection-extent)
      (click-inside-extent-p click zmacs-region-extent)
      ))

(defun point-inside-extent-p (extent)
  "Returns non-nil if the point is within or just after the bounds of the
primary selection-extent, nil otherwise."
  ;; stig@hackvan.com
  (and extent
       (eq (current-buffer) 
	   (extent-object extent))
       (> (point) (extent-start-position extent))
       (>= (extent-end-position extent) (point))))

(defun point-inside-selection-p ()
  ;; by Stig@hackvan.com
  (or (point-inside-extent-p primary-selection-extent)
      (point-inside-extent-p zmacs-region-extent)))

;;; #### - finish this...
;;; (defun mouse-drag-or-yank (event)
;;;   "Either drag or paste the current selection.  If the variable
;;; `mouse-yank-at-point' is non-nil, then moves the cursor to the location of
;;; the click before pasting."
;;;   (interactive "e")
;;;   (if (click-inside-selection-p event)
;;;       ;; okay, this is a drag
;;;       )
;;;   )

(defun mouse-eval-sexp (click force-window)
  "Evaluate the sexp under the mouse.  Usually, this is the last sexp before
the click, but if you click on a left paren, then it is the sexp beginning
with the paren that is evaluated.  Also, since strings evaluate to themselves,
they're fed to re-search-forward and the matched region is highlighted until
the mouse button is released.

Perhaps the most useful thing about this function is that the evaluation of
the expression which is clicked upon is relative not to the window where you
click, but to the current window and the current position of point.  Thus,
you can use `mouse-eval-sexp' to interactively test code that acts upon a
buffer...something you cannot do with the standard `eval-last-sexp' function.
It's also fantastic for debugging regular expressions."
  ;; by Stig@hackvan.com
  (interactive "e\nP")
  (let (exp val result-str)
    (setq exp (save-window-excursion
		(save-excursion 
		  (mouse-set-point click)
		  (save-excursion
		    (or (looking-at "(") (forward-sexp -1))
		    (read (point-marker))))))
    (cond ((stringp exp)
	   (if (setq val (re-search-forward exp nil t))
	       (let* ((oo (make-extent (match-beginning 0) (match-end 0))))
		 (set-extent-face oo 'highlight)
		 (set-extent-priority oo 1000)
		 ;; wait for button release...
		 (setq unread-command-event (next-command-event))
		 (delete-extent oo))
	     (message "Regex \"%s\" not found" exp)
	     (ding nil 'quiet)))
	  (t (setq val (if (fboundp 'eval-interactive)
			   (eval-interactive exp)
			 (eval exp)))))
    (setq result-str (prin1-to-string val))
    ;; #### -- need better test
    (if (and (not force-window)
	     (<= (length result-str) (window-width (selected-window))))
	(message "%s" result-str)
      (with-output-to-temp-buffer "*Mouse-Eval*"
	(condition-case nil
	    (pprint val)
	  (error (prin1 val))))
      )))

(defun mouse-line-length (event)
  "Print the length of the line indicated by the pointer."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (message "Line length: %d" (- (progn (end-of-line) (point))
				  (progn (beginning-of-line) (point)))))
  (sleep-for 1))

(defun mouse-set-mark (event)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save))))

(defun mouse-scroll (event)
  "Scroll point to the mouse position."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (recenter 0)
    (scroll-right (event-x event))))

(defun mouse-del-char (event)
  "Delete the char pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (delete-char 1 nil)))

(defun mouse-kill-line (event)
  "Kill the line pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (kill-line nil)))

(defun mouse-bury-buffer (event)
  "Bury the buffer pointed to by the mouse, thus selecting the next one."
  (interactive "e")
  (save-selected-window
    (select-window (event-window event))
    (bury-buffer)))
  
(defun mouse-unbury-buffer (event)
  "Unbury and select the most recently buried buffer."
  (interactive "e")
  (save-selected-window
    (select-window (event-window event))
    (let* ((bufs (buffer-list))
	   (entry (1- (length bufs)))
	   val)
      (while (not (setq val (nth entry bufs)
			val (and (/= (aref (buffer-name val) 0)
				     ? )
				 val)))
	(setq entry (1- entry)))
      (switch-to-buffer val))))

(defun narrow-window-to-region (m n)
  "Narrow window to region between point and last mark"
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (eq (selected-window) (next-window))
	  (split-window))
      (goto-char m)
      (recenter 0)
      (if (eq (selected-window)
	      (if (zerop (minibuffer-depth))
		  (next-window)))
	  ()
	(shrink-window (- (- (window-height) (count-lines m n)) 1))))))

(defun mouse-window-to-region (event)
  "Narrow window to region between cursor and mouse pointer."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save)
      (narrow-window-to-region (region-beginning) (region-end)))))

(defun mouse-ignore ()
  "Don't do anything."
  (interactive))


;;
;; Commands for the scroll bar.
;;

;; #### this stuff has never ever been used and should be junked.

;; Vertical bar

(defun mouse-scroll-down (nlines)
  "Junk me, please."
  (interactive "@p")
  (scroll-down nlines))

(defun mouse-scroll-up (nlines)
  "Junk me, please."
  (interactive "@p")
  (scroll-up nlines))

(defun mouse-scroll-down-full ()
  "Junk me, please."
  (interactive "@")
  (scroll-down nil))

(defun mouse-scroll-up-full ()
  "Junk me, please."
  (interactive "@")
  (scroll-up nil))

(defun mouse-scroll-move-cursor (nlines)
  "Junk me, please."
  (interactive "@p")
  (move-to-window-line nlines))

(defun mouse-scroll-absolute (event)
  "Junk me, please."
  (interactive "@e")
  (let* ((position (event-x event))
	 (length (event-y event))
	 (size (buffer-size))
	 (scale-factor (max 1 (/ 8000000 size)))
	 (newpos (* (/ (* (/ size scale-factor) position) length)
		    scale-factor)))
    (goto-char newpos)
    (recenter '(4))))

;; These scroll while the invoking button is depressed.

(defvar scrolled-lines 0)
(defvar scroll-speed 1)

(defun incr-scroll-down (event)
  "Junk me, please."
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll scroll-speed))

(defun incr-scroll-up (event)
  "Junk me, please."
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll (- scroll-speed)))

(defun incremental-scroll (n)
  "Junk me, please."
  (let ((down t))
    (while down
      (sit-for mouse-track-scroll-delay)
      (cond ((input-pending-p)
	     (let ((event (next-command-event)))
	       (if (or (button-press-event-p event)
		       (button-release-event-p event))
		   (setq down nil))
	       (dispatch-event event))))
      (setq scrolled-lines (1+ (* scroll-speed scrolled-lines)))
      (scroll-down n))))

(defun incr-scroll-stop (event)
  "Junk me, please."
  (interactive "@e")
  (setq scrolled-lines 0)
  (sleep-for 1))


(defun mouse-scroll-left (ncolumns)
  "Junk me, please."
  (interactive "@p")
  (scroll-left ncolumns))

(defun mouse-scroll-right (ncolumns)
  "Junk me, please."
  (interactive "@p")
  (scroll-right ncolumns))

(defun mouse-scroll-left-full ()
  "Junk me, please."
  (interactive "@")
  (scroll-left nil))

(defun mouse-scroll-right-full ()
  "Junk me, please."
  (interactive "@")
  (scroll-right nil))

(defun mouse-scroll-move-cursor-horizontally (ncolumns)
  "Junk me, please."
  (interactive "@p")
  (move-to-column ncolumns))

(defun mouse-scroll-absolute-horizontally (event)
  "Junk me, please."
  (interactive "@e")
  (set-window-hscroll (selected-window) 33))



;;; mouse/selection tracking
;;; generalized mouse-track

(defvar mouse-track-down-hook nil
  "Function or functions called when the user presses the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-press event and a click
count (see `mouse-track-click-hook').

If any function returns non-nil, the remaining functions will not be
called.

Note that most applications should take action when the mouse is
released, not when it is pressed.'")

(defvar mouse-track-drag-hook nil
  "Function or functions called when the user drags the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with three arguments: the mouse-motion event, a click
count (see `mouse-track-click-hook'), and whether the call to
this hook occurred as a result of a drag timeout (see
`mouse-track-scroll-delay').

If any function returns non-nil, the remaining functions will not be
called.

Note that no calls to this function will be made until the user
initiates a drag (i.e. moves the mouse more than a certain
threshold in either the X or the Y direction, as defined by
`mouse-track-x-threshold' and `mouse-track-y-threshold').

See also `mouse-track-drag-up-hook'.")

(defvar mouse-track-drag-up-hook nil
  "Function or functions called when the user finishes a drag.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-press event and a click
count (see `mouse-track-click-hook').

If any function returns non-nil, the remaining functions will not be
called.

Note that this hook will not be invoked unless the user has
initiated a drag, i.e. moved the mouse more than a certain threshold
(see `mouse-track-drag-hook').  When this function is invoked,
`mouse-track-drag-hook' will have been invoked at least once.

See also `mouse-track-click-hook'.")

(defvar mouse-track-click-hook nil
  "Function or functions called when the user clicks the mouse.
`Clicking' means pressing and releasing the mouse without having
initiated a drag (i.e. without having moved more than a certain
threshold -- see `mouse-track-drag-hook').

This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-release event and a click
count, which specifies the number of times that the mouse has been
clicked in a series of clicks, each of which is separated by at most
`mouse-track-multi-click-time'.  This can be used to implement actions
that are called on double clicks, triple clicks, etc.

If any function returns non-nil, the remaining functions will not be
called.

See also `mouse-track-drag-up-hook.")

(defvar mouse-track-up-hook nil
  "Function or functions called when the user releases the mouse.
This hook is invoked by `mouse-track'; thus, it will not be called
for any buttons with a different binding.  The functions will be
called with two arguments: the button-release event and a click
count (see `mouse-track-click-hook').

For many applications, it is more appropriate to use one or both
of `mouse-track-click-hook' and `mouse-track-drag-up-hook'.")

(defvar mouse-track-cleanup-hook nil
  "Function or functions called when `mouse-track' terminates.
This hook will be called in all circumstances, even upon a
non-local exit out of `mouse-track', and so is useful for
doing cleanup work such as removing extents that may have
been created during the operation of `mouse-track'.

Unlike all of the other mouse-track hooks, this is a \"normal\"
hook: the hook functions are called with no arguments, and
all hook functions are called regardless of their return
values.")

(defvar mouse-track-multi-click-time 400
  "Maximum number of milliseconds allowed between clicks for a multi-click.
See `mouse-track-click-hook'.")

(defvar mouse-track-scroll-delay 100
  "Maximum of milliseconds between calls to `mouse-track-drag-hook'.
If the user is dragging the mouse (i.e. the button is held down and
a drag has been initiated) and does not move the mouse for this many
milliseconds, the hook will be called with t as the value of the
WAS-TIMEOUT parameter.  This can be used to implement scrolling
in a selection when the user drags the mouse out the window it
was in.

A value of nil disables the timeout feature.")

(defvar mouse-track-x-threshold '(face-width 'default)
  "Minimum number of pixels in the X direction for a drag to be initiated.
If the mouse is moved more than either the X or Y threshold while the
button is held down (see also `mouse-track-y-threshold'), then a drag
is initiated; otherwise the gesture is considered to be a click.
See `mouse-track'.

The value should be either a number of a form to be evaluated to
produce a number.")

(defvar mouse-track-y-threshold '(face-height 'default)
  "Minimum number of pixels in the Y direction for a drag to be initiated.
If the mouse is moved more than either the X or Y threshold while the
button is held down (see also `mouse-track-x-threshold'), then a drag
is initiated; otherwise the gesture is considered to be a click.
See `mouse-track'.

The value should be either a number of a form to be evaluated to
produce a number.")

;; these variables are private to mouse-track.
(defvar mouse-track-up-time nil)
(defvar mouse-track-up-x nil)
(defvar mouse-track-up-y nil)
(defvar mouse-track-timeout-id nil)
(defvar mouse-track-click-count nil)

(defun mouse-track-set-timeout (event)
  (if mouse-track-timeout-id
      (disable-timeout mouse-track-timeout-id))
  (if mouse-track-scroll-delay
      (setq mouse-track-timeout-id
	    (add-timeout (/ mouse-track-scroll-delay 1000.0)
			 'mouse-track-scroll-undefined
			 (copy-event event)))))

(defun mouse-track-run-hook (hook event &rest args)
  ;; ugh, can't use run-special-hook-with-args because we
  ;; have to get the value using symbol-value-in-buffer.
  ;; Doing a save-excursion/set-buffer is wrong because
  ;; the hook might want to change the buffer, but just
  ;; doing a set-buffer is wrong because the hook might
  ;; not want to change the buffer.
  (let ((buffer (event-buffer event)))
    (if mouse-grabbed-buffer (setq buffer mouse-grabbed-buffer))
    (if buffer
	(let ((value (symbol-value-in-buffer hook buffer nil)))
	  (if (and (listp value) (not (eq (car value) 'lambda)))
	      (let (retval)
		(while (and value
			    (not (setq retval (apply (car value) event args))))
		  (setq value (cdr value)))
		retval)
	    (apply value event args))))))

(defun mouse-track-scroll-undefined (random)
  ;; the old implementation didn't actually define this function,
  ;; and in normal use it won't ever be called because the timeout
  ;; will either be removed before it fires or will be picked off
  ;; with next-event and not dispatched.  However, if you're
  ;; attempting to debug a click-hook (which is pretty damn
  ;; difficult to do), this function may get called.
)

(defun mouse-track (event)
  "Make a selection with the mouse.  This should be bound to a mouse button.
The behavior of XEmacs during mouse selection is customizable using various
hooks and variables: see `mouse-track-click-hook', `mouse-track-drag-hook',
`mouse-track-drag-up-hook', `mouse-track-down-hook', `mouse-track-up-hook',
`mouse-track-cleanup-hook', `mouse-track-multi-click-time',
`mouse-track-scroll-delay', `mouse-track-x-threshold', and
`mouse-track-y-threshold'.

Default handlers are provided to implement standard selecting/positioning
behavior.  You can explicitly request this default behavior, and override
any custom-supplied handlers, by using the function `mouse-track-default'
instead of `mouse-track'.

Default behavior is as follows: 

If you click-and-drag, the selection will be set to the region between the
point of the initial click and the point at which you release the button.
These positions need not be ordered.

If you click-and-release without moving the mouse, then the point is moved
and the selection is disowned (there will be no selection owner).  The mark
will be set to the previous position of point.

If you double-click, the selection will extend by symbols instead of by
characters.  If you triple-click, the selection will extend by lines.

If you drag the mouse off the top or bottom of the window, you can select
pieces of text which are larger than the visible part of the buffer; the
buffer will scroll as necessary.

The selected text becomes the current X Selection.  The point will be left
at the position at which you released the button, and the mark will be left
at the initial click position."
  (interactive "e")
  (let ((mouse-down t)
	(xthresh (eval mouse-track-x-threshold))
	(ythresh (eval mouse-track-y-threshold))
	(orig-x (event-x-pixel event))
	(orig-y (event-y-pixel event))
	(buffer (event-buffer event))
	(mouse-grabbed-buffer (event-buffer event))
	mouse-moved)
    (if (or (not mouse-track-up-x)
	    (not mouse-track-up-y)
	    (not mouse-track-up-time)
	    (> (- (event-timestamp event) mouse-track-up-time)
	       mouse-track-multi-click-time)
	    (> (abs (- mouse-track-up-x orig-x)) xthresh)
	    (> (abs (- mouse-track-up-y orig-y)) ythresh))
	(setq mouse-track-click-count 1)
      (setq mouse-track-click-count (1+ mouse-track-click-count)))
    (if (not (event-window event))
	(error "Not over a window."))
    (mouse-track-run-hook 'mouse-track-down-hook
			  event mouse-track-click-count)
    (unwind-protect
	(while mouse-down
	  (setq event (next-event event))
	  (cond ((motion-event-p event)
		 (if (and (not mouse-moved)
			  (or (> (abs (- (event-x-pixel event) orig-x))
				 xthresh)
			      (> (abs (- (event-y-pixel event) orig-y))
				 ythresh)))
		     (setq mouse-moved t))
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-hook
		      event mouse-track-click-count nil))
		 (mouse-track-set-timeout event))
		((and (timeout-event-p event)
		      (eq (event-function event)
			  'mouse-track-scroll-undefined))
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-hook
		      (event-object event) mouse-track-click-count t))
		 (mouse-track-set-timeout (event-object event)))
		((button-release-event-p event)
		 (setq mouse-track-up-time (event-timestamp event))
		 (setq mouse-track-up-x (event-x-pixel event))
		 (setq mouse-track-up-y (event-y-pixel event))
		 (setq mouse-down nil)
		 (mouse-track-run-hook 'mouse-track-up-hook
		  event mouse-track-click-count)
		 (if mouse-moved
		     (mouse-track-run-hook 'mouse-track-drag-up-hook
		      event mouse-track-click-count)
		   (mouse-track-run-hook 'mouse-track-click-hook
		    event mouse-track-click-count)))
		((key-press-event-p event)
		 (error "Selection aborted"))
		(t
		 (dispatch-event event))))
      ;; protected
      (if mouse-track-timeout-id
	  (disable-timeout mouse-track-timeout-id))
      (setq mouse-track-timeout-id nil)
      (and buffer
	   (save-excursion
	     (set-buffer buffer)
	     (run-hooks 'mouse-track-cleanup-hook))))))


;;;;;;;;;;;; default handlers: new version of mouse-track

(defvar default-mouse-track-type nil)
(defvar default-mouse-track-type-list '(char word line))
(defvar default-mouse-track-window nil)
(defvar default-mouse-track-extent nil)
(defvar default-mouse-track-adjust nil)
(defvar default-mouse-track-min-anchor nil)
(defvar default-mouse-track-max-anchor nil)
(defvar default-mouse-track-result nil)
(defvar default-mouse-track-down-event nil)

(defun default-mouse-track-set-point-in-window (event window)
  (if (not (and (not (event-over-modeline-p event))
		(eq (event-window event) window)
		(let ((p (event-closest-point event)))
		  (and p (pos-visible-in-window-p p window)))))
      nil
    (mouse-set-point event)
    t))

(defun default-mouse-track-scroll-and-set-point (event window)
  (select-window window)
  (let ((edges (window-pixel-edges window))
	(row (event-y-pixel event))
	(height (face-height 'default)))
    (cond ((< (abs (- row (nth 1 edges))) (abs (- row (nth 3 edges))))
	   ;; closer to window's top than to bottom, so move up
	   (let ((delta (max 1 (/ (- (nth 1 edges) row) height))))
	     (condition-case () (scroll-down delta) (error))
	     (goto-char (window-start))))
	  ((>= (point) (point-max)))
	  (t
	   ;; scroll by one line if over the modeline or a clipped line
	   (let ((delta (if (or (event-over-modeline-p event)
				(< row (nth 3 edges)))
			    1
			  (+ (/ (- row (nth 3 edges)) height) 1)))
		 (close-pos (event-closest-point event)))
	     (condition-case () (scroll-up delta) (error))
	     (if (and close-pos (pos-visible-in-window-p close-pos))
		 (goto-char close-pos)
	       (goto-char (window-end))
	       (vertical-motion delta)
	       ;; window-end reports the end of the clipped line, even if
	       ;; scroll-on-clipped-lines is t.  compensate.
	       ;; (If window-end gets fixed this can be removed.)
	       (if (not (pos-visible-in-window-p (max (1- (point)) 
						      (point-min))))
		   (vertical-motion -1))
	       (condition-case () (backward-char 1) 
		 (error (end-of-line)))))))))


;; This remembers the last position at which the user clicked, for the
;; benefit of mouse-track-adjust (for example, button1; scroll until the
;; position of the click is off the frame; then Sh-button1 to select the
;; new region.
(defvar default-mouse-track-previous-point nil)

(defun default-mouse-track-set-point (event window)
  (if (default-mouse-track-set-point-in-window event window)
      nil
    (default-mouse-track-scroll-and-set-point event window)))

(defsubst default-mouse-track-beginning-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) "\\w\\|\\s_\\|\\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((bobp) nil)
	  ((looking-at word-constituent)
	   (backward-char)
	   (while (and (not (bobp)) (looking-at word-constituent))
	     (backward-char))
	   (if (or (not (bobp)) (not (looking-at word-constituent)))
	       (forward-char)))
	  ((looking-at white-space)
	   (backward-char)
	   (while (looking-at white-space)
	     (backward-char))
	   (forward-char)))))

(defun default-mouse-track-end-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) "\\w\\|\\s_\\|\\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((looking-at word-constituent) ; word or symbol constituent
	   (while (looking-at word-constituent)
	     (forward-char)))
	  ((looking-at white-space) ; word or symbol constituent
	   (while (looking-at white-space)
	     (forward-char))))))

(defun default-mouse-track-normalize-point (type forwardp)
  (cond ((eq type 'word)
	 ;; trap the beginning and end of buffer errors
	 (condition-case ()
	     (progn
	       (setq type (char-syntax (char-after (point))))
	       (if forwardp
		   (if (= type ?\()
		       (goto-char (scan-sexps (point) 1))
		     (if (= type  ?\))
			 (forward-char 1)
		       (default-mouse-track-end-of-word t)))
		 (if (= type ?\))
		     (goto-char (scan-sexps (1+ (point)) -1))
		   (default-mouse-track-beginning-of-word t))))
	   (error ())))
	((eq type 'line)
	 (if forwardp (end-of-line) (beginning-of-line)))
	((eq type 'buffer)
	 (if forwardp (end-of-buffer) (beginning-of-buffer)))))

(defun default-mouse-track-next-move (min-anchor max-anchor extent)
  (let ((anchor (if (<= (point) min-anchor) max-anchor min-anchor)))
    (default-mouse-track-normalize-point
      default-mouse-track-type (> (point) anchor))
    (if (consp extent)
	(default-mouse-track-next-move-rect anchor (point) extent)
      (if extent
	  (if (<= anchor (point))
	      (set-extent-endpoints extent anchor (point))
	    (set-extent-endpoints extent (point) anchor))))))

(defun default-mouse-track-next-move-rect (start end extents &optional pad-p)
  (if (< end start)
      (let ((tmp start)) (setq start end end tmp)))
  (cond
   ((= start end)		; never delete the last remaining extent
    (mapcar 'delete-extent (cdr extents))
    (setcdr extents nil)
    (set-extent-endpoints (car extents) start start))
   (t
    (let ((indent-tabs-mode nil)	; if pad-p, don't use tabs
	  (rest extents)
	  left right last p)
      (save-excursion
	(save-restriction
	  (goto-char end)
	  (setq right (current-column))
	  (goto-char start)
	  (setq left (current-column))
	  (if (< right left)
	      (let ((tmp left))
		(setq left right right tmp)
		(setq start (- start (- right left))
		      end (+ end (- right left)))))
	  ;; End may have been set to a value greater than point-max if drag
	  ;; or movement extends to end of buffer, so reset it.
	  (setq end (min end (point-max)))
	  (beginning-of-line)
	  (narrow-to-region (point) end)
	  (goto-char start)
	  (while (and rest (not (eobp)))
	    (setq p (point))
	    (move-to-column right pad-p)
	    (set-extent-endpoints (car rest) p (point))
	    ;; this code used to look at the return value
	    ;; of forward-line, but that doesn't work because
	    ;; forward-line has bogus behavior: If you're on
	    ;; the last line of a buffer but not at the very
	    ;; end, forward-line will move you to the very
	    ;; end and return 0 instead of 1, like it should.
	    ;; the result was frequent infinite loops here,
	    ;; creating very large numbers of extents at
	    ;; the same position.  There was an N^2 sorting
	    ;; algorithm in extents.c for extents at a
	    ;; particular position, and the result was very
	    ;; bad news.
	    (forward-line 1)
	    (if (not (eobp))
		(move-to-column left pad-p))
	    (setq last rest
		  rest (cdr rest)))
	  (cond (rest
		 (mapcar 'delete-extent rest)
		 (setcdr last nil))
		((not (eobp))
		 (while (not (eobp))
		   (setq p (point))
		   (move-to-column right pad-p)
		   (let ((e (make-extent p (point))))
		     (set-extent-face e (extent-face (car extents)))
		     (set-extent-priority e (extent-priority (car extents)))
		     (setcdr last (cons e nil))
		     (setq last (cdr last)))
		   (forward-line 1)
		   (if (not (eobp))
		       (move-to-column left pad-p))
		   )))))
      ))))

(defun default-mouse-track-has-selection-p (buffer)
  (and (or (not (eq 'x (console-type (selected-console))))
	   (x-selection-owner-p))
       (extent-live-p primary-selection-extent)
       (not (extent-detached-p primary-selection-extent))
       (eq buffer (extent-object primary-selection-extent))))

(defun default-mouse-track-anchor (adjust previous-point)
  (if adjust
      (if (default-mouse-track-has-selection-p (current-buffer))
	  (let ((start (extent-start-position primary-selection-extent))
		(end (extent-end-position primary-selection-extent)))
	    (cond ((< (point) start) end)
		  ((> (point) end) start)
		  ((> (- (point) start) (- end (point))) start)
		  (t end)))
	previous-point)
    (point)))

(defun default-mouse-track-maybe-own-selection (pair type)
  (let ((start (car pair))
	(end (cdr pair)))
    (or (= start end) (push-mark (if (= (point) start) end start)))
    (cond (zmacs-regions
	   (if (= start end)
	       nil
	     ;; #### UTTER KLUDGE.
	     ;; If we don't have this sit-for here, then triple-clicking
	     ;; will result in the line not being highlighted as it
	     ;; should.  What appears to be happening is this:
	     ;;
	     ;; -- each time the button goes down, the selection is
	     ;;    disowned (see comment "remove the existing selection
	     ;;    to unclutter the display", below).
	     ;; -- this causes a SelectionClear event to be sent to
	     ;;    XEmacs.
	     ;; -- each time the button goes up except the first, the
	     ;;    selection is owned again.
	     ;; -- later, XEmacs processes the SelectionClear event.
	     ;;    The selection code attempts to keep track of the
	     ;;    time that it last asserted the selection, and
	     ;;    compare it to the time of the SelectionClear event,
	     ;;    to see if it's a bogus notification or not (as
	     ;;    is the case here).  However, for some unknown
	     ;;    reason this doesn't work in the triple-clicking
	     ;;    case, and the selection code bogusly thinks this
	     ;;    SelectionClear event is the real thing.
	     ;; -- putting the sit-for in causes the pending
	     ;;    SelectionClear events to get processed before
	     ;;    the selection is reasserted, so everything works
	     ;;    out OK.
	     ;;
	     ;; Presumably(?) this means there is a weird timing bug
	     ;; in the selection code, but there's not a chance in hell
	     ;; that I have the patience to track it down.  Blame the
	     ;; designers of X for fucking everything up so badly.
	     ;;
	     ;; This was originally a sit-for 0 but that wasn't
	     ;; sufficient to make things work.  Even this isn't
	     ;; always sufficient but it seems to give something
	     ;; approaching a 99% success rate.  Making it higher yet
	     ;; would help guarantee success with the price that the
	     ;; delay would start to become noticable.
	     ;;
	     (sit-for 0.15 t)
	     (zmacs-activate-region)))
	  ((eq 'x (console-type (selected-console)))
	   (if (= start end)
	       (x-disown-selection type)
	     (if (consp default-mouse-track-extent)
		 ;; own the rectangular region
		 ;; this is a hack
		 (let ((r default-mouse-track-extent))
		   (save-excursion
		     (set-buffer (get-buffer-create " *rect yank temp buf*"))
		     (while r
		       (insert (extent-string (car r)) "\n")
		       (setq r (cdr r)))
		     (x-own-selection (buffer-substring (point-min) (point-max)))
		     (kill-buffer (current-buffer))))
	       (x-own-selection (cons (set-marker (make-marker) start)
				      (set-marker (make-marker) end))
				type)))))
    (if (and (eq 'x (console-type (selected-console)))
	     (not (= start end)))
	;; I guess cutbuffers should do something with rectangles too.
	;; does anybody use them?
	(x-store-cutbuffer (buffer-substring start end)))))

(defun default-mouse-track-deal-with-down-event (click-count)
  (let ((event default-mouse-track-down-event))
    (if (null event) nil
      (select-frame (event-frame event))
      (let ((adjust default-mouse-track-adjust)
	    ;; ####When you click on the splash-screen,
	    ;; event-{closest-,}point can be out of bounds.  Should
	    ;; event-closest-point really be allowed to return a bad
	    ;; position like that?  Maybe pixel_to_glyph_translation
	    ;; needs to invalidate its cache when the buffer changes.
	    ;; -dkindred@cs.cmu.edu
	    (close-pos  (save-excursion
			  (set-buffer (event-buffer event))
			  (let ((p (event-closest-point event)))
			    (and p (min (max p (point-min)) (point-max))))))
	    extent previous-point)
	
	(if (not (event-window event))
	    (error "not over window?"))
	(setq default-mouse-track-type
	      (nth (mod (1- click-count)
			(length default-mouse-track-type-list))
		   default-mouse-track-type-list))
	(setq default-mouse-track-window (event-window event))
	;; Note that the extent used here is NOT the extent which
	;; ends up as the value of zmacs-region-extent - this one is used
	;; just during mouse-dragging.
	(setq default-mouse-track-extent
	      (make-extent close-pos close-pos (event-buffer event)))
	(setq extent default-mouse-track-extent)
	(set-extent-face extent 'zmacs-region)
	;; While the selection is being dragged out, give the selection extent
	;; slightly higher priority than any mouse-highlighted extent, so that
	;; the exact endpoints of the selection will be visible while the mouse
	;; is down.  Normally, the selection and mouse highlighting have the
	;; same priority, so that conflicts between the two of them are
	;; resolved by the usual size-and-endpoint-comparison method.
	(set-extent-priority extent (1+ mouse-highlight-priority))
	(if mouse-track-rectangle-p
	    (setq default-mouse-track-extent
		  (list default-mouse-track-extent)))
	
	(setq previous-point
	      (if (and adjust
		       (markerp default-mouse-track-previous-point)
		       (eq (current-buffer)
			   (marker-buffer default-mouse-track-previous-point)))
		  (marker-position default-mouse-track-previous-point)
		(point)))
	(default-mouse-track-set-point event default-mouse-track-window)
	(if (not adjust)
	    (if (markerp default-mouse-track-previous-point)
		(set-marker default-mouse-track-previous-point (point))
	      (setq default-mouse-track-previous-point (point-marker))))
	;;
	;; adjust point to a word or line boundary if appropriate
	(let ((anchor (default-mouse-track-anchor adjust previous-point)))
	  (setq default-mouse-track-min-anchor
		(save-excursion (goto-char anchor)
				(default-mouse-track-normalize-point
				 default-mouse-track-type nil)
				(point)))
	  (setq default-mouse-track-max-anchor
		(save-excursion (goto-char anchor)
				(default-mouse-track-normalize-point
				 default-mouse-track-type t)
				(point))))
	;;
	;; remove the existing selection to unclutter the display
	(if (not adjust)
	    (cond (zmacs-regions
		   (zmacs-deactivate-region))
		  ((eq 'x (console-type (selected-console)))
		   (x-disown-selection)))))
      (setq default-mouse-track-down-event nil))))

(defun default-mouse-track-down-hook (event click-count)
  (setq default-mouse-track-down-event (copy-event event))
  nil)

(defun default-mouse-track-cleanup-hook ()
  (let ((extent default-mouse-track-extent))
    (if (consp extent) ; rectangle-p
	(mapcar 'delete-extent extent)
      (if extent
	  (delete-extent extent)))))

(defun default-mouse-track-cleanup-extent ()
  (let ((dead-func
	 (function (lambda (x)
		     (or (not (extent-live-p x))
			 (extent-detached-p x)))))
	(extent default-mouse-track-extent))
    (if (consp extent)
	(if (some dead-func extent)
	    (let (newval)
	      (mapcar (function (lambda (x)
				  (if (not (funcall dead-func x))
				      (setq newval (cons x newval)))))
		      extent)
	      (setq default-mouse-track-extent (nreverse newval))))
      (if (funcall dead-func extent)
	  (setq default-mouse-track-extent nil)))))

(defun default-mouse-track-drag-hook (event click-count was-timeout)
  (default-mouse-track-deal-with-down-event click-count)
  (default-mouse-track-set-point event default-mouse-track-window)
  (default-mouse-track-cleanup-extent)
  (default-mouse-track-next-move default-mouse-track-min-anchor
    default-mouse-track-max-anchor
    default-mouse-track-extent)
  t)

(defun default-mouse-track-return-dragged-selection (event)
  (default-mouse-track-cleanup-extent)
  (let ((extent default-mouse-track-extent)
	result)
    (default-mouse-track-set-point-in-window event default-mouse-track-window)
    (default-mouse-track-next-move default-mouse-track-min-anchor
			   default-mouse-track-max-anchor
			   extent)
    (cond ((consp extent) ; rectangle-p
	   (let ((first (car extent))
		 (last (car (setq extent (nreverse extent)))))
	     ;; nreverse is destructive so we need to reset this
	     (setq default-mouse-track-extent extent)
	     (setq result (cons (extent-start-position first)
				(extent-end-position last)))
	     ;; kludge to fix up region when dragging backwards...
	     (if (and (/= (point) (extent-start-position first))
		      (/= (point) (extent-end-position last))
		      (= (point) (extent-end-position first)))
		 (goto-char (car result)))))
	  (extent
	   (setq result (cons (extent-start-position extent)
			      (extent-end-position extent)))))
    ;; Minor kludge: if we're selecting in line-mode, include the
    ;; final newline.  It's hard to do this in *-normalize-point.
    (if (and result (eq default-mouse-track-type 'line))
	(let ((end-p (= (point) (cdr result))))
	  (goto-char (cdr result))
	  (if (not (eobp))
	      (setcdr result (1+ (cdr result))))
	  (goto-char (if end-p (cdr result) (car result)))))
;;;	  ;; Minor kludge sub 2.  If in char mode, and we drag the
;;;	  ;; mouse past EOL, include the newline.
;;;	  ;;
;;;	  ;; Major problem: can't easily distinguish between being
;;;	  ;; just past the last char on a line, and well past it,
;;;	  ;; to determine whether or not to include it in the region
;;;	  ;;
;;;	  (if nil ; (eq default-mouse-track-type 'char)
;;;	      (let ((after-end-p (and (not (eobp))
;;; 				      (eolp)
;;;				      (> (point) (car result)))))
;;;		(if after-end-p
;;;		    (progn
;;;		      (setcdr result (1+ (cdr result)))
;;;		      (goto-char (cdr result))))))
    result))

(defun default-mouse-track-drag-up-hook (event click-count)
  (let ((result (default-mouse-track-return-dragged-selection event)))
    (if result
	(default-mouse-track-maybe-own-selection result 'PRIMARY)))
  t)

(defun default-mouse-track-click-hook (event click-count)
  (default-mouse-track-drag-hook event click-count nil)
  (default-mouse-track-drag-up-hook event click-count)
  t)

(add-hook 'mouse-track-down-hook 'default-mouse-track-down-hook)
(add-hook 'mouse-track-drag-hook 'default-mouse-track-drag-hook)
(add-hook 'mouse-track-drag-up-hook 'default-mouse-track-drag-up-hook)
(add-hook 'mouse-track-click-hook 'default-mouse-track-click-hook)
(add-hook 'mouse-track-cleanup-hook 'default-mouse-track-cleanup-hook)


;;;;;;;;;;;; other mouse-track stuff (mostly associated with the
;;;;;;;;;;;; default handlers)

(defun mouse-track-default (event)
  "Invoke `mouse-track' with only the default handlers active."
  (interactive "e")
  (let ((mouse-track-down-hook 'default-mouse-track-down-hook)
	(mouse-track-drag-hook 'default-mouse-track-drag-hook)
	(mouse-track-drag-up-hook 'default-mouse-track-drag-up-hook)
	(mouse-track-click-hook 'default-mouse-track-click-hook)
	(mouse-track-cleanup-hook 'default-mouse-track-cleanup-hook))
    (mouse-track event)))

(defun mouse-track-do-rectangle (event)
  "Like `mouse-track' but selects rectangles instead of regions."
  (interactive "e")
  (let ((mouse-track-rectangle-p t))
	(mouse-track event)))

(defun mouse-track-adjust (event)
  "Extend the existing selection.  This should be bound to a mouse button.
The selection will be enlarged or shrunk so that the point of the mouse
click is one of its endpoints.  This function in fact behaves fairly
similarly to `mouse-track', but begins by extending the existing selection
(or creating a new selection from the previous text cursor position to
the current mouse position) instead of creating a new, empty selection.

The mouse-track handlers are run from this command just like from
`mouse-track'.  Therefore, do not call this command from a mouse-track
handler!"
  (interactive "e")
  (let ((default-mouse-track-adjust t))
    (mouse-track event)))

(defun mouse-track-adjust-default (event)
  "Extend the existing selection, using only the default handlers.
This is just like `mouse-track-adjust' but will override any
custom mouse-track handlers that the user may have installed."
  (interactive "e")
  (let ((default-mouse-track-adjust t))
    (mouse-track-default event)))

(defvar mouse-track-insert-selected-region nil)

(defun mouse-track-insert-drag-up-hook (event click-count)
  (setq mouse-track-insert-selected-region
	(default-mouse-track-return-dragged-selection event)))
  
(defun mouse-track-insert (event &optional delete)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track],
except that point is not moved; the selected text is immediately inserted
after being selected\; and the selection is immediately disowned afterwards."
  (interactive "*e")
  (setq mouse-track-insert-selected-region nil)
  (let ((mouse-track-drag-up-hook 'mouse-track-insert-drag-up-hook)
 	(mouse-track-click-hook 'mouse-track-insert-click-hook)
	s)
    (save-excursion
      (save-window-excursion
	(mouse-track event)
	(if (consp mouse-track-insert-selected-region)
	    (let ((pair mouse-track-insert-selected-region))
	      (setq s (prog1
			  (buffer-substring (car pair) (cdr pair))
			(if delete
			    (kill-region (car pair) (cdr pair)))))))))
	(or (null s) (equal s "") (insert s))))

(defun mouse-track-insert-click-hook (event click-count)
  (default-mouse-track-drag-hook event click-count nil)
  (mouse-track-insert-drag-up-hook event click-count)
  t)

(defun mouse-track-delete-and-insert (event)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track],
except that point is not moved; the selected text is immediately inserted
after being selected\; and the text of the selection is deleted."
  (interactive "*e")
  (mouse-track-insert event t))

;;;;;;;;;;;;;;;;;;;;;;;;


(defvar inhibit-help-echo nil
  "Inhibits display of `help-echo' extent properties in the minibuffer.")
(defvar last-help-echo-object nil)
(defvar help-echo-owns-message nil)

(defun clear-help-echo (&optional ignored-frame)
  (if help-echo-owns-message
      (progn
	(setq help-echo-owns-message nil
	      last-help-echo-object nil)
	(clear-message 'help-echo))))

(defun show-help-echo (mess)
  ;; (clear-help-echo)
  (setq help-echo-owns-message t)
  (display-message 'help-echo mess))

(add-hook 'mouse-leave-frame-hook 'clear-help-echo)

(defun default-mouse-motion-handler (event)
  "For use as the value of `mouse-motion-handler'.
This implements the various pointer-shape variables,
as well as extent highlighting, help-echo, toolbar up/down,
and `mode-motion-hook'."
  (let* ((frame (or (event-frame event) (selected-frame)))
	 (window (event-window event))
	 (buffer (event-buffer event))
	 (point (and buffer (event-point event)))
	 (modeline-point (and buffer (event-modeline-position event)))
	 (extent (and point (extent-at point buffer 'mouse-face)))
	 (glyph1 (event-glyph-extent event))
	 (glyph (and glyph1 (extent-live-p glyph1) glyph1))
	 (user-pointer1 (or (and glyph (extent-property glyph 'pointer))
			    (and point
				 (condition-case nil
				     (extent-at point buffer 'pointer)
				   (error nil)))
			    (and modeline-point
				 (condition-case nil
				     (extent-at modeline-point
						(symbol-value-in-buffer
						 'generated-modeline-string
						 buffer) 'pointer)))))
	 (user-pointer (and user-pointer1 (extent-live-p user-pointer1)
			    (extent-property user-pointer1 'pointer)))
	 (button (event-toolbar-button event))
	 (help (or (and glyph (extent-property glyph 'help-echo) glyph)
		   (and button (not (null (toolbar-button-help-string button)))
			button)
		   (and point
			(condition-case nil
			    (extent-at point buffer 'help-echo)
			  (error nil)))
		   (and modeline-point
			(condition-case nil
			    (extent-at modeline-point
				       (symbol-value-in-buffer
					'generated-modeline-string
					buffer) 'help-echo)))))
	 ;; vars is a list of glyph variables to check for a pointer
	 ;; value.
	 (vars (cond
		;; Checking if button is non-nil is not sufficent
		;; since the pointer could be over a blank portion
		;; of the toolbar.
		((event-over-toolbar-p event)
		 '(toolbar-pointer-glyph nontext-pointer-glyph
					 text-pointer-glyph))
		((or extent glyph)
		 '(selection-pointer-glyph text-pointer-glyph))
		((event-over-modeline-p event)
		 '(modeline-pointer-glyph nontext-pointer-glyph
					  text-pointer-glyph))
		(point '(text-pointer-glyph))
		(buffer '(nontext-pointer-glyph text-pointer-glyph))
		(t '(modeline-pointer-glyph nontext-pointer-glyph
					    text-pointer-glyph))))
	 pointer)
    (if (and user-pointer (glyphp user-pointer))
	(setq vars (cons 'user-pointer vars)))
    (while (and vars (not (pointer-image-instance-p pointer)))
      (setq pointer (glyph-image-instance (symbol-value (car vars))
					  (or window frame))
	    vars (cdr vars)))

    (if (pointer-image-instance-p pointer)
	(set-frame-pointer frame pointer))

    ;; If last-pressed-toolbar-button is not nil, then check and see
    ;; if we have moved to a new button and adjust the down flags
    ;; accordingly.
    (if (and (featurep 'toolbar) toolbar-active)
	(if (not (eq last-pressed-toolbar-button button))
	    (progn
	      (release-previous-toolbar-button event)
	      (and button (press-toolbar-button event)))))
    
    (cond (extent (highlight-extent extent t))
	  (glyph (highlight-extent glyph t))
	  (t (highlight-extent nil nil)))
    (cond ((extentp help)
           (or inhibit-help-echo
               (eq help last-help-echo-object) ;save some time
               (let ((hprop (extent-property help 'help-echo)))
                 (setq last-help-echo-object help)
                 (or (stringp hprop)
                     (setq hprop (funcall hprop help)))
                 (and hprop (show-help-echo hprop)))))
	  ((and (featurep 'toolbar)
                (toolbar-button-p help)
                (toolbar-button-enabled-p help))
	   (or (not toolbar-help-enabled)
	       (eq help last-help-echo-object) ;save some time
	       (let ((hstring (toolbar-button-help-string button)))
		 (setq last-help-echo-object help)
		 (or (stringp hstring)
		     (setq hstring (funcall hstring help)))
		 (show-help-echo hstring))))
          (last-help-echo-object
	   (clear-help-echo)))
    (if mouse-grabbed-buffer (setq buffer mouse-grabbed-buffer))
    (if (and buffer (symbol-value-in-buffer 'mode-motion-hook buffer nil))
	(save-window-excursion
	  (set-buffer buffer)
	  (run-hook-with-args 'mode-motion-hook event)

	  ;; If the mode-motion-hook created a highlightable extent around
	  ;; the mouse-point, highlight it right away.  Otherwise it wouldn't
	  ;; be highlighted until the *next* motion event came in.
	  (if (and point
		   (null extent)
		   (setq extent (extent-at point
					   (event-buffer event) ; not buffer
					   'mouse-face)))
	      (highlight-extent extent t)))))
  nil)

(setq mouse-motion-handler 'default-mouse-motion-handler)