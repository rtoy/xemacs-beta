;;; window.el --- XEmacs window commands aside from those written in C.

;; Copyright (C) 1985, 1989, 1993-94, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: frames, extensions, dumped

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Emacs/Mule zeta.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

;;;; Window tree functions.

(defun one-window-p (&optional nomini which-frames which-devices)
  "Return non-nil if the selected window is the only window (in its frame).
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.

By default, only the windows in the selected frame are considered.
The optional argument WHICH-FRAMES changes this behavior:
WHICH-FRAMES nil or omitted means count only the selected frame,
plus the minibuffer it uses (which may be on another frame).
WHICH-FRAMES = `visible' means include windows on all visible frames.
WHICH-FRAMES = 0 means include windows on all visible and iconified frames.
WHICH-FRAMES = t means include windows on all frames including invisible frames.
If WHICH-FRAMES is any other value, count only the selected frame.

The optional third argument WHICH-DEVICES further clarifies on which
devices to search for frames as specified by WHICH-FRAMES.  This value
is only meaningful if WHICH-FRAMES is non-nil.
If nil or omitted, search all devices on the selected console.
If a device, only search that device.
If a console, search all devices on that console.
If a device type, search all devices of that type.
If `window-system', search all devices on window-system consoles.
Any other non-nil value means search all devices."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) which-frames which-devices))))

(defun walk-windows (function &optional minibuf which-frames which-devices)
  "Cycle through all visible windows, calling FUNCTION for each one.
FUNCTION is called with a window as argument.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, when a separate minibuffer frame is active,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.  But if the
minibuffer does not count, only the selected window counts.

By default, only the windows in the selected frame are included.
The optional argument WHICH-FRAMES changes this behavior:
WHICH-FRAMES nil or omitted means cycle within the frames as specified above.
WHICH-FRAMES = `visible' means include windows on all visible frames.
WHICH-FRAMES = 0 means include windows on all visible and iconified frames.
WHICH-FRAMES = t means include windows on all frames including invisible frames.
Anything else means restrict to WINDOW's frame.

The optional fourth argument WHICH-DEVICES further clarifies on which
devices to search for frames as specified by WHICH-FRAMES.  This value
is only meaningful if WHICH-FRAMES is non-nil.
If nil or omitted, search all devices on the selected console.
If a device, only search that device.
If a console, search all devices on that console.
If a device type, search all devices of that type.
If `window-system', search all devices on window-system consoles.
Any other non-nil value means search all devices."
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
		   (next-window walk-windows-current minibuf which-frames
				which-devices))
	     (funcall function walk-windows-current)
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
;		   (next-window walk-windows-current minibuf which-frames
;				which-devices))
;	     (not (memq walk-windows-current walk-windows-history)))
;      (setq walk-windows-history (cons walk-windows-current
;				       walk-windows-history))
;      (funcall function walk-windows-current))))

(defun get-window-with-predicate (predicate &optional minibuf
					    all-frames default)
  "Return a window satisfying PREDICATE.

This function cycles through all visible windows using `walk-windows',
calling PREDICATE on each one.  PREDICATE is called with a window as
argument.  The first window for which PREDICATE returns a non-nil
value is returned.  If no window satisfies PREDICATE, DEFAULT is
returned.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, if you are using a separate minibuffer frame
and the minibuffer is active and MINIBUF says it counts,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.

ALL-FRAMES is the optional third argument.
ALL-FRAMES nil or omitted means cycle within the frames as specified above.
ALL-FRAMES = `visible' means include windows on all visible frames.
ALL-FRAMES = 0 means include windows on all visible and iconified frames.
ALL-FRAMES = t means include windows on all frames including invisible frames.
If ALL-FRAMES is a frame, it means include windows on that frame.
Anything else means restrict to the selected frame."
  (catch 'found
    (walk-windows #'(lambda (window)
		      (when (funcall predicate window)
			(throw 'found window)))
		  minibuf all-frames)
    default))

(defalias 'some-window 'get-window-with-predicate)

(defun minibuffer-window-active-p (window)
  "Return t if WINDOW (a minibuffer window) is now active."
  (eq window (active-minibuffer-window)))

(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY.
The value returned is the value of the last form in BODY."
  (let ((old-window (gensym "ssw")))
  `(let ((,old-window (selected-window)))
     (unwind-protect
	 (progn ,@body)
       (when (window-live-p ,old-window)
	 (select-window ,old-window))))))

(defmacro with-selected-window (window &rest body)
  "Execute forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY."
  `(save-selected-window
     (select-window ,window)
     ,@body))

(defmacro save-window-excursion (&rest body)
  "Execute body, preserving window sizes and contents.
Restores which buffer appears in which window, where display starts,
as well as the current buffer.
Does not restore the value of point in current buffer."
  (let ((window-config (gensym 'window-config)))
    `(let ((,window-config (current-window-configuration)))
      (unwind-protect
	  (progn ,@body)
	(set-window-configuration ,window-config)))))

(defun count-windows (&optional minibuf)
   "Return the number of visible windows.
This counts the windows in the selected frame and (if the minibuffer is
to be counted) its minibuffer frame (if that's not the same frame).
The optional arg MINIBUF non-nil means count the minibuffer
even if it is inactive."
   (let ((count 0))
     (walk-windows (function (lambda (w)
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun window-safely-shrinkable-p (&optional window)
  "Non-nil if the WINDOW can be shrunk without shrinking other windows.
If WINDOW is nil or omitted, it defaults to the currently selected window."
  (save-selected-window
    (when window (select-window window))
    (or (and (not (eq window (frame-first-window)))
	     (= (car (window-pixel-edges))
		(car (window-pixel-edges (previous-window)))))
	(= (car (window-pixel-edges))
	   (car (window-pixel-edges (next-window)))))))

(defun balance-windows ()
  "Make all visible windows the same height (approximately)."
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
    (walk-windows (function
		   (lambda (w)
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
(defcustom split-window-keep-point t
  "*If non-nil, split windows keeps the original point in both children.
This is often more convenient for editing.
If nil, adjust point in each of the two windows to minimize redisplay.
This is convenient on slow terminals, but point can move strangely."
  :type 'boolean
  :group 'windows)

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
The uppermost window gets ARG lines and the other gets the rest.
Negative arg means select the size of the lowermost window instead.
With no argument, split equally or close to it.
Both windows display the same buffer now current.

If the variable `split-window-keep-point' is non-nil, both new windows
will get the same value of point as the current window.  This is often
more convenient for editing.

Otherwise, we choose window starts so as to minimize the amount of
redisplay; this is convenient on slow terminals.  The new selected
window is the one that the current value of point appears in.  The
value of point can change if the text around point is hidden by the
new mode line.

Programs should probably use split-window instead of this."
  (interactive "P")
  (let ((old-w (selected-window))
	(old-point (point))
	(size (and arg (prefix-numeric-value arg)))
	(window-full-p nil)
	new-w bottom moved)
    (and size (< size 0) (setq size (+ (window-height) size)))
    (setq new-w (split-window nil size))
    (or split-window-keep-point
	(progn
	  (save-excursion
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
	    (setq moved (vertical-motion (window-height)))
	    (set-window-start new-w (point))
	    (if (> (point) (window-point new-w))
		(set-window-point new-w (point)))
	    (and (= moved (window-height))
		 (progn
		   (setq window-full-p t)
		   (vertical-motion -1)))
	    (setq bottom (point)))
	  (and window-full-p
	       (<= bottom (point))
	       (set-window-point old-w (1- bottom)))
	  (and window-full-p
	       (<= (window-start new-w) old-point)
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

; (defun window-buffer-height (window)
;   "Return the height (in screen lines) of the buffer that WINDOW is displaying."
;   (save-excursion
;     (set-buffer (window-buffer window))
;     (goto-char (point-min))
;     (let ((ignore-final-newline
;            ;; If buffer ends with a newline, ignore it when counting height
;            ;; unless point is after it.
;            (and (not (eobp)) (eq ?\n (char-after (1- (point-max)))))))
;       (+ 1 (nth 2 (compute-motion (point-min)
;                                   '(0 . 0)
;                                   (- (point-max) (if ignore-final-newline 1 0))
;                                   (cons 0 100000000)
;                                   (window-width window)
;                                   nil
;                                   window))))))

(defun count-screen-lines (&optional beg end count-final-newline window)
  "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        (1+ (vertical-motion (buffer-size) window))))))

; (defun fit-window-to-buffer (&optional window max-height min-height)
;   "Make WINDOW the right size to display its contents exactly.
; If WINDOW is omitted or nil, it defaults to the selected window.
; If the optional argument MAX-HEIGHT is supplied, it is the maximum height
;   the window is allowed to be, defaulting to the frame height.
; If the optional argument MIN-HEIGHT is supplied, it is the minimum
;   height the window is allowed to be, defaulting to `window-min-height'.

; The heights in MAX-HEIGHT and MIN-HEIGHT include the mode-line and/or
; header-line."
;   (interactive)

;   (when (null window)
;     (setq window (selected-window)))
;   (when (null max-height)
;     (setq max-height (frame-height (window-frame window))))

;   (let* ((buf
; 	  ;; Buffer that is displayed in WINDOW
; 	  (window-buffer window))
; 	 (window-height
; 	  ;; The current height of WINDOW
; 	  (window-height window))
; 	 (desired-height
; 	  ;; The height necessary to show the buffer displayed by WINDOW
; 	  ;; (`count-screen-lines' always works on the current buffer).
; 	  (with-current-buffer buf
; 	    (+ (count-screen-lines)
; 	       ;; If the buffer is empty, (count-screen-lines) is
; 	       ;; zero.  But, even in that case, we need one text line
; 	       ;; for cursor.
; 	       (if (= (point-min) (point-max))
; 		   1 0)
; 	       ;; For non-minibuffers, count the mode-line, if any
; 	       (if (and (not (window-minibuffer-p window))
; 			mode-line-format)
; 		   1 0)
; 	       ;; Count the header-line, if any
; 	       (if header-line-format 1 0))))
; 	 (delta
; 	  ;; Calculate how much the window height has to change to show
; 	  ;; desired-height lines, constrained by MIN-HEIGHT and MAX-HEIGHT.
; 	  (- (max (min desired-height max-height)
; 		  (or min-height window-min-height))
; 	     window-height))
; 	 ;; We do our own height checking, so avoid any restrictions due to
; 	 ;; window-min-height.
; 	 (window-min-height 1))

;     ;; Don't try to redisplay with the cursor at the end
;     ;; on its own line--that would force a scroll and spoil things.
;     (when (with-current-buffer buf
; 	    (and (eobp) (bolp) (not (bobp))))
;       (set-window-point window (1- (window-point window))))

;     (save-selected-window
;       (select-window window)

;       ;; Adjust WINDOW to the nominally correct size (which may actually
;       ;; be slightly off because of variable height text, etc).
;       (unless (zerop delta)
; 	(enlarge-window delta))

;       ;; Check if the last line is surely fully visible.  If not,
;       ;; enlarge the window.
;       (let ((end (with-current-buffer buf
; 		   (save-excursion
; 		     (goto-char (point-max))
; 		     (when (and (bolp) (not (bobp)))
; 		       ;; Don't include final newline
; 		       (backward-char 1))
; 		     (when truncate-lines
; 		       ;; If line-wrapping is turned off, test the
; 		       ;; beginning of the last line for visibility
; 		       ;; instead of the end, as the end of the line
; 		       ;; could be invisible by virtue of extending past
; 		       ;; the edge of the window.
; 		       (forward-line 0))
; 		     (point)))))
; 	(set-window-vscroll window 0)
; 	(while (and (< desired-height max-height)
; 		    (= desired-height (window-height window))
; 		    (not (pos-visible-in-window-p end window)))
; 	  (enlarge-window 1)
; 	  (setq desired-height (1+ desired-height)))))))

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
    (let ((test-pos
	   (- (point-max)
	      ;; If buffer ends with a newline, ignore it when counting
	      ;; height unless point is after it.
	      (if (and (not (eobp))
		       (eq ?\n (char-after (1- (point-max)))))
		  1 0)))
	  (mini (frame-property (window-frame window) 'minibuffer)))
      (if (and (< 1 (let ((frame (selected-frame)))
		      (select-frame (window-frame window))
		      (unwind-protect
			  (count-windows)
			(select-frame frame))))
	       ;; check to make sure that the window is the full width
	       ;; of the frame
	       (window-leftmost-p window)
	       (window-rightmost-p window)
	       ;; The whole buffer must be visible.
	       (pos-visible-in-window-p (point-min) window)
	       ;; The frame must not be minibuffer-only.
	       (not (eq mini 'only)))
	  (progn
	    (goto-char (point-min))
       	    (while (and (pos-visible-in-window-p test-pos window)
			(> (window-height window) window-min-height))
	      (shrink-window 1 nil window))
	    (if (not (pos-visible-in-window-p test-pos window))
		(enlarge-window 1 nil window)))))))

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  (interactive)
  (if (yes-or-no-p (format "Kill buffer `%s'? " (buffer-name)))
      (let ((buffer (current-buffer)))
	(delete-window (selected-window))
	(kill-buffer buffer))
    (error "Aborted")))

(defun quit-window (&optional kill window)
  "Quit the current buffer.  Bury it, and maybe delete the selected frame.
\(The frame is deleted if it is contains a dedicated window for the buffer.)
With a prefix argument, kill the buffer instead.

Noninteractively, if KILL is non-nil, then kill the current buffer,
otherwise bury it.

If WINDOW is non-nil, it specifies a window; we delete that window,
and the buffer that is killed or buried is the one in that window."
  (interactive "P")
  (let ((buffer (window-buffer window))
	(frame (window-frame (or window (selected-window))))
	(window-solitary
	 (save-selected-window
	   (if window
	       (select-window window))
	   (one-window-p t)))
	window-handled)

    (save-selected-window
      (if window
	  (select-window window))
      (or (window-minibuffer-p)
	  (window-dedicated-p (selected-window))
	  (switch-to-buffer (other-buffer))))

    ;; Get rid of the frame, if it has just one dedicated window
    ;; and other visible frames exist.
    (and (or (window-minibuffer-p) (window-dedicated-p window))
	 (delq frame (visible-frame-list))
	 window-solitary
	 (if (and (eq default-minibuffer-frame frame)
		  (= 1 (length (minibuffer-frame-list))))
	     (setq window nil)
	   (delete-frame frame)
	   (setq window-handled t)))

    ;; Deal with the buffer.
    (if kill
	(kill-buffer buffer)
      (bury-buffer buffer))

    ;; Maybe get rid of the window.
    (and window (not window-handled) (not window-solitary)
	 (delete-window window))))

;;; window.el ends here
