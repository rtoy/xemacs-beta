;;; rsz-minibuf.el --- dynamically resize minibuffer to display entire contents

;;; Copyright (C) 1990 Roland McGrath
;;; Copyright (C) 1993, 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;;         Roland McGrath <roland@prep.ai.mit.edu>
;;; Modified for Lucid Emacs By: Peter Stout <pds@cs.cmu.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: minibuffer, window, frames, display
;;; Status: Known to work in FSF GNU Emacs 19.23 and Lucid Emacs 19.9.

;;; $Id: rsz-minibuf.el,v 1.1.1.1 1996/12/18 03:30:55 steve Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package allows the entire contents (or as much as possible) of the
;;; minibuffer to be visible at once when typing.  As the end of a line is
;;; reached, the minibuffer will resize itself.  When the user is done
;;; typing, the minibuffer will return to its original size.

;;; In window systems where it is possible to have a frame in which the
;;; minibuffer is the only window, the frame itself can be resized.  In FSF
;;; GNU Emacs 19.22 and earlier, the frame may not be properly returned to
;;; its original size after it ceases to be active because
;;; `minibuffer-exit-hook' didn't exist until version 19.23.

;;; NOTE: The code to resize frames has not been tested under Lucid Emacs,
;;; because detached minibuffers are broken.

;;; Note that the minibuffer and echo area are not the same!  They simply
;;; happen to occupy roughly the same place on the frame.  Messages put in
;;; the echo area will not cause any resizing by this package.

;;; This package is considered a minor mode but it doesn't put anything in
;;; minor-mode-alist because this mode is specific to the minibuffer, which
;;; has no modeline.

;;; To use this package, put the following in your .emacs:
;;;
;;;     (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
;;;
;;; Invoking the command `resize-minibuffer-mode' will then enable this mode.

;;; Code:


;;;###autoload
(defvar resize-minibuffer-mode nil
  "*If non-`nil', resize the minibuffer so its entire contents are visible.")

;;;###autoload
(defvar resize-minibuffer-window-max-height nil
  "*Maximum size the minibuffer window is allowed to become.
If less than 1 or not a number, the limit is the height of the frame in
which the active minibuffer window resides.")

;;;###autoload
(defvar resize-minibuffer-window-exactly t
  "*If non-`nil', make minibuffer exactly the size needed to display all its contents.
Otherwise, the minibuffer window can temporarily increase in size but
never get smaller while it is active.")


;;;###autoload
(defvar resize-minibuffer-frame nil
  "*If non-`nil' and the active minibuffer is the sole window in its frame, allow changing the frame height.")

;;;###autoload
(defvar resize-minibuffer-frame-max-height nil
  "*Maximum size the minibuffer frame is allowed to become.
If less than 1 or not a number, there is no limit.")

;;;###autoload
(defvar resize-minibuffer-frame-exactly nil
  "*If non-`nil', make minibuffer frame exactly the size needed to display all its contents.
Otherwise, the minibuffer frame can temporarily increase in size but
never get smaller while it is active.")


;;;###autoload
(defun resize-minibuffer-mode (&optional prefix)
  "Enable or disable resize-minibuffer mode.
A negative prefix argument disables this mode.  A positive argument or
argument of 0 enables it.

When this minor mode is enabled, the minibuffer is dynamically resized to
contain the entire region of text put in it as you type.

The variable `resize-minibuffer-mode' is set to t or nil depending on
whether this mode is active or not.

The maximum height to which the minibuffer can grow is controlled by the
variable `resize-minibuffer-window-max-height'.

The variable `resize-minibuffer-window-exactly' determines whether the
minibuffer window should ever be shrunk to make it no larger than needed to
display its contents.

When using a window system, it is possible for a minibuffer to tbe the sole
window in a frame.  Since that window is already its maximum size, the only
way to make more text visible at once is to increase the size of the frame.
The variable `resize-minibuffer-frame' controls whether this should be
done.  The variables `resize-minibuffer-frame-max-height' and
`resize-minibuffer-frame-exactly' are analogous to their window
counterparts."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond
   ((>= prefix 0)
    (setq resize-minibuffer-mode t))
   (t
    (setq resize-minibuffer-mode nil))))

;;; Glue code to make things work in both FSF and Lucid Emacsen.
(if (string-match "Lucid" emacs-version)
    (progn
      (fset 'resize-frame-parameters 'screen-parameters)
      (fset 'resize-frame-height 'screen-height)
      (fset 'resize-frame-width 'screen-width)
      (fset 'resize-selected-frame 'selected-screen)
      (fset 'resize-set-frame-size 'set-screen-size)
      (defun resize-minibuffer-frame-alist ()
	"Return the frame alist for the minibuffer."
	minibuffer-alist))
  (fset 'resize-frame-parameters 'frame-parameters)
  (fset 'resize-frame-height 'frame-height)
  (fset 'resize-frame-width 'frame-width)
  (fset 'resize-selected-frame 'selected-frame)
  (fset 'resize-set-frame-size 'set-frame-size)
  (defun resize-minibuffer-frame-alist ()
    "Return the frame alist for the minibuffer."
    minibuffer-frame-alist))

(defun resize-minibuffer-setup ()
  (cond
   (resize-minibuffer-mode
    (cond
     ((and (not (eq 'tty (console-type)))
	   (eq 'only (cdr (assq 'minibuffer (resize-frame-parameters)))))
      (and resize-minibuffer-frame
	   (progn
	     (make-local-hook 'minibuffer-exit-hook)
	     (add-hook 'minibuffer-exit-hook 'resize-minibuffer-frame-restore
		       nil t)
	     (make-local-hook 'post-command-hook)
	     (add-hook 'post-command-hook 'resize-minibuffer-frame nil t))))
     (t
      (make-local-hook 'post-command-hook)
      (add-hook 'post-command-hook 'resize-minibuffer-window nil t))))))

(defun resize-minibuffer-count-window-lines (&optional start end)
  "Return number of window lines occupied by text in region.
The number of window lines may be greater than the number of actual lines
in the buffer if any wrap on the display due to their length.

Optional arguments START and END default to point-min and point-max,
respectively."
  (or start (setq start (point-min)))
  (or end   (setq end   (point-max)))
  (if (= start end)
      0
    (save-excursion
      (save-restriction
        (widen)
	(narrow-to-region start end)
	(goto-char start)
        (vertical-motion (buffer-size))))))

;; Why isn't `min' a subr?
;; Do not pretend this is a real definition of `min'.  It suffices for this
;; packages's purposes (and is reasonably fast for a lisp call) but a real
;; min function would be able to take more than 2 arguments.
(defun resize-minibuffer-min (x y)
  "Return the lesser of X or Y."
  (if (< x y)
      x
    y))


;; Resize the minibuffer window to contain the minibuffer's contents.
;; The minibuffer must be the current window.
(defun resize-minibuffer-window ()
  (let ((height (window-height))
	(lines (1+ (resize-minibuffer-count-window-lines))))
    (and (numberp resize-minibuffer-window-max-height)
	 (> resize-minibuffer-window-max-height 0)
	 (setq lines (resize-minibuffer-min
		      lines
		      resize-minibuffer-window-max-height)))
    (or (if resize-minibuffer-window-exactly
	    (= lines height)
	  (<= lines height))
	(enlarge-window (- lines height)))))


;; Resize the minibuffer frame to contain the minibuffer's contents.
;; The minibuffer frame must be the current frame.
(defun resize-minibuffer-frame ()
  (let ((height (resize-frame-height))
	(lines (1+ (resize-minibuffer-count-window-lines))))
    (and (numberp resize-minibuffer-frame-max-height)
	 (> resize-minibuffer-frame-max-height 0)
	 (setq lines (resize-minibuffer-min
		      lines
		      resize-minibuffer-frame-max-height)))
    (cond
     ((> lines height)
      (resize-set-frame-size (resize-selected-frame)
			     (resize-frame-width)
			     lines))
     ((and resize-minibuffer-frame-exactly
	   (> height (cdr (assq 'height (resize-minibuffer-frame-alist))))
	   (< lines height))
      (resize-set-frame-size (resize-selected-frame)
			     (resize-frame-width)
			     lines)))))

;; Restore the original height of the frame.
(defun resize-minibuffer-frame-restore ()
  (resize-set-frame-size (resize-selected-frame)
			 (resize-frame-width)
			 (cdr (assq 'height (resize-minibuffer-frame-alist)))))


(provide 'rsz-minibuf)

(add-hook 'minibuffer-setup-hook 'resize-minibuffer-setup)

;; rsz-minibuf.el ends here
