;;; mwheel.el --- Mouse support for MS intelli-mouse type mice

;; Copyright (C) 1998, Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@cs.indiana.edu>
;; Keywords: mouse

;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not synched.

;;; Commentary:

;; This code will enable the use of the infamous 'wheel' on the new
;; crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
;; events are sent as button4/button5 events.

;; I for one would prefer some way of converting the button4/button5
;; events into different event types, like 'mwheel-up' or
;; 'mwheel-down', but I cannot find a way to do this very easily (or
;; portably), so for now I just live with it.

;; To enable this code, simply put this at the top of your .emacs
;; file:
;;
;; (autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
;; (mwheel-install)

;;; Code:

(require 'custom)
(require 'cl)

(globally-declare-fboundp
 '(event-basic-type
   posn-window event-start mwheel-event-window mwheel-event-button))

(defcustom mwheel-scroll-amount '(5 1 nil)
  "Amount to scroll windows by when spinning the mouse wheel.
A list with 3 elements specifying the amount to scroll on: a normal wheel
event, a wheel event with the shift key pressed, and a wheel event with the
control key pressed, in that order.

Each item should be the number of lines to scroll, or `nil' for near
full screen.
A near full screen is `next-screen-context-lines' less than a full screen."
  :group 'mouse
  :type '(list
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "Shifted"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))
	  (choice :tag "Controlled"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines"))))

(defcustom mwheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

(if (not (fboundp 'event-button))
    (defun mwheel-event-button (event)
      (let ((x (symbol-name (event-basic-type event))))
	(if (not (string-match "^mouse-\\([0-9]+\\)" x))
	    (error "Not a button event: %S" event))
	(string-to-int (substring x (match-beginning 1) (match-end 1)))))
  (fset 'mwheel-event-button 'event-button))

(if (not (fboundp 'event-window))
    (defun mwheel-event-window (event)
      (posn-window (event-start event)))
  (fset 'mwheel-event-window 'event-window))

(defun mwheel-scroll (event)
  (interactive "e")
  (let ((curwin (if mwheel-follow-mouse
		    (prog1
			(selected-window)
		      (select-window (mwheel-event-window event)))))
	(amt (if (memq 'shift (event-modifiers event))
		 (cadr mwheel-scroll-amount)
	       (if (memq 'control (event-modifiers event))
		   (caddr mwheel-scroll-amount)
		 (car mwheel-scroll-amount)))))
    (unwind-protect
	(case (mwheel-event-button event)
	  (4 (scroll-down amt))
	  (5 (scroll-up amt))
	  (otherwise (error "Bad binding in mwheel-scroll")))
      (if curwin (select-window curwin)))
    ))

;;;###autoload
(defun mwheel-install ()
  "Enable mouse wheel support."
  (interactive)
  (let ((keys '([(mouse-4)] [(shift mouse-4)] [(control mouse-4)]
		[(mouse-5)] [(shift mouse-5)] [(control mouse-5)])))

    ;; This condition-case is here because Emacs 19 will throw an error
    ;; if you try to define a key that it does not know about.  I for one
    ;; prefer to just unconditionally do a mwheel-install in my .emacs, so
    ;; that if the wheeled-mouse is there, it just works, and this way it
    ;; doesn't yell at me if I'm on my laptop or another machine, etc.
    (condition-case ()
	(while keys
	  (define-key global-map (car keys) 'mwheel-scroll)
	  (setq keys (cdr keys)))
      (error nil))))

;;;###autoload
(define-behavior 'mwheel
  "This code enables the use of the infamous 'wheel' on the new
crop of mice.  Under XFree86 and the XSuSE X Servers, the wheel
events are sent as button4/button5 events, which are automatically
set up to do scrolling in the expected way.  The actual way that the
scrolling works can be controlled by `mwheel-scroll-amount' and
`mwheel-follow-mouse'."
  :group 'mouse
  :short-doc "Mouse wheel support for X Windows"
  :enable 'mwheel-install)

(provide 'mwheel)

;;; mwheel.el ends here
