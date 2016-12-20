;;; mwheel.el --- Wheel mouse support

;; Copyright (C) 1998, 2000-2016 Free Software Foundation, Inc.
;; Maintainer: William M. Perry <wmperry@cs.indiana.edu>
;; Keywords: mouse
;; Package: emacs

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

;;; Synched up with: GNU's mwheel.el as of August 2016, last significant
;;; modification to it, 2012, f06e2758.

;;; Commentary:

;; This code enables the use of the mouse wheel.  Under XFree86 and the XSuSE
;; X Servers, the wheel events are sent as button4/button5 events, and XEmacs'
;; Windows code translates mouse wheel events to reflect this. Under the Apple
;; X11 server, however, mouse wheel events arrived as button6 and button7
;; events for a period of time, before reverting to the standard-for-X11
;; button4/button5 events.

;; Mouse wheel support is enabled by default under XEmacs by means of
;; autoloads. 

;;; Code:

(require 'custom)

;; XEmacs; I don't feel it's reasonable to support a minor mode for the mouse
;; wheel. It should just work, there is no need for mouse-wheel-mode-hook,
;; mouse-wheel-mode-on-hook, mouse-wheel-mode-off-hook and the custom
;; support.

;; Given our existing support for the mouse wheel as a behavior, and given
;; that I do accept that people can reasonably want to disable the mouse wheel
;; entirely, I have added a :disable function to the behavior to ease turning
;; it off, implemented in terms of an UNINSTALL argument to #'mwheel-install.

; (defvar mouse-wheel-mode)

;; XEmacs; don't make it particularly convenient to set the mouse wheel
;; button, its value is stable within XEmacs and any change should be made in
;; our source code, any user who needs it should get in touch with us.

; (defun mouse-wheel-change-button (var button)
;   ...)

; [...] 

;; GNU initially had the relatively sensible name "mouse-wheel-click-button"
;; for this, but moved to the following more confusing one with 22.1.
(defcustom mouse-wheel-click-event 'mouse-2
  "Button that should be temporarily inhibited after mouse scrolling.
The mouse wheel is typically on the mouse-2 button, so it may easily
happen that text is accidentally yanked into the buffer when
scrolling with the mouse wheel.  To prevent that, this variable can be
set to the button sent when clicking on the mouse wheel button.  Use the
syntax of `define-key' to specify the button in question.

XEmacs will error if `mouse-wheel-click-event' is set to a non-nil value which
does not correspond to a mouse button."
  :group 'mouse
  :type 'symbol)

(defcustom mouse-wheel-inhibit-click-time 0.35
  "Time in seconds to inhibit clicking on mouse wheel button after scroll."
  :group 'mouse
  :type 'number)

(defcustom mouse-wheel-scroll-amount '((() . 5) ((shift) . 1) ((control) . nil))
  "Amount to scroll windows by when spinning the mouse wheel.
This is an alist mapping the modifier keys to the amount to scroll when
the wheel is moved with the modifier keys depressed.
Elements of the list have the form (MODIFIERS . AMOUNT).  Each MODIFIERS list
must be in alphabetical order for it to match.

AMOUNT should be the number of lines to scroll, or nil for near full
screen.  It can also be a floating point number, specifying the fraction of
a full screen to scroll.  A near full screen is `next-screen-context-lines'
less than a full screen."
  :group 'mouse
  :type '(cons
	  (choice :tag "Normal"
		  (const :tag "Full screen" :value nil)
		  (integer :tag "Specific # of lines")
		  (float :tag "Fraction of window")
		  (cons
		   (repeat (choice :tag "modifier"
				   (const alt) (const control) (const hyper)
				   (const meta) (const shift) (const super)))
		   (choice :tag "scroll amount"
			   (const :tag "Full screen" :value nil)
			   (integer :tag "Specific # of lines")
			   (float :tag "Fraction of window"))))
          (repeat
           (cons
            (repeat (choice :tag "modifier"
			    (const alt) (const control) (const hyper)
                            (const meta) (const shift) (const super)))
            (choice :tag "scroll amount"
                    (const :tag "Full screen" :value nil)
                    (integer :tag "Specific # of lines")
                    (float :tag "Fraction of window"))))))
  ; :set 'mouse-wheel-change-button)

(defcustom mouse-wheel-progressive-speed t
  "If non-nil, the faster the user moves the wheel, the faster the scrolling.
Note that this has no effect when `mouse-wheel-scroll-amount' specifies
a \"near full screen\" scroll or when the mouse wheel sends key instead
of button events."
  :group 'mouse
  :type 'boolean)

(defcustom mouse-wheel-follow-mouse nil
  "Whether the mouse wheel should scroll the window that the mouse is over.
This can be slightly disconcerting, but some people may prefer it."
  :group 'mouse
  :type 'boolean)

;; XEmacs; use #'scroll-up-command, #'scroll-down-command here. It is
;; reasonable to allow them to be adjusted easily, there are some odd souls
;; out there who use mouse wheels in the unintuitive way.
(defvar mouse-wheel-scroll-up-function #'scroll-up-command
  "Function that does the job of scrolling upward.")

(defvar mouse-wheel-scroll-down-function #'scroll-down-command
 "Function that does the job of scrolling downward.")

;; XEmacs; make these symbol macros available within #'mouse-wheel-scroll and
;; the various event predicates below.
;; Apple X11 supplied 6 and 7 as the mouse wheel buttons for a few years, but
;; now have reverted to the traditional 4, 5.
(symbol-macrolet
    ((mouse-wheel-buttons '(4 6 ; down
                            5 7)) ; up
     (mouse-wheel-down-buttons '(4 6))
     (mouse-wheel-up-buttons '(5 7))
     ;; Make these vars really private to #'mouse-wheel-scroll:
     (mouse-wheel-last-event-button #:mouse-wheel-last-event-button)
     (mouse-wheel-timeout-id #:mouse-wheel-timeout-id))

;;;###autoload
  (defun mouse-wheel-scroll (event)
    "Scroll up or down depending on EVENT.

EVENT should reflect a mouse wheel scroll movement, upward or downward, and
`mouse-wheel-scroll' will error if it does not.

See also `mouse-wheel-scroll-amount', `mouse-wheel-progressive-speed', and
`mouse-wheel-follow-mouse', which modify the behavior of `mouse-wheel-scroll',
and `mouse-wheel-click-event', which specifies a mouse click to ignore for a
period of time after mouse wheel activity, which period is specified with
`mouse-wheel-inhibit-click-time', to avoid inadvertent clicks."
    (interactive "e")
    (let* ((window (if mouse-wheel-follow-mouse
                       (prog1
                           (selected-window)
                         (select-window (event-window event)))))
           (amount (cdr (assoc (sort (event-modifiers event) #'string-lessp)
                               mouse-wheel-scroll-amount)))
           (event-button (event-button event)))

      (defvar mouse-wheel-last-event-button nil)
      ; "Last wheel button clicked"
      (defvar mouse-wheel-timeout-id nil)
      ; "Timer running while mouse wheel click event is inhibited."
      (unless amount
        ;; Accept GNU's non-cons-is-no-modifiers syntax silently.
        (setq amount (find-if-not #'consp mouse-wheel-scroll-amount)))
      (if (floatp amount)
          (setq amount (1+ (truncate (* amount (window-height window))))))
      ;; XEmacs; implement mouse-wheel-progressive-speed in terms of
      ;; mouse-track's private variables, use our #'event-timestamp<.
      (when (and mouse-wheel-progressive-speed (numberp amount))
        (if (or
             (event-timestamp< (+ (prog1
                                      (or mouse-track-up-time
                                          most-negative-fixnum)
                                    (setq mouse-track-up-time
                                          (event-timestamp event)))
                                  mouse-track-multi-click-time)
                               ;; After the prog1 trickery above, this now
                               ;; reflects EVENT.
                               mouse-track-up-time)
             ;; If we change direction in the scrolling, slow down.
             (not (eql event-button (prog1 mouse-wheel-last-event-button
                                      (setq mouse-wheel-last-event-button
                                            event-button)))))
            (setq mouse-track-click-count 1)
          (setq mouse-track-click-count (1+ mouse-track-click-count)
                ;; When the double-mouse-N comes in, a mouse-N has been
                ;; executed already, So by adding things up we get a
                ;; squaring up (1, 3, 6, 10, 15, ...).
                amount (* amount mouse-track-click-count))))
      (unwind-protect
           (cond
             ((member* event-button mouse-wheel-down-buttons)
              (funcall mouse-wheel-scroll-down-function amount))
             ((member* event-button mouse-wheel-up-buttons)
              (funcall mouse-wheel-scroll-up-function amount))
             (t (error 'undefined-keystroke-sequence event)))
        (if window (select-window window)))
      ;; XEmacs; if we've moved point, keep the region active with
      ;; zmacs-regions. Don't save and examine point as GNU does.
      (setq zmacs-region-stays t)
      (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
        ;; XEmacs; make this couple of utility functions into labels, use our
        ;; native timeouts rather than the GNU-compatible timer library.
        (labels
            ((mwheel-filter-click-events ()
               ; "Discard `mouse-wheel-click-event' briefly after scrolling
               ; with the mouse wheel."
               (if (and current-mouse-event
                        (inline (button-event-p current-mouse-event))
                        ;; Sigh, no #'event-matches-key-specifier-p for mouse
                        ;; buttons.
                        (eql (event-button current-mouse-event)
                             (or
                              (cdr (assoc*
                                    mouse-wheel-click-event
                                    '((button2 . 2) (mouse-2 . 2) 
                                      (button1 . 1) (mouse-1 . 1)
                                      (button3 . 3) (mouse-3 . 3) 
                                      (button4 . 4) (mouse-4 . 4)
                                      (mouse-5 . 5) (button5 . 5))))
                              (let ((symbol-name (symbol-name
                                                  mouse-wheel-click-event)))
                                (cond
                                  ((eql (mismatch symbol-name "button")
                                        (length "button"))
                                   (parse-integer symbol-name
                                                  :start (length "button")))
                                  ((eql (mismatch symbol-name "mouse-")
                                        (length "mouse-"))
                                   (parse-integer symbol-name :start
                                                  (length "mouse-")))
                                  (t
                                   (lwarn 'gui 'error
                                     "Not a known mouse button: %s, `%s'"
                                     'mouse-wheel-click-event symbol-name)
                                   ;; Return some non-fixnum value from cond,
                                   ;; so #'eql gives nil.
                                   symbol-name))))))
                        (setq this-command 'ignore)))
             (mwheel-inhibit-click-timeout (function)
               ; "Remove the filter for click events"
               (remove-hook 'pre-command-hook function)))
          (add-hook 'pre-command-hook #'mwheel-filter-click-events)
          (prog1 ; Return nil from #'mouse-wheel-scroll.
              (if mouse-wheel-timeout-id
                  (disable-timeout mouse-wheel-timeout-id))
            (setq mouse-wheel-timeout-id
                  (add-timeout mouse-wheel-inhibit-click-time
                               #'mwheel-inhibit-click-timeout
                               #'mwheel-filter-click-events)))))))

  (defun mouse-wheel-event-p (object)
    "Return t if OBJECT is an event reflecting mouse wheel motion.
Make sure to `(require 'mwheel) before using this function."
    (and (event-live-p object)
         (member* (event-type object) '(button-press button-release))
         (member* (event-button object) mouse-wheel-buttons)
         t))

  (defun mouse-wheel-scroll-down-event-p (object)
    "Return t if OBJECT is an event reflecting downward mouse wheel motion.
Make sure to `(require 'mwheel) before using this function."
    (and (event-live-p object)
         (member* (event-type object) '(button-press button-release))
         (member* (event-button object) mouse-wheel-down-buttons)
         t))

  (defun mouse-wheel-scroll-up-event-p (object)
    "Return t if OBJECT is an event reflecting upward mouse wheel motion.
Make sure to `(require 'mwheel) before using this function."
    (and (event-live-p object)
         (member* (event-type object) '(button-press button-release))
         (member* (event-button object) mouse-wheel-up-buttons)
         t))

  (defun mwheel-install (&optional uninstall)
    "Enable mouse wheel support.  On by default.

With non-nil argument UNINSTALL, disable mouse wheel support."
    (interactive)
    (let ((list
           (mapcar #'(lambda (fixnum)
                       (intern (concat "button" (number-to-string fixnum)
                                       "up")))
                   mouse-wheel-buttons)))
      (if uninstall
          (dolist (elt list)
            (when (eq (lookup-key global-map elt) #'mouse-wheel-scroll)
              (define-key global-map elt nil))
            (dolist (acons mouse-wheel-scroll-amount)
              (when (and (car-safe acons)
                         (eq (lookup-key global-map `[(,@(car acons) ,elt)])
                             #'mouse-wheel-scroll))
                (define-key global-map `[(,@(car acons) ,elt)] nil))))
        (dolist (elt list)
          (define-key global-map elt #'mouse-wheel-scroll)
          (dolist (acons mouse-wheel-scroll-amount)
            (when (car-safe acons)
              (define-key global-map `[(,@(car acons) ,elt)]
                #'mouse-wheel-scroll))))))))

(provide 'mwheel)

;; XEmacs; our autoload code can't quite handle symbol-macrolet, above, make
;; these explicit.
;;;###autoload
(autoload 'mouse-wheel-scroll "mwheel" "\
Scroll up or down depending on EVENT.

EVENT should reflect a mouse wheel scroll, upward or downward, and
`mouse-wheel-scroll' will error if it does not.

See also `mouse-wheel-scroll-amount', `mouse-wheel-progressive-speed', and
`mouse-wheel-follow-mouse', which modify the behavior of `mouse-wheel-scroll',
and `mouse-wheel-click-event' which specifies a mouse click to ignore for a
period of time after mouse wheel activity, which period is specified with
`mouse-wheel-inhibit-click-time'.

arguments: (EVENT)
" t)

;;;###autoload
(autoload 'mwheel-install "mwheel" "\
Enable mouse wheel support.  On by default.

With argument UNINSTALL, disable mouse wheel support.

arguments: (&optional UNINSTALL)
" t) 

;;; mwheel.el ends here
