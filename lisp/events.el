;;; events.el --- event functions for XEmacs.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996-7 Sun Microsystems, Inc.
;; Copyright (C) 1996 Ben Wing.

;; Maintainer: Martin Buchholz
;; Keywords: internal, event, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:


(defun event-console (event)
  "Return the console that EVENT occurred on.
This will be nil for some types of events (e.g. eval events)."
  (cdfw-console (event-channel event)))

(defun event-device (event)
  "Return the device that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
  (dfw-device (event-channel event)))

(defun event-frame (event)
  "Return the frame that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
  (fw-frame (event-channel event)))

(defun event-buffer (event)
  "Return the buffer of the window over which mouse event EVENT occurred.
Return nil unless both (mouse-event-p EVENT) and
(event-over-text-area-p EVENT) are non-nil."
  (let ((window (event-window event)))
    (and (windowp window) (window-buffer window))))

(defalias 'allocate-event 'make-event)


(defun key-press-event-p (object)
  "Return t if OBJECT is a key-press event."
  (and (event-live-p object) (eq 'key-press (event-type object))))

(defun button-press-event-p (object)
  "Return t if OBJECT is a mouse button-press event."
  (and (event-live-p object) (eq 'button-press (event-type object))))

(defun button-release-event-p (object)
  "Return t if OBJECT is a mouse button-release event."
  (and (event-live-p object) (eq 'button-release (event-type object))))

(defun button-event-p (object)
  "Return t if OBJECT is a mouse button-press or button-release event."
  (and (event-live-p object)
       (memq (event-type object) '(button-press button-release))
       t))

(defun motion-event-p (object)
  "Return t if OBJECT is a mouse motion event."
  (and (event-live-p object) (eq 'motion (event-type object))))

(defun mouse-event-p (object)
  "Return t if OBJECT is a mouse button-press, button-release or motion event."
  (and (event-live-p object)
       (memq (event-type object) '(button-press button-release motion))
       t))

(defun process-event-p (object)
  "Return t if OBJECT is a process-output event."
  (and (event-live-p object) (eq 'process (event-type object))))

(defun timeout-event-p (object)
  "Return t if OBJECT is a timeout event."
  (and (event-live-p object) (eq 'timeout (event-type object))))

(defun eval-event-p (object)
  "Return t if OBJECT is an eval event."
  (and (event-live-p object) (eq 'eval (event-type object))))

(defun misc-user-event-p (object)
  "Return t if OBJECT is a misc-user event.
A misc-user event is a user event that is not a keypress or mouse click;
normally this means a menu selection or scrollbar action."
  (and (event-live-p object) (eq 'misc-user (event-type object))))

;; You could just as easily use event-glyph but we include this for
;; consistency.

(defun event-over-glyph-p (object)
  "Return t if OBJECT is a mouse event occurring over a glyph.
Mouse events are events of type button-press, button-release or motion."
  (and (event-live-p object) (event-glyph object) t))

(defun keyboard-translate (&rest pairs)
  "Translate character or keysym FROM to TO at a low level.
Multiple FROM-TO pairs may be specified.

See `keyboard-translate-table' for more information."
  (while pairs
    (puthash (pop pairs) (pop pairs) keyboard-translate-table)))

(defun set-character-of-keysym (keysym character)
  "Make CHARACTER be inserted when KEYSYM is pressed, 
and the key has been bound to `self-insert-command'.  "
  (check-argument-type 'symbolp keysym) 
  (check-argument-type 'characterp character)
  (put keysym 'character-of-keysym character))

(defun get-character-of-keysym (keysym)
  "Return the character inserted when KEYSYM is pressed, 
and the key is bound to `self-insert-command'.  "
  (check-argument-type 'symbolp keysym)
  (event-to-character (make-event 'key-press (list 'key keysym))))

;; We could take the first few of these out by removing the "/* Optimize for
;; ASCII keysyms */" code in event-Xt.c, and I've a suspicion that may be
;; the right thing to do anyway.

(loop for (keysym char) in
  '((tab ?\t)
    (linefeed ?\n)
    (clear ?\014)
    (return ?\r)
    (escape ?\e)
    (space ? )

    ;; Do the same voodoo for the keypad keys.  I used to bind these to
    ;; keyboard macros (for instance, kp-0 was bound to "0") so that they
    ;; would track the bindings of the corresponding keys by default, but
    ;; that made the display of M-x describe-bindings much harder to read,
    ;; so now we'll just bind them to self-insert by default.  Not a big
    ;; difference...

    (kp-0 ?0)
    (kp-1 ?1)
    (kp-2 ?2)
    (kp-3 ?3)
    (kp-4 ?4)
    (kp-5 ?5)
    (kp-6 ?6)
    (kp-7 ?7)
    (kp-8 ?8)
    (kp-9 ?9)

    (kp-space ? )
    (kp-tab ?\t)
    (kp-enter ?\r)
    (kp-equal ?=)
    (kp-multiply ?*)
    (kp-add ?+)
    (kp-separator ?,)
    (kp-subtract ?-)
    (kp-decimal ?.)
    (kp-divide ?/))
  do (set-character-of-keysym keysym char))

;;; events.el ends here
