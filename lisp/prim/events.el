;;; events.el --- event functions.

;;;; Copyright (C) 1996 Ben Wing.

;; Maintainer:
;; Keywords: internal

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

;;; Synched up with: Not in FSF.

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
  "Given a mouse-motion, button-press, or button-release event, return
the buffer on which that event occurred.  This will be nil for non-mouse
events.  If event-over-text-area-p is nil, this will also be nil."
  (let ((window (event-window event)))
    (and (windowp window) (window-buffer window))))

(defalias 'allocate-event 'make-event)

(defun key-press-event-p (obj)
  "True if OBJ is a key-press event object."
  (and (event-live-p obj) (eq 'key-press (event-type obj))))

(defun button-press-event-p (obj)
  "True if OBJ is a mouse-button-press event object."
  (and (event-live-p obj) (eq 'button-press (event-type obj))))

(defun button-release-event-p (obj)
  "True if OBJ is a mouse-button-release event object."
  (and (event-live-p obj) (eq 'button-release (event-type obj))))

(defun button-event-p (obj)
  "True if OBJ is a button-press or button-release event object."
  (or (button-press-event-p obj) (button-release-event-p obj)))

(defun motion-event-p (obj)
  "True if OBJ is a mouse-motion event object."
  (and (event-live-p obj) (eq 'motion (event-type obj))))

(defun mouse-event-p (obj)
 "True if OBJ is a button-press, button-release, or mouse-motion event object."
  (or (button-event-p obj) (motion-event-p obj)))

(defun process-event-p (obj)
  "True if OBJ is a process-output event object."
  (and (event-live-p obj) (eq 'process (event-type obj))))

(defun timeout-event-p (obj)
  "True if OBJ is a timeout event object."
  (and (event-live-p obj) (eq 'timeout (event-type obj))))

(defun eval-event-p (obj)
  "True if OBJ is an eval event object."
  (and (event-live-p obj) (eq 'eval (event-type obj))))

(defun misc-user-event-p (obj)
  "True if OBJ is a misc-user event object.
A misc-user event is a user event that is not a keypress or mouse click;
normally this means a menu selection or scrollbar action."
  (and (event-live-p obj) (eq 'misc-user (event-type obj))))

;; You could just as easily use event-glyph but we include this for
;; consistency.

(defun event-over-glyph-p (event)
  "Given a mouse-motion, button-press, or button-release event, return
t if the event is over a glyph.  Otherwise, return nil."
  (not (null (event-glyph event))))

(defun keyboard-translate (&rest pairs)
  "Translate character or keysym FROM to TO at a low level.
Multiple FROM-TO pairs may be specified.

See `keyboard-translate-table' for more information."
  (while pairs
    (puthash (car pairs) (car (cdr pairs)) keyboard-translate-table)
    (setq pairs (cdr (cdr pairs)))))

(put 'backspace 'ascii-character ?\b)
(put 'delete    'ascii-character ?\177)
(put 'tab       'ascii-character ?\t)
(put 'linefeed  'ascii-character ?\n)
(put 'clear     'ascii-character 12)
(put 'return    'ascii-character ?\r)
(put 'escape    'ascii-character ?\e)
(put 'space	'ascii-character ? )

 ;; Do the same voodoo for the keypad keys.  I used to bind these to keyboard
 ;; macros (for instance, kp-0 was bound to "0") so that they would track the
 ;; bindings of the corresponding keys by default, but that made the display
 ;; of M-x describe-bindings much harder to read, so now we'll just bind them
 ;; to self-insert by default.  Not a big difference...
 
(put 'kp-0 'ascii-character ?0)
(put 'kp-1 'ascii-character ?1)
(put 'kp-2 'ascii-character ?2)
(put 'kp-3 'ascii-character ?3)
(put 'kp-4 'ascii-character ?4)
(put 'kp-5 'ascii-character ?5)
(put 'kp-6 'ascii-character ?6)
(put 'kp-7 'ascii-character ?7)
(put 'kp-8 'ascii-character ?8)
(put 'kp-9 'ascii-character ?9)

(put 'kp-space     'ascii-character ? )
(put 'kp-tab       'ascii-character ?\t)
(put 'kp-enter     'ascii-character ?\r)
(put 'kp-equal     'ascii-character ?=)
(put 'kp-multiply  'ascii-character ?*)
(put 'kp-add       'ascii-character ?+)
(put 'kp-separator 'ascii-character ?,)
(put 'kp-subtract  'ascii-character ?-)
(put 'kp-decimal   'ascii-character ?.)
(put 'kp-divide    'ascii-character ?/)
