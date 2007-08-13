;;; blink-cursor.el --- Blink the cursor on or off

;; Copyright (C) 1996 Ben Wing.

;; Keywords: display

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

;;; Commentary:

;;; Code:

(defvar blink-cursor-last-selected-window nil)
(defvar blink-cursor-lost-focus nil)

(defun blink-cursor-callback (foo)
  (let ((inhibit-quit t)
	(window (selected-window)))
    (if blink-cursor-lost-focus
	nil
      (or blink-cursor-last-selected-window
	  (setq blink-cursor-last-selected-window window))
      (if (eq window blink-cursor-last-selected-window)
	  (set-specifier text-cursor-visible-p
			 (not (specifier-instance text-cursor-visible-p
						  window))
			 window)
	(remove-specifier text-cursor-visible-p
			  blink-cursor-last-selected-window)
	(setq blink-cursor-last-selected-window window)
	(set-specifier text-cursor-visible-p nil window)))))

(defun blink-cursor-reenable-cursor ()
  (if blink-cursor-last-selected-window
      (progn
	(remove-specifier text-cursor-visible-p
			  blink-cursor-last-selected-window)
	(setq blink-cursor-last-selected-window nil))))

(defun blink-cursor-deselect-frame-hook ()
  (blink-cursor-reenable-cursor)
  (setq blink-cursor-lost-focus t))

(defun blink-cursor-select-frame-hook ()
  (setq blink-cursor-lost-focus nil))

(add-hook 'deselect-frame-hook 'blink-cursor-deselect-frame-hook)
(add-hook 'select-frame-hook 'blink-cursor-select-frame-hook)

(defvar blink-cursor-timeout 1.0)
(defvar blink-cursor-timeout-id nil)
(defvar blink-cursor-mode nil)

;;;###autoload
(defun blink-cursor-mode (&optional timeout)
  "Enable or disable a blinking cursor.
If TIMEOUT is nil, toggle on or off.
If TIMEOUT is t, enable with the previous timeout value.
If TIMEOUT is 0, disable.
If TIMEOUT is greater than 0, then the cursor will blink once
each TIMEOUT secs (can be a float)."
  (interactive)
  (cond ((not timeout)
	 (setq timeout blink-cursor-timeout)
	 (setq blink-cursor-mode (not blink-cursor-mode)))
	((eq timeout t)
	 (setq timeout blink-cursor-timeout)
	 (setq blink-cursor-mode t))
	((<= timeout 0)
	 (setq blink-cursor-mode nil))
	(t
	 (setq blink-cursor-timeout timeout)
	 (setq blink-cursor-mode t)))
  (if blink-cursor-timeout-id
      (progn
	(disable-timeout blink-cursor-timeout-id)
	(blink-cursor-reenable-cursor)
	(setq blink-cursor-timeout-id nil)))
  (if blink-cursor-mode
      (setq blink-cursor-timeout-id
	    (add-timeout (/ (float timeout) 2) 'blink-cursor-callback nil
			 (/ (float timeout) 2)))))
