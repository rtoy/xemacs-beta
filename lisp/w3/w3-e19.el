;;; w3-e19.el --- Emacs 19.xx specific functions for emacs-w3
;; Author: wmperry
;; Created: 1997/07/08 14:00:33
;; Version: 1.28
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
;;; Copyright (c) 1996, 1997 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For Emacs 19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'w3-props))
(require 'w3-forms)
(require 'font)
(require 'w3-script)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-e19-hotlist-menu nil "A menu for hotlists.")
(defvar w3-e19-links-menu nil "A buffer-local menu for hyperlinks.")
(defvar w3-e19-nav-menu nil "A buffer-local menu for html based <link> tags.")
(defvar w3-e19-window-width nil)

(mapcar 'make-variable-buffer-local
	'(w3-e19-hotlist-menu
	  w3-e19-window-width
	  w3-e19-links-menu
	  w3-e19-nav-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-e19-show-hotlist-menu ()
  (interactive)
  (let ((keymap (easy-menu-create-keymaps "Hotlist"
					  (w3-menu-hotlist-constructor nil)))
	(x nil)
	(y nil))
    (setq x (x-popup-menu t keymap)
	  y (and x (lookup-key keymap (apply 'vector x))))
    (if (and x y)
	(funcall y))))

(defun w3-e19-show-links-menu ()
  (interactive)
  (if (not w3-e19-links-menu)
      (w3-build-FSF19-menu))
  (let (x y)
    (setq x (x-popup-menu t w3-e19-links-menu)
	  y (and x (lookup-key w3-e19-links-menu (apply 'vector x))))
    (if (and x y)
	(funcall y))))

(defun w3-e19-show-navigate-menu ()
  (interactive)
  (if (not w3-e19-nav-menu)
      (w3-build-FSF19-menu))
  (let (x y)
    (setq x (x-popup-menu t w3-e19-nav-menu)
	  y (and x (lookup-key w3-e19-nav-menu (apply 'vector x))))
    (if (and x y)
	(funcall y))))

(defun w3-build-FSF19-menu ()
  ;; Build emacs19 menus from w3-links-list
  (let ((links (w3-menu-html-links-constructor nil))
	(hlink (w3-menu-links-constructor nil)))
    (setq w3-e19-nav-menu (easy-menu-create-keymaps "Navigate" links)
	  w3-e19-links-menu (easy-menu-create-keymaps "Links" hlink))))

(defun w3-setup-version-specifics ()
  ;; Set up routine for emacs 19
  (require 'lmenu) ; for popup-menu
  )

(defun w3-store-in-clipboard (str)
  "Store string STR in the system clipboard"
  (cond
   ((boundp 'interprogram-cut-function)
    (if interprogram-cut-function
	(funcall interprogram-cut-function str t)))
   (t
    (case (device-type)
      (x (x-select-text str))
      (pm (pm-put-clipboard str))
      (ns (ns-store-pasteboard-internal str))
      (otherwise nil)))))

(defun w3-e19-no-read-only (st nd)
  ;; Make sure we don't yank any read-only data out of this buffer
  (let ((inhibit-read-only t)
	(after-change-functions nil)
	(after-change-function nil))
    (put-text-property st nd 'w3-munged-ro t)
    (put-text-property st nd 'read-only nil)))

(defun w3-mode-version-specifics ()
  ;; Emacs 19 specific stuff for w3-mode
  (declare (special w3-face-index w3-display-background-properties))
  (make-local-variable 'track-mouse)
  (set (make-local-variable 'buffer-access-fontify-functions) 'w3-e19-no-read-only)
  (set (make-local-variable 'buffer-access-fontified-property) 'w3-munged-ro)
  (setq w3-e19-window-width (window-width))
  (if w3-track-mouse (setq track-mouse t))
  (if w3-display-background-properties
      (let ((face (w3-make-face (intern
				 (format "w3-style-face-%05d" w3-face-index))
				"An Emacs-W3 face... don't edit by hand." t))
	    (fore (car w3-display-background-properties))
	    (inhibit-read-only t)
	    (back (cdr w3-display-background-properties)))
	(setq w3-face-index (1+ w3-face-index))
	(if fore (font-set-face-foreground face fore))
	(if back (font-set-face-background face back))
	(fillin-text-property (point-min) (point-max) 'face 'face face))))

(defun w3-text-pixel-width (str &optional face)
  "Return the pixel-width of a chunk of text STR with face FACE."
  (* (length str) (frame-char-width)))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (posn-point (event-start e)))
	 (good (eq (posn-window (event-start e)) (selected-window)))
	 (mouse-events nil))
    (if (not (and good pt (number-or-marker-p pt)))
	nil
      (widget-echo-help pt)
      ;; Need to handle onmouseover, on mouseout
      (setq mouse-events (w3-script-find-event-handlers pt 'mouse))
      (if (assq 'onmouseover mouse-events)
	  (w3-script-evaluate-form (cdr (assq 'onmouseover mouse-events)))))))

(defun w3-window-size-change-function (frame)
  (let ((first (frame-first-window frame))
	(cur nil))
    (while (not (eq cur first))
      (setq cur (if cur (next-window cur nil frame) first))
      (save-excursion
	(set-buffer (window-buffer cur))
	(if (and (eq major-mode 'w3-mode)
		 (not (eq (window-width cur) w3-e19-window-width)))
	    (w3-refresh-buffer))))))


(provide 'w3-emacs19)
(provide 'w3-e19)
