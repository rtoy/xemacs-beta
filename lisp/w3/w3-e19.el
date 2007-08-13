;;; w3-e19.el --- Emacs 19.xx specific functions for emacs-w3
;; Author: wmperry
;; Created: 1997/02/18 23:32:51
;; Version: 1.18
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
(require 'w3-forms)
(require 'font)
(require 'w3-script)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-e19-hotlist-menu nil "A menu for hotlists.")
(defvar w3-e19-links-menu nil "A buffer-local menu for hyperlinks.")
(defvar w3-e19-nav-menu nil "A buffer-local menu for html based <link> tags.")
(mapcar 'make-variable-buffer-local
	'(w3-e19-hotlist-menu w3-e19-links-menu w3-e19-nav-menu))

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
  "Store string STR in the Xwindows clipboard"
  (cond
   ((memq (device-type) '(x pm))
    (x-select-text str))
   ((eq (device-type) 'ns)
    (ns-store-pasteboard-internal str))
   (t nil)))

(defun w3-mode-version-specifics ()
  ;; Emacs 19 specific stuff for w3-mode
  (make-local-variable 'track-mouse)
  (if w3-track-mouse (setq track-mouse t)))

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

(defun w3-color-values (color)
  (cond
   ((eq window-system 'x)
    (x-color-values color))
   ((eq window-system 'pm)
    (pm-color-values color))
   ((eq window-system 'ns)
    (ns-color-values color))
   (t nil)))
     
(defun w3-color-light-p (color-or-face)
  (let (colors)
    (cond
     ((null window-system)
      nil)
     ((facep color-or-face)
      (setq color-or-face (face-background color-or-face))
      (if (null color-or-face)
	  (setq color-or-face (cdr-safe
			       (assq 'background-color (frame-parameters)))))
      (setq colors (w3-color-values color-or-face)))
     ((stringp color-or-face)
      (setq colors (w3-color-values color-or-face)))
     ((font-rgb-color-p color-or-face)
      (setq colors (list (font-rgb-color-red color-or-face)
			 (font-rgb-color-green color-or-face)
			 (font-rgb-color-blue color-or-face))))
     (t
      (signal 'wrong-type-argument 'color-or-face-p)))
    (not (< (apply '+ colors)
	    (/ (apply '+ (w3-color-values "white")) 3)))))



(provide 'w3-emacs19)
(provide 'w3-e19)
