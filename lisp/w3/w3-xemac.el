;;; w3-xemac.el --- XEmacs specific functions for emacs-w3
;; Author: wmperry
;; Created: 1997/04/01 19:23:21
;; Version: 1.17
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

(require 'w3-imap)
(require 'images)
(require 'w3-widget)
(require 'w3-menu)
(require 'w3-forms)
(require 'w3-script)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For XEmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (event-point e))
	 (good (eq (event-window e) (selected-window)))
	 (mouse-events))
    (if (not (and good pt (number-or-marker-p pt)))
	nil
      (if (and inhibit-help-echo w3-track-mouse)
	  (widget-echo-help pt))
      (setq mouse-events (w3-script-find-event-handlers pt 'mouse))
      (if (assq 'onmouseover mouse-events)
	  (w3-script-evaluate-form (cdr (assq 'onmouseover mouse-events)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-setup-version-specifics ()
  "Set up routine for XEmacs 19.12 or later"
  ;; Create the toolbar buttons
  (and (featurep 'toolbar)
       (w3-toolbar-make-buttons))

  ;; Register the default set of image conversion utilities
  (image-register-netpbm-utilities)

  ;; Add our menus, but make sure that we do it to the global menubar
  ;; not the current one, which could be anything, but usually GNUS or
  ;; VM if not the default.
  (if (featurep 'menubar)
      (let ((current-menubar (default-value 'current-menubar)))
	(if current-menubar
	    (add-submenu '("Help") (cons "WWW" (cdr w3-menu-help-menu))))))

  ;; FIXME FIXME: Do sexy things to the default modeline for Emacs-W3
  
  ;; The following is a workaround for XEmacs 19.14 and XEmacs 20.0
  ;; The text property implementation is badly broken - you could not have
  ;; a text property with a `nil' value.  Bad bad bad.
  (if (or (and (= emacs-major-version 20)
	       (= emacs-minor-version 0))
	  (and (= emacs-major-version 19)
	       (= emacs-minor-version 14)))
      (defun text-prop-extent-paste-function (ext from to)
	(let ((prop (extent-property ext 'text-prop nil))
	      (val nil))
	  (if (null prop)
	      (error "Internal error: no text-prop"))
	  (setq val (extent-property ext prop nil))
	  (put-text-property from to prop val nil)
	  nil))
    )
  )

(defun w3-store-in-clipboard (str)
  "Store string STR into the clipboard in X"
  (cond
   ((eq (device-type) 'tty)
    nil)
   ((eq (device-type) 'x)
    (x-own-selection str))
   ((eq (device-type) 'ns)
    )
   (t nil)))

(defun w3-mode-motion-hook (e)
  (let* ((glyph  (event-glyph e))
	 (x      (and glyph (event-glyph-x-pixel e)))
	 (y      (and glyph (event-glyph-y-pixel e)))
	 (widget (and glyph (glyph-property glyph 'widget)))
	 (usemap (and widget (w3-image-widget-usemap widget)))
	 (ismap  (and widget (widget-get widget 'ismap)))
	 (echo   (and widget (widget-get widget 'href))))
    (cond
     (usemap
      (setq echo (w3-point-in-map (vector x y) usemap t)))
     (ismap
      (setq echo (format "%s?%d,%d" echo x y)))
     (t
      nil))
    (and echo (message "%s" echo))))

(defun w3-mode-version-specifics ()
  "XEmacs specific stuff for w3-mode"
  (if (featurep 'mouse)
      (progn
	(if (not w3-track-mouse)
	    (setq inhibit-help-echo nil))
	(setq mode-motion-hook 'w3-mouse-handler)))
  (case (device-type)
    ((tty stream)			; TTY or batch
     nil)
    (otherwise
     (w3-add-toolbar-to-buffer)))
  (setq mode-popup-menu w3-popup-menu))

(require 'w3-toolbar)
(provide 'w3-xemacs)
(provide 'w3-xemac)
