;;; w3-xemac.el,v --- XEmacs specific functions for emacs-w3
;; Author: wmperry
;; Created: 1996/06/06 14:14:34
;; Version: 1.165
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3-imap)
(require 'images)
(require 'w3-widget)
(require 'w3-menu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For XEmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (let* ((pt (event-point e))
	 (props (and pt (extent-properties-at pt)))
	 (link (nth 1 (nth 1 (memq 'w3 props)))) ; The link info if it exists
	 (form (nth 1 (memq 'w3form props))) 	 ; The form info it it exists
	 (dely (nth 0 (nth 1 (memq 'w3delayed props))))	 ; The delayed img info
	 (mpeg (nth 1 (memq 'w3mpeg props)))     ; the delayed mpeg info
	 (imag (nth 1 (memq 'w3graphic props)))) ; The image info if it exists
    (cond
     (link (message "%s" link))
     (form
      (let ((args (nth 0 form)))
	(cond
	 ((string= "SUBMIT" (nth 1 form))
	  (message "Submit form to %s" (cdr-safe (assq 'action args))))
	 ((string= "RESET" (nth 1 form))
	  (message "Reset form contents"))
	 (t
	  (message "Form entry (name=%s, type=%s)" (nth 2 form)
		   (if (equal "" (nth 1 form))
		       "text"
		     (downcase (nth 1 form))))))))
     (dely (message "Delayed image (%s)" (car dely)))
     (imag (message "Inlined image (%s)" (car imag)))
     (mpeg (message "Delayed mpeg (%s)" (car mpeg)))
     (t (message "")))))

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

(defun w3-color-light-p (color-or-face)
  (let (face color)
    (cond
     ((or (facep color-or-face)
	  (and (symbolp color-or-face)
	       (find-face color-or-face)))
      (setq color (specifier-instance (face-background color-or-face))))
     ((color-instance-p color-or-face)
      (setq color color-or-face))
     ((color-specifier-p color-or-face)
      (setq color (specifier-instance color-or-face)))
     ((stringp color-or-face)
      (setq color (make-color-instance color-or-face)))
     (t (signal 'wrong-type-argument 'color-or-face-p)))
    (if color
	(not (< (apply '+ (color-instance-rgb-components color))
		(/ (apply '+ (color-instance-rgb-components
			      (make-color-instance "white"))) 3)))
      t)))

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
  (cond
   ((not w3-track-mouse)
    nil)
   ((or (not (boundp 'inhibit-help-echo))
	inhibit-help-echo)
    (setq mode-motion-hook 'w3-mouse-handler))
   (t nil))
  (if (eq (device-type) 'tty)
      nil
    (w3-add-toolbar-to-buffer))
  (setq mode-popup-menu w3-popup-menu))

(require 'w3-toolbar)
(provide 'w3-xemacs)
(provide 'w3-xemac)
