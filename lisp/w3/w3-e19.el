;;; w3-e19.el --- Emacs 19.xx specific functions for emacs-w3
;; Author: wmperry
;; Created: 1996/07/11 04:49:02
;; Version: 1.3
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry (wmperry@cs.indiana.edu)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For Emacs 19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-forms)
(require 'font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-links-menu nil "Menu for w3-mode in emacs 19.")
(make-variable-buffer-local 'w3-links-menu)

(defun w3-add-hotlist-menu ()
  ;; Add the hotlist menu to this buffer - used when it changes.
  (let ((hot-menu (make-sparse-keymap "w3-hotlist"))
	(ctr 0)
	(hot w3-hotlist))
    (while hot
      (define-key hot-menu (vector (intern (concat "w3-hotlist-"
						   (int-to-string ctr))))
	(cons (car (car hot))
	      (list 'lambda () '(interactive)
		    (list 'w3-fetch (car (cdr (car hot)))))))
      (setq ctr (1+ ctr)
	    hot (cdr hot)))
    (setq w3-e19-hotlist-menu hot-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-e19-show-hotlist-menu (e)
  (interactive "e")
  (if w3-html-bookmarks
      (popup-menu w3-html-bookmarks)
    (let* ((x (condition-case ()
		  (x-popup-menu e w3-e19-hotlist-menu)
		(error nil)))		; to trap for empty menus
	   (y (and x (lookup-key w3-e19-hotlist-menu (apply 'vector x)))))
      (if (and x y)
	  (funcall y)))))

(defun w3-e19-show-links-menu (e)
  (interactive "e")
  (if (not w3-e19-links-menu)
      (w3-build-FSF19-menu))
  (let* ((x (condition-case ()
		(x-popup-menu e w3-e19-links-menu)
	      (error nil)))		; to trap for empty menus
	 (y (and x (lookup-key w3-e19-links-menu (apply 'vector x)))))
    (if (and x y)
	(funcall y))))

(defun w3-build-FSF19-menu ()
  ;; Build emacs19 menus from w3-links-list
  (let* ((ctr 0)
	 (menu-ctr 0)
	 (tmp nil)
	 (widgets (w3-only-links))
	 (widget nil)
	 (href nil)
	 (menus nil))
     (setq tmp (make-sparse-keymap "Links"))
     (while widgets
       (setq widget (car widgets)
	     widgets (cdr widgets)
	     href (widget-get widget 'href))
       (if (> ctr w3-max-menu-length)
	   (setq menus (cons tmp menus)
		 ctr 0
		 tmp (make-sparse-keymap
		      (concat "Links" (int-to-string
				       (setq menu-ctr
					     (1+ menu-ctr)))))))
       (let ((ttl (w3-fix-spaces
		   (buffer-substring
		    (widget-get widget :from)
		    (widget-get widget :to))))
	     (key (vector (intern (concat "link"
					  (int-to-string
					   (setq ctr (1+ ctr))))))))
	 (if (and (> (length ttl) 0) href)
	     (define-key tmp key 
	       (cons ttl
		     (list 'lambda () '(interactive)
			   (list 'w3-fetch href)))))))
     (if (not menus)
	 (setq w3-e19-links-menu tmp)
       (setq w3-e19-links-menu (make-sparse-keymap "LinkMenu")
	     menus (nreverse (cons tmp menus))
	     ctr 0)
       (while menus
	 (define-key w3-e19-links-menu
	   (vector (intern (concat "SubMenu" ctr)))
	   (cons "More..." (car menus)))
	 (setq menus (cdr menus)
	       ctr (1+ ctr))))))

(defun w3-setup-version-specifics ()
  ;; Set up routine for emacs 19
  (require 'lmenu))

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
  (if w3-track-mouse (setq track-mouse t))
  (if (or (memq (device-type) '(x pm ns)))
      (w3-build-FSF19-menu)))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (posn-point (event-start e)))
	 (good (eq (posn-window (event-start e)) (selected-window)))
	 (widget (and good pt (number-or-marker-p pt) (widget-at pt)))
	 (link (and widget (widget-get widget 'href)))
	 (form (and widget (widget-get widget 'w3-form-data)))
	 (imag nil) ; (nth 1 (memq 'w3graphic props))))
	 )
    (cond
     (link (message "%s" link))
     (form
      (cond
       ((eq 'submit (w3-form-element-type form))
	(message "Submit form to %s"
		 (cdr-safe (assq 'action (w3-form-element-action form)))))
       ((eq 'reset (w3-form-element-type form))
	(message "Reset form contents"))
       (t
	(message "Form entry (name=%s, type=%s)" (w3-form-element-name form)
		 (w3-form-element-type form)))))
     (imag (message "Inlined image (%s)" (car imag)))
     (t (message "")))))

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
