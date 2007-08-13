;;; x-init.el --- initialization code for X windows
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: various
;; Keywords: terminals

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

;;; If you want to change this variable, this is the place you must do it.
;;; Do not set it to a string containing periods.  X doesn't like that.
;(setq x-emacs-application-class "Emacs")

;;; selections and active regions

;;; If and only if zmacs-regions is true:
;;;
;;; When a mark is pushed and the region goes into the "active" state, we
;;; assert it as the Primary selection.  This causes it to be hilighted.
;;; When the region goes into the "inactive" state, we disown the Primary
;;; selection, causing the region to be dehilighted.
;;;
;;; Note that it is possible for the region to be in the "active" state
;;; and not be hilighted, if it is in the active state and then some other
;;; application asserts the selection.  This is probably not a big deal.

(defun x-activate-region-as-selection ()
  (if (marker-buffer (mark-marker t))
      (x-own-selection (cons (point-marker t) (mark-marker t)))))

;;; OpenWindows-like "find" processing.  These functions are really Sunisms,
;;; but we put them here instead of in x-win-sun.el in case someone wants
;;; to use them when not running on a Sun console (presumably after binding
;;; them to different keys, or putting them on menus.)

(defvar ow-find-last-string nil)
(defvar ow-find-last-clipboard nil)

(defun ow-find (&optional backward-p)
  "Search forward the next occurrence of the text of the selection."
  (interactive)
  (let ((sel (condition-case () (x-get-selection) (error nil)))
	(clip (condition-case () (x-get-clipboard) (error nil)))
	text)
    (setq text (cond
		(sel)
		((not (equal clip ow-find-last-clipboard))
		 (setq ow-find-last-clipboard clip))
		(ow-find-last-string)
		(t (error "No selection available"))))
    (setq ow-find-last-string text)
    (cond (backward-p
	   (search-backward text)
	   (set-mark (+ (point) (length text))))
	  (t
	   (search-forward text)
	   (set-mark (- (point) (length text)))))
    (zmacs-activate-region)))

(defun ow-find-backward ()
  "Search backward the previous occurrence of the text of the selection."
  (interactive)
  (ow-find t))

(defun x-initialize-compose ()
  "Enable compose processing"
  (require 'x-compose))

;;; Load X-server specific code.
;;; Specifically, load some code to repair the grievous damage that MIT and
;;; Sun have done to the default keymap for the Sun keyboards.

(defun x-initialize-keyboard ()
  "Perform X-Server-specific initializations.  Don't call this."
  ;; This is some heuristic junk that tries to guess whether this is
  ;; a Sun keyboard.
  ;;
  ;; One way of implementing this (which would require C support) would
  ;; be to examine the X keymap itself and see if the layout looks even
  ;; remotely like a Sun - check for the Find key on a particular
  ;; keycode, for example.  It'd be nice to have a table of this to
  ;; recognize various keyboards; see also xkeycaps.
  (let ((vendor (x-server-vendor)))
    (cond ((or (string-match "Sun Microsystems" vendor)
	       ;; MIT losingly fails to tell us what hardware the X server
	       ;; is managing, so assume all MIT displays are Suns...  HA HA!
	       (string-equal "MIT X Consortium" vendor)
	       (string-equal "X Consortium" vendor))
           ;; Ok, we think this could be a Sun keyboard.  Load the Sun code.
           (or (load "x-win-sun" t t)
               (warn "Unable to load term file x-win-sun")))
          ((string-match "XFree86" vendor)
           ;; Those XFree86 people do some weird keysym stuff, too.
           (or (load "x-win-xfree86" t t)
               (warn "Unable to load term file x-win-xfree86")))
          )))


(defvar pre-x-win-initted nil)

(defun init-pre-x-win ()
  "Initialize X Windows at startup (pre).  Don't call this."
  (when (not pre-x-win-initted)
    (require 'x-iso8859-1)
    (setq character-set-property 'x-iso8859/1) ; see x-iso8859-1.el

    (setq initial-frame-plist (if initial-frame-unmapped-p
                                  '(initially-unmapped t)
                                nil))
    (setq pre-x-win-initted t)))

(defvar x-win-initted nil)

(defun init-x-win ()
  "Initialize X Windows at startup.  Don't call this."
  (when (not x-win-initted)
    (init-pre-x-win)

    ;; Open the X display when this file is loaded
    ;; (Note that the first frame is created later.)
    (setq x-initial-argv-list (cons (car command-line-args)
                                    command-line-args-left))
    (make-x-device nil)
    (setq command-line-args-left (cdr x-initial-argv-list))
    (setq x-win-initted t)))
    
(defvar post-x-win-initted nil)

(defun init-post-x-win ()
  "Initialize X Windows at startup (post).  Don't call this."
  (when (not post-x-win-initted)
    ;; We can't load this until after the initial X device is created
    ;; because the icon initialization needs to access the display to get
    ;; any toolbar-related color resources.
    (if (featurep 'toolbar)
        (init-x-toolbar))
    ;; these are only ever called if zmacs-regions is true.
    (add-hook 'zmacs-deactivate-region-hook 
	      (lambda () 
		(if (console-on-window-system-p) 
		    (x-disown-selection))))
    (add-hook 'zmacs-activate-region-hook
	      (lambda () 
		(if (console-on-window-system-p) 
		    (x-activate-region-as-selection))))
    (add-hook 'zmacs-update-region-hook
	      (lambda ()
		  (if (console-on-window-system-p)
		      (x-activate-region-as-selection))))
    ;; Motif-ish bindings
    ;; The following two were generally unliked.
    ;;(define-key global-map '(shift delete)   'x-kill-primary-selection)
    ;;(define-key global-map '(control delete) 'x-delete-primary-selection)
    (define-key global-map '(shift insert)   'x-yank-clipboard-selection)
    (define-key global-map '(control insert) 'x-copy-primary-selection)
    ;; These are Sun-isms.
    (define-key global-map 'copy	'x-copy-primary-selection)
    (define-key global-map 'paste	'x-yank-clipboard-selection)
    (define-key global-map 'cut		'x-kill-primary-selection)

    (define-key global-map 'menu	'popup-mode-menu)
    ;;(define-key global-map '(shift menu) 'x-goto-menubar) ;NYI

    ;; This runs after the first frame has been created (we can't
    ;; talk to the X server before that) but before the
    ;; site-start-file or .emacs file, so sites and users have a
    ;; chance to override it.
    (add-hook 'before-init-hook 'x-initialize-keyboard)
    (add-hook 'before-init-hook 'x-initialize-compose)

    (setq post-x-win-initted t)))

(defun make-frame-on-display (display &optional parms)
  "Create a frame on the X display named DISPLAY.
DISPLAY should be a standard display string such as \"unix:0\",
or nil for the display specified on the command line or in the
DISPLAY environment variable.

PROPS should be an plist of properties, as in the call to `make-frame'.

This function opens a connection to the display or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on display: ")
  (if (equal display "") (setq display nil))
  (make-frame-on-device 'x display parms))

;;; x-init.el ends here
