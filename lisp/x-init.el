;;; x-init.el --- initialization code for X windows

;; Copyright (C) 1990, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: terminals, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when X support is compiled in).

;;; Code:

(globally-declare-fboundp
 '(x-server-vendor x-init-specifier-from-resources init-mule-x-win))

(globally-declare-boundp
 '(x-initial-argv-list x-app-defaults-directory))

;; If you want to change this variable, this is the place you must do it.
;; Do not set it to a string containing periods.  X doesn't like that.
;(setq x-emacs-application-class "Emacs")

(defgroup x nil
  "The X Window system."
  :group 'environment)

;; OpenWindows-like "find" processing.  These functions are really Sunisms,
;; but we put them here instead of in x-win-sun.el in case someone wants
;; to use them when not running on a Sun console (presumably after binding
;; them to different keys, or putting them on menus.)

(defvar ow-find-last-string nil)
(defvar ow-find-last-clipboard nil)

(defun ow-find (&optional backward-p)
  "Search forward the next occurrence of the text of the selection."
  (interactive)
  (let ((sel  (ignore-errors (get-selection)))
	(clip (ignore-errors (get-clipboard)))
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
  "Search backward for the previous occurrence of the text of the selection."
  (interactive)
  (ow-find t))

(labels
    ((pseudo-canonicalize-keysym (keysym)
       "If KEYSYM (a string or a symbol) might describe a keysym on
the current keyboard, return its canonical XEmacs form, a symbol;
otherwise return nil.

Does not intern new symbols, since if a string doesn't correspond to a
keysym that XEmacs has seen, that string won't be in obarray."
       (if (symbolp keysym)
	   keysym
	 (if (stringp keysym)
             (or (intern-soft keysym)
                 (intern-soft (nsubstitute ?- ?_ (downcase keysym))))))))
  (declare (inline pseudo-canonicalize-keysym))

  (defun x-keysym-on-keyboard-sans-modifiers-p (keysym &optional device)
    "Return true if KEYSYM names a key on the keyboard of DEVICE.
More precisely, return true if pressing a physical key
on the keyboard of DEVICE without any modifier keys generates KEYSYM.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
The keysym name can be provided in two forms:
- if keysym is a string, it must be the name as known to X windows.
- if keysym is a symbol, it must be the name as known to XEmacs.
The two names differ in capitalization and underscoring."
    (eq 'sans-modifiers (gethash (pseudo-canonicalize-keysym keysym)
				 (x-keysym-hash-table device))))

 (defun x-keysym-on-keyboard-p (keysym &optional device)
   "Return true if KEYSYM names a key on the keyboard of DEVICE.
More precisely, return true if some keystroke (possibly including modifiers)
on the keyboard of DEVICE keys generates KEYSYM.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
The keysym name can be provided in two forms:
- if keysym is a string, it must be the name as known to X windows.
- if keysym is a symbol, it must be the name as known to XEmacs.
The two names differ in capitalization and underscoring.

This function is not entirely trustworthy, in that Xlib compose processing
can produce keysyms that XEmacs will not have seen when it examined the
keysyms available on startup.  So pressing `dead-diaeresis' and then 'a' may
pass `adiaeresis' to XEmacs, or (in some implementations) even `U00E4',
where `(x-keysym-on-keyboard-p 'adiaeresis)' and `(x-keysym-on-keyboard-p
'U00E4)' would both have returned nil.  Subsequent to XEmacs seeing a keysym
it was previously unaware of, the predicate will take note of it, though."
   (and	(gethash (pseudo-canonicalize-keysym keysym)
                 (x-keysym-hash-table device))
	t)))

(eval-when-compile
  (load "x-win-sun"     nil t)
  (load "x-win-xfree86" nil t))

(defun x-initialize-keyboard (device)
  "Perform X-Server-specific initializations.  Don't call this."
  ;; This is some heuristic junk that tries to guess whether this is
  ;; a Sun keyboard.
  ;;
  ;; One way of implementing this (which would require C support) would
  ;; be to examine the X keymap itself and see if the layout looks even
  ;; remotely like a Sun - check for the Find key on a particular
  ;; keycode, for example.  It'd be nice to have a table of this to
  ;; recognize various keyboards; see also xkeycaps.
  ;;
  ;; Note that we cannot use most vendor-provided proprietary keyboard
  ;; APIs to identify the keyboard - those only work on the console.
  ;; xkeycaps has the same problem when running `remotely'.
  (let ((vendor (x-server-vendor device)))
    (cond ((or (search "Sun Microsystems" vendor)
	       ;; MIT losingly fails to tell us what hardware the X server
	       ;; is managing, so assume all MIT displays are Suns...  HA HA!
	       (string-equal "MIT X Consortium" vendor)
	       (string-equal "X Consortium" vendor))
           ;; Ok, we think this could be a Sun keyboard.  Run the Sun code.
	   (x-win-init-sun device))
          ((string-match #r"XFree86\|Cygwin/X\|The X\.Org Foundation" vendor)
           ;; Those XFree86 people do some weird keysym stuff, too.
	   (x-win-init-xfree86 device)))))

;; Moved from x-toolbar.el, since InfoDock doesn't dump x-toolbar.el.
(defun x-init-toolbar-from-resources (locale)
  (loop for (specifier . resname) in
    `((   ,top-toolbar-height       .    "topToolBarHeight")
      (,bottom-toolbar-height       . "bottomToolBarHeight")
      (  ,left-toolbar-width        .   "leftToolBarWidth")
      ( ,right-toolbar-width        .  "rightToolBarWidth")

      (   ,top-toolbar-border-width .    "topToolBarBorderWidth")
      (,bottom-toolbar-border-width . "bottomToolBarBorderWidth")
      (  ,left-toolbar-border-width .   "leftToolBarBorderWidth")
      ( ,right-toolbar-border-width .  "rightToolBarBorderWidth"))
    do
    (x-init-specifier-from-resources
     specifier 'natnum locale (cons resname (upcase-initials resname)))))

(defvar make-device-early-x-entry-point-called-p nil
  "Whether `make-device-early-x-entry-point' has been called, at least once.

Much of the X11-specific Lisp init code should only be called the first time
an X11 device is created; this variable allows for that.")

(defvar make-device-late-x-entry-point-called-p nil
  "Whether `make-device-late-x-entry-point' has been called, at least once.

Much of the X11-specific Lisp init code should only be called the first time
an X11 device is created; this variable allows for that.")

(defun make-device-early-x-entry-point ()
  "Entry point to set up the Lisp environment for X device creation."
  (unless make-device-early-x-entry-point-called-p
    (setq initial-frame-plist
          (and initial-frame-unmapped-p '(initially-unmapped t))
          ;; Save the argv value. 
          x-initial-argv-list
          (cons (car command-line-args) command-line-args-left)
          ;; Locate the app-defaults directory
          x-app-defaults-directory
          (or x-app-defaults-directory (locate-data-directory "app-defaults"))
          make-device-early-x-entry-point-called-p t)))

(defun make-device-late-x-entry-point (device)
  "Entry point to do any Lisp-level X device-specific initialization."
  ;; General code, called on every X device created:
  (x-initialize-keyboard device)
  ;; And the following code is to be called once, the first time an X11
  ;; device is created:
  (unless make-device-late-x-entry-point-called-p
    (setq command-line-args-left (cdr x-initial-argv-list))
    ;; Motif-ish bindings
    (define-key global-map '(shift insert)   'yank-clipboard-selection)
    (define-key global-map '(control insert) 'copy-primary-selection)
    ;; These are Sun-isms.
    (define-key global-map 'copy	'copy-primary-selection)
    (define-key global-map 'paste	'yank-clipboard-selection)
    (define-key global-map 'cut		'kill-primary-selection)
    (setq make-device-late-x-entry-point-called-p t)))

(defun make-frame-on-display (display &optional props)
  "Create a frame on the X display named DISPLAY.
DISPLAY should be a standard display string such as \"unix:0\",
or nil for the display specified on the command line or in the
DISPLAY environment variable.

PROPS should be a plist of properties, as in the call to `make-frame'.

This function opens a connection to the display or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on display: ")
  (if (equal display "") (setq display nil))
  (make-frame-on-device 'x display props))

;;; x-init.el ends here
