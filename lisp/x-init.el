;;; x-init.el --- initialization code for X windows

;; Copyright (C) 1990, 1993, 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Maintainer: XEmacs Development Team
;; Keywords: terminals, dumped

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

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when X support is compiled in).

;;; Code:

(globally-declare-fboundp
 '(x-keysym-on-keyboard-p
   x-server-vendor x-init-specifier-from-resources init-mule-x-win))

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

;; Load X-server specific code.
;; Specifically, load some code to repair the grievous damage that MIT and
;; Sun have done to the default keymap for the Sun keyboards.
(defun x-initialize-compose (device)
  "Enable compose key and dead key processing on DEVICE."
  (autoload 'compose-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-acute-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-grave-map	    "x-compose" nil t 'keymap)
  (autoload 'compose-cedilla-map    "x-compose" nil t 'keymap)
  (autoload 'compose-diaeresis-map  "x-compose" nil t 'keymap)
  (autoload 'compose-circumflex-map "x-compose" nil t 'keymap)
  (autoload 'compose-tilde-map	    "x-compose" nil t 'keymap)

  (loop 
    for (key map)
    ;; The dead keys might really be called just about anything, depending
    ;; on the vendor.  MIT thinks that the prefixes are "SunFA_", "D", and
    ;; "hpmute_" for Sun, DEC, and HP respectively.  However, OpenWindows 3
    ;; thinks that the prefixes are "SunXK_FA_", "DXK_", and "hpXK_mute_".
    ;; And HP (who don't mention Sun and DEC at all) use "XK_mute_".  Go
    ;; figure.

    ;; Presumably if someone is running OpenWindows, they won't be using the
    ;; DEC or HP keysyms, but if they are defined then that is possible, so
    ;; in that case we accept them all.

    ;; If things seem not to be working, you might want to check your
    ;; /usr/lib/X11/XKeysymDB file to see if your vendor has an equally
    ;; mixed up view of what these keys should be called.

    ;; Canonical names:
    in '((acute			compose-acute-map)
         (grave			compose-grave-map)
         (cedilla		compose-cedilla-map)
         (diaeresis		compose-diaeresis-map)
         (circumflex		compose-circumflex-map)
         (tilde			compose-tilde-map)
         (degree			compose-ring-map)
         (multi-key              compose-map)

         ;; Sun according to MIT:
         (SunFA_Acute		compose-acute-map)
         (SunFA_Grave		compose-grave-map)
         (SunFA_Cedilla		compose-cedilla-map)
         (SunFA_Diaeresis	compose-diaeresis-map)
         (SunFA_Circum		compose-circumflex-map)
         (SunFA_Tilde		compose-tilde-map)

         ;; Sun according to OpenWindows 2:
         (Dead_Grave		compose-grave-map)
         (Dead_Circum		compose-circumflex-map)
         (Dead_Tilde		compose-tilde-map)

         ;; Sun according to OpenWindows 3:
         (SunXK_FA_Acute		compose-acute-map)
         (SunXK_FA_Grave		compose-grave-map)
         (SunXK_FA_Cedilla	compose-cedilla-map)
         (SunXK_FA_Diaeresis	compose-diaeresis-map)
         (SunXK_FA_Circum	compose-circumflex-map)
         (SunXK_FA_Tilde		compose-tilde-map)

         ;; DEC according to MIT:
         (Dacute_accent		compose-acute-map)
         (Dgrave_accent		compose-grave-map)
         (Dcedilla_accent	compose-cedilla-map)
         (Dcircumflex_accent	compose-circumflex-map)
         (Dtilde			compose-tilde-map)
         (Dring_accent		compose-ring-map)

         ;; DEC according to OpenWindows 3:
         (DXK_acute_accent	compose-acute-map)
         (DXK_grave_accent	compose-grave-map)
         (DXK_cedilla_accent	compose-cedilla-map)
         (DXK_circumflex_accent	compose-circumflex-map)
         (DXK_tilde		compose-tilde-map)
         (DXK_ring_accent	compose-ring-map)

         ;; HP according to MIT:
         (hpmute_acute		compose-acute-map)
         (hpmute_grave		compose-grave-map)
         (hpmute_diaeresis	compose-diaeresis-map)
         (hpmute_asciicircum	compose-circumflex-map)
         (hpmute_asciitilde	compose-tilde-map)

         ;; Empirically discovered on Linux XFree86 MetroX:
         (usldead_acute		compose-acute-map)
         (usldead_grave		compose-grave-map)
         (usldead_diaeresis	compose-diaeresis-map)
         (usldead_asciicircum	compose-circumflex-map)
         (usldead_asciitilde	compose-tilde-map)

         ;; HP according to OpenWindows 3:
         (hpXK_mute_acute	compose-acute-map)
         (hpXK_mute_grave	compose-grave-map)
         (hpXK_mute_diaeresis	compose-diaeresis-map)
         (hpXK_mute_asciicircum	compose-circumflex-map)
         (hpXK_mute_asciitilde	compose-tilde-map)

         ;; HP according to HP-UX 8.0:
         (XK_mute_acute		compose-acute-map)
         (XK_mute_grave		compose-grave-map)
         (XK_mute_diaeresis	compose-diaeresis-map)
         (XK_mute_asciicircum	compose-circumflex-map)
         (XK_mute_asciitilde	compose-tilde-map)

         ;; [[ XFree86 seems to use lower case and a hyphen ]] Not true;
         ;; they use lower case and an underscore. XEmacs converts the
         ;; underscore to a hyphen in x_keysym_to_emacs_keysym because the
         ;; keysym is in the "Keyboard" character set, which is just totally
         ;; fucking random, considering it doesn't happen for any other
         ;; character sets.
         (dead-acute		compose-acute-map)
         (dead-grave		compose-grave-map)
         (dead-cedilla		compose-cedilla-map)
         (dead-diaeresis		compose-diaeresis-map)
         (dead-circum		compose-circumflex-map)
         (dead-circumflex	compose-circumflex-map)
         (dead-tilde		compose-tilde-map))
    
    ;; Get the correct value for function-key-map
    with function-key-map = (symbol-value-in-console 'function-key-map
                                                     (device-console device)
                                                     function-key-map)
    do (when (x-keysym-on-keyboard-p key device)
         (define-key function-key-map (vector key) map))))

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
    (cond ((or (string-match "Sun Microsystems" vendor)
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
  (x-initialize-compose device)
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
