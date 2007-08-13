;;; obsolete.el --- obsoleteness support

;; Copyright (C) 1985-1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;; The obsoleteness support used to be scattered throughout various
;; source files.  We put the stuff in one place to remove the junkiness
;; from other source files and to facilitate creating/updating things
;; like sysdep.el.

;;; Code:

(defsubst define-obsolete-function-alias (oldfun newfun)
  "Define OLDFUN as an obsolete alias for function NEWFUN.
This makes calling OLDFUN equivalent to calling NEWFUN and marks OLDFUN
as obsolete."
  (define-function oldfun newfun)
  (make-obsolete oldfun newfun))

(defsubst define-compatible-function-alias (oldfun newfun)
  "Define OLDFUN as a compatible alias for function NEWFUN.
This makes calling OLDFUN equivalent to calling NEWFUN and marks OLDFUN
as provided for compatibility only."
  (define-function oldfun newfun)
  (make-compatible oldfun newfun))

(defsubst define-obsolete-variable-alias (oldvar newvar)
  "Define OLDVAR as an obsolete alias for variable NEWVAR.
This makes referencing or setting OLDVAR equivalent to referencing or
setting NEWVAR and marks OLDVAR as obsolete.
If OLDVAR was bound and NEWVAR was not, Set NEWVAR to OLDVAR.

Note: Use this before any other references (defvar/defcustom) to NEWVAR"
  (let ((needs-setting (and (boundp oldvar) (not (boundp newvar))))
        (value (and (boundp oldvar) (symbol-value oldvar))))
     (defvaralias oldvar newvar)
     (make-obsolete-variable oldvar newvar)
     (and needs-setting (set newvar value))))

(defsubst define-compatible-variable-alias (oldvar newvar)
  "Define OLDVAR as a compatible alias for variable NEWVAR.
This makes referencing or setting OLDVAR equivalent to referencing or
setting NEWVAR and marks OLDVAR as provided for compatibility only."
  (defvaralias oldvar newvar)
  (make-compatible-variable oldvar newvar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; device stuff

(make-compatible-variable 'window-system "use (console-type)")
(make-obsolete-variable 'meta-flag
			"use the `set-input-mode' function instead.")

(defun x-display-color-p (&optional device)
  "Return t if DEVICE is a color device."
  (eq 'color (device-class device)))
(make-compatible 'x-display-color-p 'device-class)

(define-function 'x-color-display-p 'x-display-color-p)
(make-compatible 'x-display-color-p 'device-class)

(defun x-display-grayscale-p (&optional device)
  "Return t if DEVICE is a grayscale device."
  (eq 'grayscale (device-class device)))
(make-compatible 'x-display-grayscale-p 'device-class)

(define-function 'x-grayscale-display-p 'x-display-grayscale-p)
(make-compatible 'x-display-grayscale-p 'device-class)

(define-compatible-function-alias 'x-display-pixel-width  'device-pixel-width)
(define-compatible-function-alias 'x-display-pixel-height 'device-pixel-height)
(define-compatible-function-alias 'x-display-planes       'device-bitplanes)
(define-compatible-function-alias 'x-display-color-cells  'device-color-cells)

(define-obsolete-function-alias 'baud-rate 'device-baud-rate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; events

(define-obsolete-function-alias 'menu-event-p 'misc-user-event-p)
(make-obsolete-variable 'unread-command-char 'unread-command-events)
(make-obsolete 'sleep-for-millisecs "use sleep-for with a float")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; extents

(defun extent-data (extent)
  "Obsolete.  Return the `data' property of EXTENT."
  (extent-property extent 'data))
(make-obsolete 'set-window-dot 'set-window-point)

(defun set-extent-data (extent data)
  "Obsolete.  Set the `data' property of EXTENT."
  (set-extent-property extent 'data data))
(make-obsolete 'set-extent-data 'set-extent-property)

(define-obsolete-function-alias 'extent-buffer 'extent-object)

(defun set-extent-attribute (extent attr &optional clearp)
  ;; obsoleteness info will be displayed, so no need for docstring.
  (cond ((eq attr 'write-protected)
         (set-extent-property extent 'read-only t))
        ((eq attr 'unhighlight)
         (set-extent-property extent 'mouse-face nil))
        ((eq attr 'writable)
         (set-extent-property extent 'read-only nil))
        ((eq attr 'visible)
         (set-extent-property extent 'invisible nil))
        (t
         (set-extent-property extent attr t))))
(make-obsolete 'set-extent-attribute 'set-extent-property)

(defun extent-glyph (extent)
  ;; obsoleteness info will be displayed, so no need for docstring.
  (or (extent-begin-glyph extent)
      (extent-end-glyph extent)))
(make-obsolete 'extent-glyph
	       "use `extent-begin-glyph' or `extent-end-glyph' instead.")

(defun extent-layout (extent)
  ;; obsoleteness info will be displayed, so no need for docstring.
  (extent-begin-glyph-layout extent))
(make-obsolete 'extent-layout
       "use `extent-begin-glyph-layout' or `extent-end-glyph-layout' instead.")

(defun set-extent-layout (extent layout)
  ;; obsoleteness info will be displayed, so no need for docstring.
  (set-extent-begin-glyph-layout extent layout))
(make-obsolete 'set-extent-layout
       "use `set-extent-begin-glyph-layout' or `set-extent-end-glyph-layout' instead.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; frames

(define-obsolete-variable-alias 'select-screen-hook 'select-frame-hook)
(define-obsolete-variable-alias 'deselect-screen-hook 'deselect-frame-hook)
(define-obsolete-variable-alias 'auto-raise-screen 'auto-raise-frame)
(define-obsolete-variable-alias 'auto-lower-screen 'auto-lower-frame)
(define-obsolete-variable-alias 'get-screen-for-buffer-default-screen-name
  'get-frame-for-buffer-default-frame-name)

(define-obsolete-function-alias 'buffer-dedicated-screen
  'buffer-dedicated-frame)
(define-obsolete-function-alias 'deiconify-screen 'deiconify-frame)
(define-obsolete-function-alias 'delete-screen 'delete-frame)
(define-obsolete-function-alias 'event-screen 'event-frame)
(define-obsolete-function-alias 'find-file-other-screen 'find-file-other-frame)
(define-obsolete-function-alias 'find-file-read-only-other-screen
  'find-file-read-only-other-frame)
(define-obsolete-function-alias 'live-screen-p 'frame-live-p)
(define-obsolete-function-alias 'screen-height 'frame-height)
(define-obsolete-function-alias 'screen-iconified-p 'frame-iconified-p)
(define-obsolete-function-alias 'screen-list 'frame-list)
(define-obsolete-function-alias 'screen-live-p 'frame-live-p)
(define-obsolete-function-alias 'screen-name 'frame-name)
(define-obsolete-function-alias 'screen-parameters 'frame-parameters)
(define-obsolete-function-alias 'screen-pixel-height 'frame-pixel-height)
(define-obsolete-function-alias 'screen-pixel-width 'frame-pixel-width)
(define-obsolete-function-alias 'screen-root-window 'frame-root-window)
(define-obsolete-function-alias 'screen-selected-window 'frame-selected-window)
(define-obsolete-function-alias 'screen-totally-visible-p
  'frame-totally-visible-p)
(define-obsolete-function-alias 'screen-visible-p 'frame-visible-p)
(define-obsolete-function-alias 'screen-width 'frame-width)
(define-obsolete-function-alias 'screenp 'framep)
(define-obsolete-function-alias 'get-screen-for-buffer 'get-frame-for-buffer)
(define-obsolete-function-alias 'get-screen-for-buffer-noselect
  'get-frame-for-buffer-noselect)
(define-obsolete-function-alias 'get-other-screen 'get-other-frame)
(define-obsolete-function-alias 'iconify-screen 'iconify-frame)
(define-obsolete-function-alias 'lower-screen 'lower-frame)
(define-obsolete-function-alias 'mail-other-screen 'mail-other-frame)
(define-obsolete-function-alias 'make-screen 'make-frame)
(define-obsolete-function-alias 'make-screen-invisible 'make-frame-invisible)
(define-obsolete-function-alias 'make-screen-visible 'make-frame-visible)
(define-obsolete-function-alias 'modify-screen-parameters
  'modify-frame-parameters)
(define-obsolete-function-alias 'new-screen 'new-frame)
(define-obsolete-function-alias 'next-screen 'next-frame)
(define-obsolete-function-alias 'next-multiscreen-window
  'next-multiframe-window)
(define-obsolete-function-alias 'other-screen 'other-frame)
(define-obsolete-function-alias 'previous-screen 'previous-frame)
(define-obsolete-function-alias 'previous-multiscreen-window
  'previous-multiframe-window)
(define-obsolete-function-alias 'raise-screen 'raise-frame)
(define-obsolete-function-alias 'redraw-screen 'redraw-frame)
(define-obsolete-function-alias 'select-screen 'select-frame)
(define-obsolete-function-alias 'selected-screen 'selected-frame)
(define-obsolete-function-alias 'set-buffer-dedicated-screen
  'set-buffer-dedicated-frame)
(define-obsolete-function-alias 'set-screen-height 'set-frame-height)
(define-obsolete-function-alias 'set-screen-position 'set-frame-position)
(define-obsolete-function-alias 'set-screen-size 'set-frame-size)
(define-obsolete-function-alias 'set-screen-width 'set-frame-width)
(define-obsolete-function-alias 'show-temp-buffer-in-current-screen
  'show-temp-buffer-in-current-frame)
(define-obsolete-function-alias 'switch-to-buffer-other-screen
  'switch-to-buffer-other-frame)
(define-obsolete-function-alias 'visible-screen-list 'visible-frame-list)
(define-obsolete-function-alias 'window-screen 'window-frame)
(define-obsolete-function-alias 'x-set-screen-pointer
  'set-frame-pointer)
(define-obsolete-function-alias 'x-set-frame-pointer
  'set-frame-pointer)

(define-obsolete-variable-alias 'screen-title-format 'frame-title-format)
(define-obsolete-variable-alias 'screen-icon-title-format
  'frame-icon-title-format)
(define-obsolete-variable-alias 'terminal-screen 'terminal-frame)
(define-obsolete-variable-alias 'delete-screen-hook 'delete-frame-hook)
(define-obsolete-variable-alias 'create-screen-hook 'create-frame-hook)
(define-obsolete-variable-alias 'mouse-enter-screen-hook
  'mouse-enter-frame-hook)
(define-obsolete-variable-alias 'mouse-leave-screen-hook
  'mouse-leave-frame-hook)
(define-obsolete-variable-alias 'map-screen-hook 'map-frame-hook)
(define-obsolete-variable-alias 'unmap-screen-hook 'unmap-frame-hook)
(define-obsolete-variable-alias 'default-screen-alist 'default-frame-alist)
(define-obsolete-variable-alias 'default-screen-name 'default-frame-name)
(define-obsolete-variable-alias 'x-screen-defaults 'default-x-frame-alist)

(defun x-create-screen (parms window-id)
  ;; obsoleteness info will be displayed, so no need for docstring.
  (if (not (eq 'x (device-type (selected-device))))
      (error "Cannot create X frames on non-X device"))
  (make-frame (append parms (list (list 'window-id window-id)))
              (selected-device)))
(make-obsolete 'x-create-screen 'make-frame)

(defun frame-first-window (frame)
  "Return the topmost, leftmost window of FRAME.
If omitted, FRAME defaults to the currently selected frame."
  (frame-highest-window frame 0))
(make-compatible 'frame-first-window 'frame-highest-window)

(define-obsolete-variable-alias 'initial-frame-alist 'initial-frame-plist)
(define-obsolete-variable-alias 'minibuffer-frame-alist
  'minibuffer-frame-plist)
(define-obsolete-variable-alias 'pop-up-frame-alist 'pop-up-frame-plist)
(define-obsolete-variable-alias 'special-display-frame-alist
  'special-display-frame-plist)

;; Defined in C.

(define-obsolete-variable-alias 'default-frame-alist 'default-frame-plist)
(define-obsolete-variable-alias 'default-x-frame-alist 'default-x-frame-plist)
(define-obsolete-variable-alias 'default-tty-frame-alist
  'default-tty-frame-plist)

(make-compatible 'frame-parameters 'frame-property)
(defun frame-parameters (&optional frame)
  "Return the parameters-alist of frame FRAME.
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
If FRAME is omitted, return information on the currently selected frame.

See the variables `default-frame-plist', `default-x-frame-plist', and
`default-tty-frame-plist' for a description of the parameters meaningful
for particular types of frames."
  (or frame (setq frame (selected-frame)))
  ;; #### This relies on a `copy-sequence' of the user properties in
  ;; `frame-properties'.  Removing that would make `frame-properties' more
  ;; efficient but this function less efficient, as we couldn't be
  ;; destructive.  Since most callers now use `frame-parameters', we'll
  ;; do it this way.  Should probably change this at some point in the
  ;; future.
  (destructive-plist-to-alist (frame-properties frame)))

(make-compatible 'modify-frame-parameters 'set-frame-properties)
(defun modify-frame-parameters (frame alist)
  "Modify the properties of frame FRAME according to ALIST.
ALIST is an alist of properties to change and their new values.
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.

See `set-frame-properties' for built-in property names."
  ;; it would be nice to be destructive here but that's not safe.
  (set-frame-properties frame (alist-to-plist alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; faces

(define-obsolete-function-alias 'list-faces-display 'edit-faces)
(define-obsolete-function-alias 'list-faces 'face-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; files

(make-obsolete-variable 'trim-versions-without-asking 'delete-old-versions)
;;; Old XEmacs name; kept around for compatibility.
(define-obsolete-variable-alias 'after-write-file-hooks 'after-save-hook)
(define-obsolete-function-alias 'truename 'file-truename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; paths

(defvar Info-default-directory-list nil
  "This used to be the initial value of Info-directory-list.
If you want to change the locations where XEmacs looks for info files,
set Info-directory-list.")
(make-obsolete-variable 'Info-default-directory-list 'Info-directory-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks

(make-obsolete-variable 'auto-fill-hook 'auto-fill-function)
(make-obsolete-variable 'blink-paren-hook 'blink-paren-function)
(make-obsolete-variable 'lisp-indent-hook 'lisp-indent-function)
(make-obsolete-variable 'comment-indent-hook 'comment-indent-function)
(make-obsolete-variable 'temp-buffer-show-hook
			'temp-buffer-show-function)
(make-obsolete-variable 'inhibit-local-variables
			"use `enable-local-variables' (with the reversed sense).")
(make-obsolete-variable 'suspend-hooks 'suspend-hook)
(make-obsolete-variable 'first-change-function 'first-change-hook)
(make-obsolete-variable 'before-change-function
  "use before-change-functions; which is a list of functions rather than a single function.")
(make-obsolete-variable 'after-change-function
  "use after-change-functions; which is a list of functions rather than a single function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; insertion and deletion

(define-compatible-function-alias 'insert-and-inherit 'insert)
(define-compatible-function-alias 'insert-before-markers-and-inherit
  'insert-before-markers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; keymaps

(defun keymap-parent (keymap)
  "Return the first parent of the given keymap."
  (car (keymap-parents keymap)))
(make-compatible 'keymap-parent 'keymap-parents)

(defun set-keymap-parent (keymap parent)
  "Make the given keymap have (only) the given parent."
  (set-keymap-parents keymap (if parent (list parent) '()))
  parent)
(make-compatible 'set-keymap-parent 'set-keymap-parents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; menu stuff

(defun add-menu-item (menu-path item-name function enabled-p &optional before)
  "Obsolete.  See the function `add-menu-button'."
  (or item-name (error "must specify an item name"))
  (add-menu-button menu-path (vector item-name function enabled-p) before))
(make-obsolete 'add-menu-item 'add-menu-button)

(defun add-menu (menu-path menu-name menu-items &optional before)
  "See the function `add-submenu'."
  (or menu-name (error (gettext "must specify a menu name")))
  (or menu-items (error (gettext "must specify some menu items")))
  (add-submenu menu-path (cons menu-name menu-items) before))
;; Can't make this obsolete.  easymenu depends on it.
(make-compatible 'add-menu 'add-submenu)

(define-obsolete-function-alias 'popup-menu-up-p 'popup-up-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; minibuffer

(define-compatible-function-alias 'read-minibuffer
  'read-expression) ; misleading name
(define-compatible-function-alias 'read-input 'read-string)
(make-obsolete 'read-no-blanks-input 'read-string) ; mocklisp crud

;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc

;; (defun user-original-login-name ()
;;   "Return user's login name from original login.
;; This tries to remain unaffected by `su', by looking in environment variables."
;;   (or (getenv "LOGNAME") (getenv "USER") (user-login-name)))
(define-obsolete-function-alias 'user-original-login-name 'user-login-name)

; old names
(define-obsolete-function-alias 'wholenump 'natnump)
(define-obsolete-function-alias 'show-buffer 'set-window-buffer)
(define-obsolete-function-alias 'buffer-flush-undo 'buffer-disable-undo)
(define-obsolete-function-alias 'eval-current-buffer 'eval-buffer)
(define-obsolete-function-alias 'byte-code-function-p
  'compiled-function-p) ;FSFmacs

;;(make-obsolete 'mod '%)	; mod and % are different now

(make-obsolete 'ring-mod 'mod)

(make-obsolete 'current-time-seconds 'current-time)
;; too bad there's not a way to check for aref, assq, and nconc
;; being called on the values of functions known to return keymaps,
;; or known to return vectors of events instead of strings...

(define-obsolete-function-alias 'run-special-hook-with-args
  'run-hook-with-args-until-success)

(make-obsolete-variable 'executing-macro 'executing-kbd-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; modeline

(define-compatible-function-alias 'redraw-mode-line 'redraw-modeline)
(define-compatible-function-alias 'force-mode-line-update
  'redraw-modeline) ;; FSF compatibility
(define-compatible-variable-alias 'mode-line-map 'modeline-map)
(define-compatible-variable-alias 'mode-line-buffer-identification
  'modeline-buffer-identification)
(define-compatible-variable-alias 'mode-line-process 'modeline-process)
(define-compatible-variable-alias 'mode-line-modified 'modeline-modified)
(make-compatible-variable 'mode-line-inverse-video
			"use set-face-highlight-p and set-face-reverse-p")
(define-compatible-variable-alias 'default-mode-line-format
  'default-modeline-format)
(define-compatible-variable-alias 'mode-line-format 'modeline-format)
(define-compatible-variable-alias 'mode-line-menu 'modeline-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; mouse

;;; (defun mouse-eval-last-sexpr (event)
;;;   (interactive "@e")
;;;   (save-excursion
;;;     (mouse-set-point event)
;;;     (eval-last-sexp nil)))

(define-obsolete-function-alias 'mouse-eval-last-sexpr 'mouse-eval-sexp)

(defun read-mouse-position (frame)
  (cdr (mouse-position (frame-device frame))))
(make-obsolete 'read-mouse-position 'mouse-position)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; `point'

(define-obsolete-function-alias 'dot 'point)
(define-obsolete-function-alias 'dot-marker 'point-marker)
(define-obsolete-function-alias 'dot-min 'point-min)
(define-obsolete-function-alias 'dot-max 'point-max)
(define-obsolete-function-alias 'window-dot 'window-point)
(define-obsolete-function-alias 'set-window-dot 'set-window-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; processes

(define-obsolete-function-alias 'send-string 'process-send-string)
(define-obsolete-function-alias 'send-region 'process-send-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; redisplay

(defun redraw-display (&optional device)
  (if (eq device t)
      (mapcar 'redisplay-device (device-list))
    (redisplay-device device)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; stuff replaced by specifiers

(defun screen-scrollbar-width (&optional screen)
  ;; specifier-specs is the inverse of set-specifier, but
  ;; the way this function was defined, specifier-instance
  ;; is closer.
  (specifier-instance scrollbar-width (or screen (selected-frame))))
(make-obsolete 'screen-scrollbar-width
	       "use (specifier-instance scrollbar-width ...).")

(defun set-screen-scrollbar-width (screen value)
  (set-specifier scrollbar-width (cons screen value)))
(make-obsolete 'set-screen-scrollbar-width
	       "use (set-specifier scrollbar-width ...).")

(defun set-screen-left-margin-width (value &optional screen)
  (set-specifier left-margin-width
		 (cons (or screen (selected-frame)) value)))
(make-obsolete 'set-screen-left-margin-width
	       "use (set-specifier left-margin-width ...).")

(defun set-screen-right-margin-width (value &optional screen)
  (set-specifier right-margin-width
		 (cons (or screen (selected-frame)) value)))
(make-obsolete 'set-screen-right-margin-width
	       "use (set-specifier right-margin-width ...).")

(defun set-buffer-left-margin-width (value &optional buffer)
  (set-specifier left-margin-width (cons (or buffer (current-buffer)) value)))
(make-obsolete 'set-buffer-left-margin-width
	       "use (set-specifier left-margin-width ...).")

(defun set-buffer-right-margin-width (value &optional buffer)
  (set-specifier right-margin-width (cons (or buffer (current-buffer)) value)))
(make-obsolete 'set-buffer-right-margin-width
	       "use (set-specifier right-margin-width ...).")

(defun screen-left-margin-width (&optional screen)
  (specifier-specs left-margin-width (or screen (selected-frame))))
(make-obsolete 'screen-left-margin-width
	       "use (specifier-specs left-margin-width ...).")

(defun screen-right-margin-width (&optional screen)
  (specifier-specs right-margin-width (or screen (selected-frame))))
(make-obsolete 'screen-right-margin-width
	       "use (specifier-specs right-margin-width ...).")

(defun buffer-left-margin-width (&optional buffer)
  (specifier-specs left-margin-width (or buffer (current-buffer))))
(make-obsolete 'buffer-left-margin-width
	       "use (specifier-specs left-margin-width ...).")

(defun buffer-right-margin-width (&optional buffer)
  (specifier-specs right-margin-width (or buffer (current-buffer))))
(make-obsolete 'buffer-right-margin-width
	       "use (specifier-specs right-margin-width ...).")

(defun x-set-frame-icon-pixmap (frame image-instance &optional mask-ignored)
  "Set the icon of the given frame to the given image instance,
which should be an image instance object (as returned by
`make-image-instance'), a glyph object (as returned by `make-glyph'),
or nil.  If a glyph object is given, the glyph will be instantiated on
the frame to produce an image instance object.

If the given image instance has a mask, that will be used as the icon mask;
however, not all window managers support this.

The window manager is also not required to support color pixmaps,
only bitmaps (one plane deep).

Optional third argument is ignored.  If you're concerned about this
incomplete backwards incompatibility, you should convert your code
to use `frame-icon-glyph' -- you can specify a mask for an XBM file
using the standard image instantiator format."
  (if (glyphp image-instance)
      (setq image-instance (glyph-image-instance image-instance frame)))
  (set-glyph-image frame-icon-glyph image-instance frame))
(make-obsolete 'x-set-frame-icon-pixmap
	       "use (set-glyph-image frame-icon-glyph ...).")
(defalias 'x-set-screen-icon-pixmap 'x-set-frame-icon-pixmap)
(make-obsolete 'x-set-screen-icon-pixmap
	       "use (set-glyph-image frame-icon-glyph ...).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; window-system objects

(define-obsolete-function-alias 'pixel-name 'color-name)

;; compatibility function -- a front-end to make-glyph
(defun make-pixmap (name &optional locale)
  "Create a glyph with NAME as an image specifier and locale LOCALE.
The file should be in `XBM' or `XPM' format.
If the XBMLANGPATH environment variable is set, it will be searched for
 matching files.  Next, the directories listed in the `x-bitmap-file-path'
 variable will be searched (this variable is initialized from the
 \"*bitmapFilePath\" resource).  Finally, the XEmacs etc/ directory
 (the value of `data-directory') will be searched.
The file argument may also be a list of the form (width height data) where
 width and height are the size in pixels, and data is a string, containing
 the raw bits of the bitmap.  (Bitmaps specified this way can only be one bit
 deep.)
If compiled with support for XPM, the file argument may also be a string
 which is the contents of an XPM file (that is, a string beginning with the
 characters \"/* XPM */\"; see the XPM documentation).
The optional second argument is the specifier locale for this pixmap glyph.
The returned object is a glyph object.  To get the actual pixmap object for
a given frame, use the function `glyph-instance'."
  (if (consp name)
      (setq name (vector 'xbm :data name)))
  (make-glyph name))
(make-obsolete 'make-pixmap 'make-glyph)

(defun make-cursor (name &optional fg bg device)
  "Creates a pointer image instance with NAME as an image specifier.
The optional second and third arguments are the foreground and background
 colors.  They may be color name strings or `pixel' objects.
The optional fourth argument is the device on which to allocate the cursor
 (defaults to the selected device).
This allocates a new pointer in the X server, and signals an error if the
 pointer is unknown or cannot be allocated.

A pointer name can take many different forms.  It can be:
 - any of the standard cursor names from appendix B of the Xlib manual
   (also known as the file <X11/cursorfont.h>) minus the XC_ prefix;
 - the name of a font, and glyph index into it of the form
   \"FONT fontname index [[mask-font] mask-index]\";
 - the name of a bitmap or pixmap file;
 - or an image instance object, as returned by `make-image-instance'.

If it is an image instance or pixmap file, and that pixmap comes with a
 mask, then that mask will be used.  If it is an image instance, it must
 have only one plane, since X pointers may only have two colors.  If it is a
 pixmap file, then the file will be read in monochrome.

If it is a bitmap file, and if a bitmap file whose name is the name of the
 pointer with \"msk\" or \"Mask\" appended exists, then that second bitmap
 will be used as the mask.  For example, a pair of files might be named
 \"pointer.xbm\" and \"pointer.xbmmsk\".

The returned object is a normal, first-class lisp object.  The way you
`deallocate' the pointer is the way you deallocate any other lisp object:
you drop all pointers to it and allow it to be garbage collected.  When
these objects are GCed, the underlying X data is deallocated as well."
  ;; #### ignores fg and bg
  (make-image-instance name device '(pointer)))
(make-obsolete 'make-cursor 'make-image-instance)

(define-obsolete-function-alias 'pixmap-width 'glyph-width)
(define-obsolete-function-alias 'pixmap-contributes-to-line-height-p
  'glyph-contrib-p-instance)
(define-obsolete-function-alias 'set-pixmap-contributes-to-line-height
  'set-glyph-contrib-p)

;; the functionality of column.el has been moved into C
(defalias 'display-column-mode 'column-number-mode)

(defun x-color-values  (color &optional frame)
  "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--(RED GREEN BLUE).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is (65280 65280 65280) or (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
  (color-instance-rgb-components (make-color-instance color)))
(make-compatible 'x-color-values 'color-instance-rgb-components)

;; Two loser functions which shouldn't be used.
(make-obsolete 'following-char 'char-after)
(make-obsolete 'preceding-char 'char-before)


;; The following several functions are useful in GNU Emacs 20 because
;; of the multibyte "characters" the internal representation of which
;; leaks into Lisp.  In XEmacs/Mule they are trivial and unnecessary.
;; We provide them for compatibility reasons solely.

(defun string-to-sequence (string type)
  "Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'.
Multibyte characters are concerned."
  (ecase type
    (list
     (mapcar #'identity string))
    (vector
     (mapvector #'identity string))))

(defun string-to-list (string)
  "Return a list of characters in STRING."
  (mapcar #'identity string))

(defun string-to-vector (string)
  "Return a vector of characters in STRING."
  (mapvector #'identity string))

(defun store-substring (string idx obj)
  "Embed OBJ (string or character) at index IDX of STRING."
  (let* ((str (cond ((stringp obj) obj)
		    ((characterp obj) (char-to-string obj))
		    (t (error
			"Invalid argument (should be string or character): %s"
			obj))))
	 (string-len (length string))
	 (len (length str))
	 (i 0))
    (while (and (< i len) (< idx string-len))
      (aset string idx (aref str i))
      (setq idx (1+ idx) i (1+ i)))
    string))

;; ### This function is not compatible with FSF in some cases.  Hard
;; to fix, because it is hard to trace the logic of the FSF function.
;; In case we need the exact behavior, we can always copy the FSF
;; version, which is very long and does lots of unnecessary stuff.
(defun truncate-string-to-width (str end-column &optional start-column padding)
  "Truncate string STR to end at column END-COLUMN.
The optional 2nd arg START-COLUMN, if non-nil, specifies
the starting column; that means to return the characters occupying
columns START-COLUMN ... END-COLUMN of STR.

The optional 3rd arg PADDING, if non-nil, specifies a padding character
to add at the end of the result if STR doesn't reach column END-COLUMN,
or if END-COLUMN comes in the middle of a character in STR.
PADDING is also added at the beginning of the result
if column START-COLUMN appears in the middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN."
  (or start-column
      (setq start-column 0))
  (let ((len (length str)))
    (concat (substring str (min start-column len) (min end-column len))
	    (and padding (> end-column len)
		 (make-string (- end-column len) padding)))))

(defalias 'truncate-string 'truncate-string-to-width)
(make-obsolete 'truncate-string 'truncate-string-to-width)

;; Keywords already do The Right Thing in XEmacs
(make-compatible 'define-widget-keywords "Just use them")

(make-obsolete 'function-called-at-point 'function-at-point)

;;; obsolete.el ends here
