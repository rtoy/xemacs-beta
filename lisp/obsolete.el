;;; obsolete.el --- obsoleteness support

;; Copyright (C) 1985-1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2002, 2004 Ben Wing.

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

Note: Use this before any other references (defvar/defcustom) to NEWVAR."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; events

(define-obsolete-function-alias 'menu-event-p 'misc-user-event-p)
(make-obsolete-variable 'unread-command-char 'unread-command-events)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; extents

(make-obsolete 'set-window-dot 'set-window-point)

(define-obsolete-function-alias 'extent-buffer 'extent-object)
(define-compatible-variable-alias 'parse-sexp-lookup-properties
  'lookup-syntax-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; frames
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

(make-compatible 'frame-parameter 'frame-property)
(defun frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.
If FRAME is nil, describe the currently selected frame."
  (cdr (assq parameter (frame-parameters frame))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; paths

(defvar Info-default-directory-list nil
  "This used to be the initial value of Info-directory-list.
If you want to change the locations where XEmacs looks for info files,
set Info-directory-list.")
(make-obsolete-variable 'Info-default-directory-list 'Info-directory-list)

(defvar init-file-user nil
  "This used to be the name of the user whose init file was read at startup.")
(make-obsolete-variable 'init-file-user 'load-user-init-file-p)

(define-obsolete-function-alias 'pui-add-install-directory
  'pui-set-local-package-get-directory) ; misleading name
;;;;;;;;;;;;;;;;;;;;;;;;;;;; hooks

(make-compatible-variable 'lisp-indent-hook 'lisp-indent-function)
(make-compatible-variable 'comment-indent-hook 'comment-indent-function)
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
  (or menu-name (error "must specify a menu name"))
  (or menu-items (error "must specify some menu items"))
  (add-submenu menu-path (cons menu-name menu-items) before))
;; Can't make this obsolete.  easymenu depends on it.
(make-compatible 'add-menu 'add-submenu)

(define-obsolete-function-alias 'package-get-download-menu 
  'package-ui-download-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; minibuffer

(define-compatible-function-alias 'read-minibuffer
  'read-expression) ; misleading name
(define-compatible-function-alias 'read-input 'read-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc

;; (defun user-original-login-name ()
;;   "Return user's login name from original login.
;; This tries to remain unaffected by `su', by looking in environment variables."
;;   (or (getenv "LOGNAME") (getenv "USER") (user-login-name)))
(define-obsolete-function-alias 'user-original-login-name 'user-login-name)

; old names
(define-obsolete-function-alias 'show-buffer 'set-window-buffer)
(define-obsolete-function-alias 'buffer-flush-undo 'buffer-disable-undo)
(make-compatible 'eval-current-buffer 'eval-buffer)
(define-compatible-function-alias 'byte-code-function-p
  'compiled-function-p) ;FSFmacs

(define-obsolete-function-alias 'isearch-yank-x-selection
  'isearch-yank-selection)
(define-obsolete-function-alias 'isearch-yank-x-clipboard
  'isearch-yank-clipboard)

;; too bad there's not a way to check for aref, assq, and nconc
;; being called on the values of functions known to return keymaps,
;; or known to return vectors of events instead of strings...

(make-obsolete-variable 'executing-macro 'executing-kbd-macro)

(define-compatible-function-alias 'interactive-form 
  'function-interactive) ;GNU 21.1
(define-compatible-function-alias 'assq-delete-all
  'remassq) ;GNU 21.1

(defun makehash (&optional test)
  "Create a new hash table.
Optional first argument TEST specifies how to compare keys in the table.  
Predefined tests are `eq', `eql', and `equal'.  Default is `eql'."
  (make-hash-table :test test))
(make-compatible 'makehash 'make-hash-table)

(defun buffer-local-value (variable buffer)
  "Return the value of VARIABLE in BUFFER.
If VARIABLE does not have a buffer-local binding in BUFFER, the value
is the default binding of variable."
  (symbol-value-in-buffer variable buffer))
(make-compatible 'buffer-local-value 'symbol-value-in-buffer)

(define-compatible-function-alias 'line-beginning-position 'point-at-bol)
(define-compatible-function-alias 'line-end-position 'point-at-eol)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; redisplay

(defun redraw-display (&optional device)
  (if (eq device t)
      (mapcar 'redisplay-device (device-list))
    (redisplay-device device)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; strings

(define-obsolete-function-alias 'sref 'aref)

(defun char-bytes (character)
  "Return number of bytes a CHARACTER occupies in a string or buffer.
It always returns 1 in XEmacs, and in recent FSF Emacs versions."
  1)
(make-obsolete 'char-bytes "This function always returns 1")

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii.
It might be available for compatibility with Mule 2.3,
because its `find-charset-string' ignores ASCII charset."
  (delq 'ascii (charsets-in-string string)))
(make-obsolete 'find-non-ascii-charset-string
	       "use (delq 'ascii (charsets-in-string STRING)) instead.")

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii in the region between START and END.
It might be available for compatibility with Mule 2.3,
because its `find-charset-string' ignores ASCII charset."
  (delq 'ascii (charsets-in-region start end)))
(make-obsolete 'find-non-ascii-charset-region
	       "use (delq 'ascii (charsets-in-region START END)) instead.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; window-system objects

;; the functionality of column.el has been moved into C
;; Function obsoleted for XEmacs 20.0/February 1997.
(defalias 'display-column-mode 'column-number-mode)

(defun x-color-values  (color &optional frame)
  "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--(RED GREEN BLUE).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is (65280 65280 65280) or (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
  (color-instance-rgb-components (make-color-instance color)))
(make-compatible 'x-color-values 'color-instance-rgb-components)

(make-obsolete 'mswindows-color-list 'color-list)
(make-obsolete 'tty-color-list 'color-list)
(make-compatible 'list-fonts 'font-list)

;; Two loser functions which shouldn't be used.
(make-obsolete 'following-char 'char-after)
(make-obsolete 'preceding-char 'char-before)

;; Keywords already do The Right Thing in XEmacs
(make-compatible 'define-widget-keywords "Just use them")

(make-obsolete 'function-called-at-point 'function-at-point)

(provide 'obsolete)
;;; obsolete.el ends here
