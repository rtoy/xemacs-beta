;; @(#) crisp.el -- Crisp/Brief Emacs emulator

;; Author: Gary D. Foster <Gary.Foster@corp.sun.com>
;; $Revision: 1.1.1.2 $
;; Keywords: emulations brief crisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Keybindings and minor functions to duplicate the functionality and
;; finger-feel of the Crisp/Brief editor.  This package is designed to
;; facilitate transitioning from Brief to (XE|E)macs with a minimum
;; amount of hassles.

;; Enable this package by putting the following in your .emacs
;; (require 'crisp)
;; and use M-x crisp-mode to toggle it on or off.

;; This package will automatically default to loading the scroll-lock.el
;; package unless you put (setq crisp-load-scroll-lock nil) in your
;; .emacs.  If this feature is enabled, it will bind meta-f1 to the
;; scroll-lock mode toggle.

;; Also, the default keybindings for brief override the meta-x key to
;; exit the editor.  If you don't like this functionality, you can
;; prevent this key from being rebound with
;; (setq crisp-override-meta-x nil) in your .emacs.

;; Finally, if you want to change the string displayed in the modeline
;; when this mode is in effect, override the definition of
;; `crisp-mode-modeline-string' in your .emacs.  The default value is
;; " *Crisp*" which may be a bit lengthy if you have a lot of things
;; being displayed there.

;; All these overrides should go *before* the (require 'crisp) statement.

;; local variables

(defvar crisp-mode-map (copy-keymap (current-global-map))
  "Local keymap for Crisp mode.
All the bindings are done here instead of globally to try and be
nice to the world.")

(defvar crisp-mode-modeline-string " *Crisp*"
  "String to display in the modeline when Crisp mode is enabled.")

(defvar crisp-mode-original-keymap (copy-keymap (current-global-map))
  "The original keymap before Crisp mode remaps anything.
This keymap is restored when Crisp mode is disabled.")

(defvar crisp-mode-enabled 'nil
  "Track status of Crisp mode.
A value of nil means Crisp mode is not enabled.  A value of t
indicates Crisp mode is enabled.")

(defvar crisp-override-meta-x 't
  "Controls overriding the normal Emacs M-x key binding.
The normal binding for M-x is `execute-extended-command', however
the normal Crisp keybinding for M-x is to exit the editor, while
the F10 key is used to execute extended commands.  If you don't
want M-x to dump you out of emacs, set this to nil before loading
the package.")

(defvar crisp-load-scroll-lock 't
  "Controls loading of the Scroll Lock minor mode package.
Default behavior is to load the scroll lock minor mode
package when Crisp mode is enabled.  Set to nil prior
to loading this package to prevent it.")

(defvar crisp-load-hook nil
  "Hooks to run after Crisp mode is enabled.")

(defvar crisp-mode-running-xemacs (string-match "XEmacs\\Lucid" emacs-version))

(if crisp-mode-running-xemacs
    (add-minor-mode 'crisp-mode-enabled crisp-mode-modeline-string)
  (or (assq 'crisp-mode-enabled minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(crisp-mode-enabled crisp-mode-modeline-string) minor-mode-alist))))

;; and now the keymap defines

(define-key crisp-mode-map [(f1)]           'other-window)

(define-key crisp-mode-map [(f2) (down)]    'enlarge-window)
(define-key crisp-mode-map [(f2) (left)]    'shrink-window-horizontally)
(define-key crisp-mode-map [(f2) (right)]   'enlarge-window-horizontally)
(define-key crisp-mode-map [(f2) (up)]      'shrink-window)
(define-key crisp-mode-map [(f3) (down)]    'split-window-vertically)
(define-key crisp-mode-map [(f3) (right)]   'split-window-horizontally)

(define-key crisp-mode-map [(f4)]           'delete-window)
(define-key crisp-mode-map [(control f4)]   'delete-other-windows)

(define-key crisp-mode-map [(f5)]           'search-forward-regexp)
(define-key crisp-mode-map [(f19)]          'search-forward-regexp)
(define-key crisp-mode-map [(meta f5)]       'search-backward-regexp)

(define-key crisp-mode-map [(f6)]           'query-replace)

(define-key crisp-mode-map [(f7)]           'start-kbd-macro)
(define-key crisp-mode-map [(meta f7)]       'end-kbd-macro)

(define-key crisp-mode-map [(f8)]           'call-last-kbd-macro)
(define-key crisp-mode-map [(meta f8)]      'save-kbd-macro)

(define-key crisp-mode-map [(f9)]           'find-file)
(define-key crisp-mode-map [(meta f9)]       'load-library)

(define-key crisp-mode-map [(f10)]          'execute-extended-command)
(define-key crisp-mode-map [(meta f10)]      'compile)

(define-key crisp-mode-map [(SunF37)]          'kill-buffer)
(define-key crisp-mode-map [(kp-add)]       'x-copy-primary-selection)
(define-key crisp-mode-map [(kp-subtract)]  'x-kill-primary-selection)
(define-key crisp-mode-map [(insert)]       'x-yank-clipboard-selection)
(define-key crisp-mode-map [(f16)]          'x-copy-primary-selection) ; copy on Sun5 kbd
(define-key crisp-mode-map [(f20)]          'x-kill-primary-selection) ; cut on Sun5 kbd 
(define-key crisp-mode-map [(f18)]          'x-yank-clipboard-selection) ; paste on Sun5 kbd

(define-key crisp-mode-map [(meta d)]       (lambda () (interactive) (beginning-of-line) (kill-line)))
(define-key crisp-mode-map [(meta e)]       'find-file)
(define-key crisp-mode-map [(meta g)]       'goto-line)
(define-key crisp-mode-map [(meta h)]       'help)
(define-key crisp-mode-map [(meta i)]       'overwrite-mode)
(define-key crisp-mode-map [(meta u)]       'advertised-undo)
(define-key crisp-mode-map [(f14)]          'advertised-undo)
(define-key crisp-mode-map [(meta w)]       'save-buffer)
(if
 (eq crisp-override-meta-x 't)
  (define-key crisp-mode-map [(meta x)]       'save-buffers-kill-emacs))

(define-key crisp-mode-map [(shift right)]  'fkey-forward-word)
(define-key crisp-mode-map [(shift left)]   'fkey-backward-word)
(define-key crisp-mode-map [(shift delete)] 'kill-word)
(define-key crisp-mode-map [(shift backspace)] 'backward-kill-word)
(define-key crisp-mode-map [(control left)] 'backward-word)
(define-key crisp-mode-map [(control right)] 'forward-word)

(define-key crisp-mode-map [(home)] 'crisp-home)
(define-key crisp-mode-map [(end)] 'crisp-end)

(defun crisp-home ()
  "Home the point according to Crisp conventions.
First call to this moves point to beginning of the line.  Second
consecutive call moves point to beginning of the screen.  Third
consecutive call moves the point to the beginning of the buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'crisp-home) (eq last-last-command 'crisp-home))
     (goto-char (point-min)))
    ((eq last-command 'crisp-home)
     (move-to-window-line 0))
    (t
     (beginning-of-line)))
  (setq last-last-command last-command))

(defun crisp-end ()
  "End the point according to Crisp conventions.
First call to this moves point to end of the line.  Second
consecutive call moves point to the end of the screen.  Third
consecutive call moves point to the end of the buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'crisp-end) (eq last-last-command 'crisp-end))
     (goto-char (point-max)))
    ((eq last-command 'crisp-end)
     (move-to-window-line -1)
     (end-of-line))
    (t
     (end-of-line)))
  (setq last-last-command last-command))

;; Now enable the mode

(defun crisp-mode ()
  "Toggle Crisp minor mode."
  (interactive nil)
  (setq crisp-mode-enabled (not crisp-mode-enabled))
  (cond
   ((eq crisp-mode-enabled 't)
    (use-global-map crisp-mode-map)
    (if crisp-load-scroll-lock
	(require 'scroll-lock))
    (if (featurep 'scroll-lock)
	(define-key crisp-mode-map [(meta f1)] 'scroll-lock-mode))
    (run-hooks 'crisp-load-hook))
   ((eq crisp-mode-enabled 'nil)
    (use-global-map crisp-mode-original-keymap))))

(provide 'crisp)

;;; crisp.el ends here
