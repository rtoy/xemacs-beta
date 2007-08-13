;; @(#) crisp.el -- CRiSP/Brief Emacs emulator

;; Author: Gary D. Foster <Gary.Foster@corp.sun.com>
;; Created: 01 Mar 1996
;; Version: 1.26
;; Keywords: emulations brief crisp
;; X-Modified-by:
;;	crisp.el,v
;;	Revision 1.26  1997/11/18 05:41:02  gfoster
;;	Added several new keybindings:
;;		C-home	top of window
;;		C-end	bottom of window
;;		M-home	beginning of line
;;		M-end	end-of-line
;;		C-F	format region
;;		M-l	mark line
;;		M-m	set mark
;;	Added crisp-version function
;;
;;	Revision 1.25  1997/11/18 04:19:09  gfoster
;;	Shortened the version numbering, removed the release-version tracking
;;
;;	Revision 1.24  1997/11/18 04:15:54  gfoster
;;	Added `crisp-submit-bug-report' (shamelessly cribbed from Barry's
;;	cc-mode.  Thanks Barry!)
;;
;;	Bound the above to C-c b
;;
;;	Changed the behavior of `crisp-(kill|copy)-line' so (kill|copy)ing
;;	works on the region from point to eol instead of the entire line, when
;;	a region is not highlighted.
;;
;;	Revision 1.23  1997/11/11 19:47:02  gfoster
;;	Merged changes suggested by Hrvoje Niksic
;;	   make crisp-mode-map a sparse keymap parented from current-global-map
;;	   don't copy the keymap in (crisp-mode-original-keymap)
;;	   declare last-last-command to shut up the byte-compiler
;;	   make (crisp-mode) honor ARG
;;
;;	Revision 1.22  1997/11/11 19:37:44  gfoster
;;	kp-add/minus now copy/kill the current line if there is no highlighted
;;	region.  These also honor the universal prefix argument conventions.
;;
;;	Revision 1.21  1997/10/16 18:52:54  gfoster
;;	Fixed bogus XEmacs/Lucid string-match checking
;;	made modeline entry mouse2-able
;;
;;	Revision 1.20  1997/08/22 18:49:11  gfoster
;;	Added next-buffer/previous-buffer keybindings (bound to M-n/M-p)
;;	Added crisp-unbury-buffer function
;;	Standardized headers for Steve
;;

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
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; CRiSP is a registered trademark of Foxtrot Systems Ltd.

;;; Commentary:

;; Keybindings and minor functions to duplicate the functionality and
;; finger-feel of the CRiSP/Brief editor.  This package is designed to
;; facilitate transitioning from Brief to (XE|E)macs with a minimum
;; amount of hassles.

;; Enable this package by putting (require 'crisp) in your .emacs and
;; use M-x crisp-mode to toggle it on or off.

;; This package will automatically load the scroll-lock.el package if
;; you put (setq crisp-load-scroll-lock t) in your .emacs before
;; loading this package.  If this feature is enabled, it will bind
;; meta-f1 to the scroll-lock mode toggle.  The scroll-lock package
;; duplicates the scroll-locking feature in CRiSP.

;; Also, the default keybindings for brief/CRiSP override the M-x
;; key to exit the editor.  If you don't like this functionality, you
;; can prevent this behavior (or redefine it dynamically) by setting
;; the value of `crisp-override-meta-x' either in your .emacs or
;; interactively.  The default setting is nil, which means that M-x will
;; by default run `execute-extended-command' instead of the command
;; `save-buffers-kill-emacs'.

;; Finally, if you want to change the string displayed in the modeline
;; when this mode is in effect, override the definition of
;; `crisp-mode-modeline-string' in your .emacs.  The default value is
;; " *Crisp*" which may be a bit lengthy if you have a lot of things
;; being displayed there.

;; All these overrides should go *before* the (require 'crisp) statement.

;; Code:

(require 'cl)

;; local variables

(defgroup crisp nil
  "CRiSP emulator customizable settings."
  :group 'emulations)

(defvar crisp-mode-map (let ((map (make-sparse-keymap)))
			 (set-keymap-parent map (current-global-map))
			 map)
  "Local keymap for CRiSP emulation mode.
All the bindings are done here instead of globally to try and be
nice to the world.")

(defcustom crisp-mode-modeline-string " *CRiSP*"
  "*String to display in the modeline when CRiSP emulation mode is enabled."
  :type 'string
  :group 'crisp)

(defvar crisp-mode-original-keymap (current-global-map)
  "The original keymap before CRiSP emulation mode remaps anything.
This keymap is restored when CRiSP emulation mode is disabled.")

(defvar crisp-mode-enabled nil
  "Track status of CRiSP emulation mode.
A value of nil means CRiSP mode is not enabled.  A value of t
indicates CRiSP mode is enabled.")

(defcustom crisp-override-meta-x t
  "*Controls overriding the normal Emacs M-x key binding in the CRiSP emulator.
Normally the CRiSP emulator rebinds M-x to save-buffers-exit-emacs and
provides the usual M-x functionality on the F10 key.  If this variable
is non-nil, M-x will exit Emacs."
  :type 'boolean
  :group 'crisp)

(defvar crisp-load-scroll-lock nil
  "Controls loading of the Scroll Lock in the CRiSP emulator.
Its Default behavior is to load and enable the Scroll Lock minor mode
package when enabling the CRiSP emulator.

If this variable is nil when you start the CRiSP emulator, it
does not load the scroll-lock package.")

(defvar crisp-load-hook nil
  "Hooks to run after loading the CRiSP emulator package.")

(defconst crisp-version "1.26"
  "The version of the CRiSP emulator.")

(defconst crisp-mode-help-address "gfoster@suzieq.ragesoft.com, Gary.Foster@corp.Sun.COM"
  "The email address of the CRiSP mode author/maintainer.")

;; Silence the byte-compiler.
(defvar last-last-command)

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
(define-key crisp-mode-map [(meta f5)]      'search-backward-regexp)

(define-key crisp-mode-map [(f6)]           'query-replace)

(define-key crisp-mode-map [(f7)]           'start-kbd-macro)
(define-key crisp-mode-map [(meta f7)]      'end-kbd-macro)

(define-key crisp-mode-map [(f8)]           'call-last-kbd-macro)
(define-key crisp-mode-map [(meta f8)]      'save-kbd-macro)

(define-key crisp-mode-map [(f9)]           'find-file)
(define-key crisp-mode-map [(meta f9)]      'load-library)

(define-key crisp-mode-map [(f10)]          'execute-extended-command)
(define-key crisp-mode-map [(meta f10)]     'compile)

(define-key crisp-mode-map [(SunF37)]       'kill-buffer)
(define-key crisp-mode-map [(kp-add)]       'crisp-copy-line)
(define-key crisp-mode-map [(kp-subtract)]  'crisp-kill-line)
(define-key crisp-mode-map [(insert)]       'x-yank-clipboard-selection)
(define-key crisp-mode-map [(f16)]          'x-copy-primary-selection) ; copy on Sun5 kbd
(define-key crisp-mode-map [(f20)]          'x-kill-primary-selection) ; cut on Sun5 kbd 
(define-key crisp-mode-map [(f18)]          'x-yank-clipboard-selection) ; paste on Sun5 kbd

(define-key crisp-mode-map [(control f)]    'fill-paragraph-or-region)
(define-key crisp-mode-map [(meta d)]       (lambda ()
					      (interactive)
					      (beginning-of-line) (kill-line)))
(define-key crisp-mode-map [(meta e)]       'find-file)
(define-key crisp-mode-map [(meta g)]       'goto-line)
(define-key crisp-mode-map [(meta h)]       'help)
(define-key crisp-mode-map [(meta i)]       'overwrite-mode)
(define-key crisp-mode-map [(meta j)]       'bookmark-jump)
(define-key crisp-mode-map [(meta l)]       'crisp-mark-line)
(define-key crisp-mode-map [(meta m)]       'set-mark-command)
(define-key crisp-mode-map [(meta n)]       'bury-buffer)
(define-key crisp-mode-map [(meta p)]       'crisp-unbury-buffer)
(define-key crisp-mode-map [(meta u)]       'advertised-undo)
(define-key crisp-mode-map [(f14)]          'advertised-undo)
(define-key crisp-mode-map [(meta w)]       'save-buffer)
(define-key crisp-mode-map [(meta x)]       'crisp-meta-x-wrapper)
(define-key crisp-mode-map [(meta ?0)]      (lambda ()
					      (interactive)
					      (bookmark-set "0")))
(define-key crisp-mode-map [(meta ?1)]      (lambda ()
					      (interactive)
					      (bookmark-set "1")))
(define-key crisp-mode-map [(meta ?2)]      (lambda ()
					      (interactive)
					      (bookmark-set "2")))
(define-key crisp-mode-map [(meta ?3)]      (lambda ()
					      (interactive)
					      (bookmark-set "3")))
(define-key crisp-mode-map [(meta ?4)]      (lambda ()
					      (interactive)
					      (bookmark-set "4")))
(define-key crisp-mode-map [(meta ?5)]      (lambda ()
					      (interactive)
					      (bookmark-set "5")))
(define-key crisp-mode-map [(meta ?6)]      (lambda ()
					      (interactive)
					      (bookmark-set "6")))
(define-key crisp-mode-map [(meta ?7)]      (lambda ()
					      (interactive)
					      (bookmark-set "7")))
(define-key crisp-mode-map [(meta ?8)]      (lambda ()
					      (interactive)
					      (bookmark-set "8")))
(define-key crisp-mode-map [(meta ?9)]      (lambda ()
					      (interactive)
					      (bookmark-set "9")))

(define-key crisp-mode-map [(shift right)]     'fkey-forward-word)
(define-key crisp-mode-map [(shift left)]      'fkey-backward-word)
(define-key crisp-mode-map [(shift delete)]    'kill-word)
(define-key crisp-mode-map [(shift backspace)] 'backward-kill-word)
(define-key crisp-mode-map [(control left)]    'backward-word)
(define-key crisp-mode-map [(control right)]   'forward-word)

(define-key crisp-mode-map [(home)]            'crisp-home)
(define-key crisp-mode-map [(control home)]    (lambda ()
						 (interactive)
						 (move-to-window-line 0)))
(define-key crisp-mode-map [(meta home)]       'beginning-of-line)
(define-key crisp-mode-map [(end)]             'crisp-end)
(define-key crisp-mode-map [(control end)]     (lambda ()
						 (interactive)
						 (move-to-window-line -1)))
(define-key crisp-mode-map [(meta end)]        'end-of-line)

(define-key crisp-mode-map [(control c) (b)]   'crisp-submit-bug-report)

(defun crisp-version (&optional arg)
  "Version number of the CRiSP emulator package.
If ARG, insert results at point."
  (interactive "P")
  (let ((foo (concat "CRiSP version " crisp-version)))
    (if arg
	(insert (message foo))
      (message foo))))

(defun crisp-mark-line (arg)
  "Put mark at the end of line.  Arg works as in `end-of-line'."
  (interactive "p")
  (mark-something 'crisp-mark-line 'end-of-line arg))

(defun crisp-kill-line (arg)
  "Mark and kill line(s).
Marks from point to end of the current line (honoring prefix arguments),
copies the region to the kill ring and clipboard, and then deletes it."
  (interactive "*p")
  (if zmacs-region-active-p
      (x-kill-primary-selection)
    (crisp-mark-line arg)
    (x-kill-primary-selection)))

(defun crisp-copy-line (arg)
  "Mark and copy line(s).
Marks from point to end of the current line (honoring prefix arguments),
copies the region to the kill ring and clipboard, and then deactivates
the region."
  (interactive "*p")
  (let ((curpos (point)))
    (if zmacs-region-active-p
	(x-copy-primary-selection)
      (crisp-mark-line arg)
      (x-copy-primary-selection)
      (goto-char curpos))))

(defun crisp-home ()
  "\"Home\" the point, the way CRiSP would do it.
The first use moves point to beginning of the line.  Second
consecutive use moves point to beginning of the screen.  Third
consecutive use moves point to the beginning of the buffer."
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
  "\"End\" the point, the way CRiSP would do it.
The first use moves point to end of the line.  Second
consecutive use moves point to the end of the screen.  Third
consecutive use moves point to the end of the buffer."
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

(defun crisp-unbury-buffer ()
  "Go back one buffer"
  (interactive)
  (switch-to-buffer (car (last (buffer-list)))))
 
(defun crisp-meta-x-wrapper ()
  "Wrapper function to conditionally override the normal M-x bindings.
When `crisp-override-meta-x' is non-nil, M-x will exit Emacs (the
normal CRiSP binding) and when it is nil M-x will run
`execute-extended-command' (the normal Emacs binding)."
  (interactive)
  (if crisp-override-meta-x
      (save-buffers-kill-emacs)
    (call-interactively 'execute-extended-command)))

;; bug reporter

(defun crisp-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
  (require 'cc-vars)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist))
	(style c-indentation-style)
	(hook c-special-indent-hook)
	(c-features c-emacs-features))
    (and
     (if (y-or-n-p "Do you want to submit a report on CRiSP Mode? ")
	 t (message "") nil)
     (require 'reporter)
     (reporter-submit-bug-report
      crisp-mode-help-address
      (concat "CRiSP Mode [" crisp-version "]")
      nil
      nil
      nil
      "Dear Gary,"
      ))))

;; Now enable the mode

(defun crisp-mode (&optional arg)
  "Toggle CRiSP emulation minor mode.
With ARG, turn CRiSP mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq crisp-mode-enabled (if (null arg)
			       (not crisp-mode-enabled)
			     (> (prefix-numeric-value arg) 0)))
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

(if (fboundp 'add-minor-mode)
    (add-minor-mode 'crisp-mode-enabled 'crisp-mode-modeline-string
		    nil nil 'crisp-mode)
  (or (assq 'crisp-mode-enabled minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(crisp-mode-enabled crisp-mode-modeline-string) minor-mode-alist))))

(provide 'crisp)

;;; crisp.el ends here
