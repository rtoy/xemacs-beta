;;!emacs
;;
;; FILE:         hmouse-mod.el
;; SUMMARY:      Action Key acts as CONTROL modifier and Assist Key as META modifier.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PPG
;;
;; ORIG-DATE:     8-Oct-92 at 19:08:31
;; LAST-MOD:     14-Apr-95 at 16:06:26 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1992-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   This module is meant to be used with a chord keyboard in one hand for
;;   typing and a mouse in the other.  It requires that Hyperbole be loaded
;;   in order to work.  Hyperbole defines two Smart Keys, the Action Key and
;;   the Assist Key, on the middle and right buttons by default.
;;
;;   If the Action Key is held down while alpha characters are typed,
;;   they are translated into Control keys instead.  The Assist
;;   Key translates them into Meta keys.  When both Smart Keys
;;   are depressed, Control-Meta keys are produced.  The commands bound
;;   to the characters produced are then run.
;;
;;   So the Smart Keys modify the keys typed, e.g. Action Key + {a}
;;   runs the function for {C-a}.
;;
;;   If no keys are typed while the Smart Keys are down, they operate as
;;   normally under Hyperbole.
;;
;;   TO INVOKE:
;;
;;       (hmouse-mod-set-global-map)
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hyperbole)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-mod-global-map nil
  "Global key map installed by hmouse-mod-set-global-map function.
Translates self-insert-command characters into control and meta characters if
the Action or Assist Keys are depressed at the time of key press.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-mod-insert-command (count)
  "Surrogate function for self-insert-command.  Accounts for modifier Smart Keys."
  (interactive "p")
  (if (and (boundp 'action-key-depressed-flag)
	   (boundp 'assist-key-depressed-flag))
      (cond ((and action-key-depressed-flag assist-key-depressed-flag)
	     (setq action-key-cancelled t
		   assist-key-cancelled t)
	     (let* ((c (downcase last-command-char))
		    (key (char-to-string (+ 128 (% (- c ?\`) 128)))))
	       (if (and (or (= c ?\C-@)
			    (>= c ?a) (<= c ?z)))
		   (hmouse-mod-execute-command key)
		 (beep)))
	     )
	    ;; Control keys
	    (action-key-depressed-flag
	      (setq action-key-cancelled t)
	      (let ((c (downcase last-command-char)))
		(if (and (or (= c ?\C-@)
			     (>= c ?a) (<= c ?z)))
		    (hmouse-mod-execute-command
		      (char-to-string (- c ?\`)))
		  (beep)))
	      )
	    ;; Meta keys
	    (assist-key-depressed-flag
	      (setq assist-key-cancelled t)
	      (hmouse-mod-execute-command
		(char-to-string (+ 128 (% last-command-char 128))))
	      )
	    (t (call-interactively 'self-insert-command)))
    (call-interactively 'self-insert-command))
  )

(defun hmouse-mod-keyboard-quit ()
  "Surrogate function for keyboard-quit.  Cancels any hmouse-mod-prefix."
  (interactive)
  (setq hmouse-mod-prefix nil)
  (keyboard-quit))

(defun hmouse-mod-set-global-map ()
  "Creates 'hmouse-mod-global-map' and installs as current global map.
It accounts for modifier Smart Keys."
  (interactive)
  (setq hmouse-mod-global-map (copy-keymap global-map))
  (substitute-key-definition
    'self-insert-command 'hmouse-mod-insert-command hmouse-mod-global-map)
  (substitute-key-definition
    'keyboard-quit 'hmouse-mod-keyboard-quit hmouse-mod-global-map)
  (use-global-map hmouse-mod-global-map))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hmouse-mod-execute-command (key)
  "Executes command associated with keyboard KEY or if KEY prefix, records it."
  (setq key (concat hmouse-mod-prefix key))
  (let ((binding (key-binding key)))
    (cond ((and (not (or (vectorp binding) (stringp binding)))
		(commandp binding))
	   (if (> (length key) 1)
	       (or noninteractive (message (key-description key))))
	   (setq hmouse-mod-prefix nil)
	   (call-interactively binding))
	  ((symbolp binding)
	   (setq hmouse-mod-prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} not bound to a command."
		  (key-description key)))
	  ((integerp binding)
	   (setq hmouse-mod-prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} invalid key sequence."
		  (key-description key)))
	  (t (or noninteractive (message (key-description key)))
	     (setq hmouse-mod-prefix key)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hmouse-mod-prefix nil
  "Prefix key part of current key sequence.")

(provide 'hmouse-mod)
