;;; mic-paren.el --- highlight matching paren.
;;; Version 1.0 - 96-08-16
;;; Copyright (C) 1996 Mikael Sjödin (mic@docs.uu.se)
;;;
;;; Author: Mikael Sjödin  --  mic@docs.uu.se
;;; Keywords: languages, faces
;;;
;;; This file is NOT part of GNU Emacs.
;;; You may however redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software Foundation; either
;;; version 2, or (at your option) any later version.
;;;
;;; mic-paren is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; ----------------------------------------------------------------------
;;; Short Description:
;;;
;;; Load this file and Emacs will display highlighting on whatever
;;; parenthesis matches the one before or after point.  This is an extension to
;;; the paren.el file distributed with Emacs.  The default behaviour is similar
;;; to paren.el but try the authors favourite options:
;;;   (setq paren-face 'bold)
;;;   (setq paren-sexp-mode t)

;;; ----------------------------------------------------------------------
;;; Installation:
;;;
;;; o Place this file in a directory in your 'load-path.
;;; o Put the following in your .emacs file:
;;;     (if window-system
;;;         (require 'mic-paren))
;;; o Restart your Emacs. mic-paren is now installed and activated!
;;; o To list the possible customisation enter `C-h f paren-activate'

;;; ----------------------------------------------------------------------
;;; Long Description:
;;;
;;; mic-paren.el is an extension to the packages paren.el and stig-paren.el for
;;; Emacs.  When mic-paren is active (it is activated when loaded) Emacs normal
;;; parenthesis matching is deactivated.  Instead parenthesis matching will be
;;; performed as soon as the cursor is positioned at a parenthesis.  The
;;; matching parenthesis (or the entire expression between the parenthesises)
;;; is highlighted until the cursor is moved away from the parenthesis.
;;; Features include:
;;; o Both forward and backward parenthesis matching (_simultaneously_ if
;;;   cursor is between two expressions).
;;; o Indication of mismatched parenthesises.
;;; o Option to select if only the matching parenthesis or the entire
;;;   expression should be highlighted.
;;; o Message describing the match when the matching parenthesis is
;;;   off-screen. 
;;; o Optional delayed highlighting (useful on slow systems), 
;;; o Functions to activate/deactivate mic-paren.el is provided.
;;; o Numerous options to control the behaviour and appearance of
;;;   mic-paren.el. 
;;;
;;; mic-paren.el is developed and tested under Emacs 19.28 - 19.31.  It should
;;; work on earlier and forthcoming Emacs versions.
;;;
;;; This file can be obtained from http://www.docs.uu.se/~mic/emacs.html

;; Ported to XEmacs 15-September, 1996 Steve Baur <steve@miranova.com>
;;; ======================================================================
;;; User Options:

(defvar paren-priority nil
  "*Defines the behaviour of mic-paren when point is between a closing and an
  opening parenthesis.

A value of 'close means highlight the parenthesis matching the
close-parenthesis before the point.

A value of 'open means highlight the parenthesis matching the open-parenthesis
after the point.

Any other value means highlight both parenthesis matching the parenthesis
beside the point.")


;;; ------------------------------

(defvar paren-sexp-mode nil
  "*If nil only the matching parenthesis is highlighted.
If non-nil the whole s-expression between the matching parenthesis is
highlighted.")

;;; ------------------------------

(defvar paren-highlight-at-point t
  "*If non-nil and point is after a close parenthesis, both the close and
open parenthesis is highlighted. If nil, only the open parenthesis is
highlighted.")

;;; ------------------------------

(defvar paren-highlight-offscreen nil
  "*If non-nil stig-paren will highlight text which is not visible in the
current buffer.  

This is useful if you regularly display the current buffer in multiple windows
or frames. For instance if you use follow-mode (by andersl@csd.uu.se), however
it may slow down your Emacs.

(This variable is ignored (treated as non-nil) if you set paren-sexp-mode to
non-nil.)")

;;; ------------------------------

(defvar paren-message-offscreen t
  "*Display message if matching parenthesis is off-screen.")

;;; ------------------------------

(defvar paren-message-no-match t
  "*Display message if no matching parenthesis is found.")

;;; ------------------------------

(defvar paren-ding-unmatched nil
  "*Make noise if the cursor is at an unmatched parenthesis or no matching
parenthesis is found.

Even if nil, typing an unmatched parenthesis produces a ding.")

;;; ------------------------------

(defvar paren-delay nil
  "*This variable controls when highlighting is done.  The variable has
different meaning in different versions of Emacs.

In Emacs 19.29 and below: 
  This variable is ignored.

In Emacs 19.30:
  A value of nil will make highlighting happen immediately (this may slow down
  your Emacs if running on a slow system).  Any non-nil value will delay
  highlighting for the time specified by post-command-idle-delay.  

In Emacs 19.31 and above:
  A value of nil will make highlighting happen immediately (this may slow down
  your Emacs if running on a slow system).  If not nil, the value should be a
  number (possible a floating point number if your Emacs support floating point
  numbers).  The number is the delay before mic-paren performs highlighting.

If you change this variable when mic-paren is active you have to re-activate
(with M-x paren-activate) mic-paren for the change to take effect.")


;;; ------------------------------

(defvar paren-dont-touch-blink nil
  "*If non-nil mic-paren will not change the value of blink-matching-paren when
activated of deactivated.

If nil mic-paren turns of blinking when activated and turns on blinking when
deactivated.")

;;; ------------------------------

(defvar paren-dont-activate-on-load nil
 "*If non-nil mic-paren will not activate itself when loaded.")

;;; ------------------------------

(defvar paren-face (if (x-display-color-p) 'highlight 'underline)
  "*Face to use for showing the matching parenthesis.")

;;; ------------------------------

(defvar paren-mismatch-face (if (x-display-color-p)
				(let ((fn 'paren-mismatch-face))
				  (copy-face 'default fn)
				  (set-face-background fn "DeepPink")
				  fn)
			      'modeline)
  "*Face to use when highlighting a mismatched parenthesis.")

;;; ======================================================================
;;; User Functions:

;; XEmacs compatibility
(eval-and-compile
  (if (fboundp 'make-extent)
      (progn
	(fset 'mic-make-overlay 'make-extent)
	(fset 'mic-delete-overlay 'delete-extent)
	(fset 'mic-overlay-put 'set-extent-property)
	(defun mic-cancel-timer (timer) (delete-itimer timer))
	(defun mic-run-with-idle-timer (secs repeat function &rest args)
	  (start-itimer "mic-paren-idle" function secs nil))
	)
    (fset 'mic-make-overlay 'make-overlay)
    (fset 'mic-delete-overlay 'delete-overlay)
    (fset 'mic-overlay-put 'overlay-put)
    (fset 'mic-cancel-timer 'cancel-timer)
    (fset 'mic-run-with-idle-timer 'run-with-idle-timer)
    ))


(defun paren-activate ()
  "Activates mic-paren parenthesis highlighting.
paren-activate deactivates the paren.el and stig-paren.el packages if they are
active 
Options:
  paren-priority
  paren-sexp-mode
  paren-highlight-at-point
  paren-highlight-offscreen
  paren-message-offscreen
  paren-message-no-match
  paren-ding-unmatched
  paren-delay
  paren-dont-touch-blink
  paren-dont-activate-on-load
  paren-face
  paren-mismatch-face"
  (interactive)
  ;; Deactivate mic-paren.el (To remove redundant hooks)
  (paren-deactivate)
  ;; Deactivate paren.el if loaded
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'show-paren-command-hook))
  (remove-hook 'post-command-hook 'show-paren-command-hook)
  (and (boundp 'show-paren-overlay)
       show-paren-overlay
       (mic-delete-overlay show-paren-overlay))
  (and (boundp 'show-paren-overlay-1)
       show-paren-overlay-1
       (mic-delete-overlay show-paren-overlay-1))
  ;; Deactivate stig-paren.el if loaded
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'stig-paren-command-hook))
  (remove-hook 'post-command-hook 'stig-paren-command-hook)
  (remove-hook 'post-command-hook 'stig-paren-safe-command-hook)
  (remove-hook 'pre-command-hook 'stig-paren-delete-overlay)
  ;; Deactivate Emacs standard parenthesis blinking
  (or paren-dont-touch-blink
      (setq blink-matching-paren nil))

  (cond
	;; If timers are available use them
	;; (Emacs 19.31 and above)
	((or (featurep 'timer) (featurep 'itimer))
	 (if (numberp paren-delay)
	     (setq mic-paren-idle-timer 
		   (mic-run-with-idle-timer paren-delay t
					    'mic-paren-command-idle-hook))
	   (add-hook 'post-command-hook 'mic-paren-command-hook)))
       ;; If the idle hook exists assume it is functioning and use it 
       ;; (Emacs 19.30)
       ((and (boundp 'post-command-idle-hook) 
	     (boundp 'post-command-idle-delay))
	(if paren-delay
	    (add-hook 'post-command-idle-hook 'mic-paren-command-idle-hook)
	  (add-hook 'post-command-hook 'mic-paren-command-hook)))
       ;; Check if we (at least) have a post-comand-hook, and use it
       ;; (Emacs 19.29 and below)
       ((boundp 'post-command-hook) 
	(add-hook 'post-command-hook 'mic-paren-command-hook))
       ;; Not possible to install mic-paren hooks
       (t (error "Cannot activate mic-paren in this Emacs version"))))



(defun paren-deactivate ()
  "Deactivates mic-paren parenthesis highlighting"
  (interactive)
  ;; Deactivate (don't bother to check where/if mic-paren is acivte, just
  ;; delete all possible hooks and timers)
  (if (boundp 'post-command-idle-hook)
      (remove-hook 'post-command-idle-hook 'mic-paren-command-idle-hook))
  (if mic-paren-idle-timer
      (mic-cancel-timer mic-paren-idle-timer))
  (remove-hook 'post-command-hook 'mic-paren-command-hook)

  ;; Remove any old highlighs
  (mic-delete-overlay mic-paren-backw-overlay)
  (mic-delete-overlay mic-paren-point-overlay)
  (mic-delete-overlay mic-paren-forw-overlay)

  ;; Reactivate Emacs standard parenthesis blinking
  (or paren-dont-touch-blink
      (setq blink-matching-paren t))
  )

;;; ======================================================================
;;; Internal variables:

(defvar mic-paren-backw-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the open-paren which matches the close-paren before
point. When in sexp-mode this is the overlay for the expression before point.")

(defvar mic-paren-point-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the close-paren before point.
(Not used when is sexp-mode.)")

(defvar mic-paren-forw-overlay (mic-make-overlay (point-min) (point-min))
  "Overlay for the close-paren which matches the open-paren after
point. When in sexp-mode this is the overlay for the expression after point.")

(defvar mic-paren-idle-timer nil
  "Idle-timer.  Used only in Emacs 19.31 and above (and if paren-delay is nil)")




;;; ======================================================================
;;; Internal function:



(defun mic-paren-command-hook ()
  (or executing-kbd-macro
      (input-pending-p)			;[This might cause trouble since the
                                        ; function is unreliable]
      (condition-case paren-error
	  (mic-paren-highligt)
	(error 
	 (if (not (window-minibuffer-p (selected-window)))
	     (message "mic-paren catched error (please report): %s"
		      paren-error))))))

(defun mic-paren-command-idle-hook ()
  (condition-case paren-error
      (mic-paren-highligt)
    (error 
     (if (not (window-minibuffer-p (selected-window)))
	 (message "mic-paren catched error (please report): %s" 
		  paren-error)))))


(defun mic-paren-highligt ()
  "The main-function of mic-paren. Does all highlighting, dinging, messages,
cleaning-up."
  ;; Remove any old highlighting
  (mic-delete-overlay mic-paren-forw-overlay)
  (mic-delete-overlay mic-paren-point-overlay)
  (mic-delete-overlay mic-paren-backw-overlay)

  ;; Handle backward highlighting (when after a close-paren):
  ;; If positioned after a close-paren, and
  ;;    not before an open-paren when priority=open, and
  ;;    the close-paren is not escaped then
  ;;      perform highlighting
  ;; else
  ;;      remove any old backward highlights
  (if (and (eq (char-syntax (preceding-char)) ?\))
	   (not (and (eq (char-syntax (following-char)) ?\()
		     (eq paren-priority 'open)))
	   (paren-evenp (paren-backslashes-before-char (1- (point)))))
       (let (open)
	 ;; Find the position for the open-paren
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region 
		  (max (point-min)
		       (- (point) blink-matching-paren-distance))
		  (point-max)))
	     (condition-case ()
		 (setq open (scan-sexps (point) -1))
	       (error nil))))

	 ;; If match found
	 ;;    highlight and/or print messages
	 ;; else
	 ;;    print no-match message
	 ;;    remove any old highlights
	 (if open
	     (let ((mismatch (/= (matching-paren (preceding-char)) 
				 (char-after open)))
		   (visible (pos-visible-in-window-p open)))
	       ;; If highlight is appropriate
	       ;;    highligt
	       ;; else
	       ;;    remove any old highlight
	       (if (or visible paren-highlight-offscreen paren-sexp-mode)
		   ;; If sexp-mode
		   ;;    highlight sexp
		   ;; else
		   ;;    highlight the two parens
		   (if paren-sexp-mode
		       (progn
			 (setq mic-paren-backw-overlay
			       (mic-make-overlay open (point)))
			 (if mismatch
			     (mic-overlay-put mic-paren-backw-overlay 
					      'face paren-mismatch-face)
			   (mic-overlay-put mic-paren-backw-overlay 
					    'face paren-face)))
		     (setq mic-paren-backw-overlay
			   (mic-make-overlay open (1+ open)))
		     (and paren-highlight-at-point
			  (setq mic-paren-point-overlay
				(mic-make-overlay (1- (point)) (point))))
		     (if mismatch
			 (progn
			   (mic-overlay-put mic-paren-backw-overlay 
					    'face paren-mismatch-face)
			   (and paren-highlight-at-point
				(mic-overlay-put mic-paren-point-overlay 
						 'face paren-mismatch-face)))
		       (mic-overlay-put mic-paren-backw-overlay 
					'face paren-face)
		       (and paren-highlight-at-point 
			    (mic-overlay-put mic-paren-point-overlay 
					     'face paren-face)))))
	       ;; Print messages if match is offscreen
	       (and paren-message-offscreen
		    (not visible)
		    (not (window-minibuffer-p (selected-window)))
		    (message "%s %s" 
			     (if mismatch "MISMATCH:" "Matches")
			     (mic-paren-get-matching-open-text open)))
	       ;; Ding if mismatch
	       (and mismatch
		    paren-ding-unmatched
		    (ding)))
	   (and paren-message-no-match
		(not (window-minibuffer-p (selected-window)))
		(message "No opening parenthesis found"))
	   (and paren-message-no-match
		paren-ding-unmatched
		(ding)))))

  ;; Handle forward highlighting (when before an open-paren):
  ;; If positioned before an open-paren, and
  ;;    not after a close-paren when priority=close, and
  ;;    the open-paren is not escaped then
  ;;      perform highlighting
  ;; else
  ;;      remove any old forward highlights
  (if (and (eq (char-syntax (following-char)) ?\()
	   (not (and (eq (char-syntax (preceding-char)) ?\))
		     (eq paren-priority 'close)))
	   (paren-evenp (paren-backslashes-before-char (point))))
       (let (close)
	 ;; Find the position for the close-paren
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region 
		  (point-min)
		  (min (point-max)
		       (+ (point) blink-matching-paren-distance))))
      	     (condition-case ()
		 (setq close (scan-sexps (point) 1))
	       (error nil))))
	 ;; If match found
	 ;;    highlight and/or print messages
	 ;; else
	 ;;    print no-match message
	 ;;    remove any old highlights
	 (if close
	     (let ((mismatch (/= (matching-paren (following-char)) 
				 (char-after (1- close))))
		   (visible (pos-visible-in-window-p close)))
	       ;; If highlight is appropriate
	       ;;    highligt
	       ;; else
	       ;;    remove any old highlight
	       (if (or visible paren-highlight-offscreen paren-sexp-mode)
		   ;; If sexp-mode
		   ;;    highlight sexp
		   ;; else
		   ;;    highlight the two parens
		   (if paren-sexp-mode
		       (progn
			 (setq mic-paren-forw-overlay
			       (mic-make-overlay (point) close))
			 (if mismatch
			     (mic-overlay-put mic-paren-forw-overlay 
					      'face paren-mismatch-face)
			   (mic-overlay-put mic-paren-forw-overlay 
					    'face paren-face)))
		     (setq mic-paren-forw-overlay
			   (mic-make-overlay (1- close) close))
		     (if mismatch
			 (mic-overlay-put mic-paren-forw-overlay 
					  'face paren-mismatch-face)
		       (mic-overlay-put mic-paren-forw-overlay 
					'face paren-face))))

	       ;; Print messages if match is offscreen
	       (and paren-message-offscreen
		    (not visible)
		    (not (window-minibuffer-p (selected-window)))
		    (message "%s %s" 
			     (if mismatch "MISMATCH:" "Matches")
			     (mic-paren-get-matching-close-text close)))
	       ;; Ding if mismatch
	       (and mismatch
		    paren-ding-unmatched
		    (ding)))
	   (and paren-message-no-match
		(not (window-minibuffer-p (selected-window)))
		(message "No closing parenthesis found"))
	   (and paren-message-no-match
		paren-ding-unmatched
		(ding))))))

;;; --------------------------------------------------

(defun mic-paren-get-matching-open-text (open)
  "Returns a string with the context around OPEN-paren."
  ;; If there's stuff on this line preceding the paren, then display text from
  ;; beginning of line to paren.
  ;;
  ;; If, however, the paren is at the beginning of a line, then skip whitespace
  ;; forward and display text from paren to end of the next line containing
  ;; non-space text.
  ;;
  ;; (Same as in stig-paren.el)
  (save-excursion
    (goto-char open)
    (if (save-excursion
	  (skip-chars-backward " \t")
	  (not (bolp)))
	(progn
	  (beginning-of-line)
	  (concat (buffer-substring (point) (1+ open)) "..."))
      (forward-char 1)			;From the beginning-of-line
      (skip-chars-forward "\n \t")
      (end-of-line)
      (buffer-substring open (point)))))


(defun mic-paren-get-matching-close-text (close)
  "Returns a string with the context around CLOSE-paren."
  ;; The whole line up until the close-paren with "..." appended if there are
  ;; more text after the close-paren
  (save-excursion
    (goto-char close)
    (beginning-of-line)
    (concat
     (buffer-substring (point) close)
     (progn 
       (goto-char close)
       (if (looking-at "[ \t]*$")
	   ""
	 "...")))))
  

(defun paren-evenp (number)
  "Returns t if NUMBER is an even number, nil otherwise"
  (eq 0 (% number 2)))

(defun paren-backslashes-before-char (pnt)
  (setq pnt (1- pnt))
  (let ((n 0))
    (while (and (>= pnt (point-min))
		(eq (char-syntax (char-after pnt)) ?\\))
      (setq n (1+ n))
      (setq pnt (1- pnt)))
    n))

    

;;; ======================================================================
;;; Initialisation when loading:


(or paren-dont-activate-on-load
    (paren-activate))

;;; This is in case mic-paren.el is preloaded. [Does this work? /Mic]
(add-hook 'window-setup-hook
	  (function (lambda ()
		      (and window-system
			   (not paren-dont-activate-on-load)
			   (paren-activate)))))

(provide 'mic-paren)
(provide 'paren)
