;;; modeline.el --- modeline hackery.

;; Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     General mouse modeline stuff                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup modeline nil
  "Modeline customizations"
  :group 'environment)

(defcustom drag-modeline-event-lag 150
  "*The pause (in msecs) between drag modeline events before redisplaying.
If this value is too small, dragging will be choppy because redisplay cannot
keep up. If it is too large, dragging will be choppy because of the explicit
redisplay delay specified."
  :type 'integer
  :group 'modeline)

(defcustom modeline-click-swaps-buffers nil
  "*If non-nil, clicking on the modeline changes the current buffer.
Click on the left half of the modeline cycles forward through the
buffer list and clicking on the right half cycles backward."
  :type 'boolean
  :group 'modeline)

(defun mouse-drag-modeline (event)
  "Resize the window by dragging the modeline.
This should be bound to a mouse button in `modeline-map'."
  (interactive "e")
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  (or (event-over-modeline-p event)
      (error "not over a modeline"))
  (let ((depress-line (event-y event))
	(mouse-down t)
	(window (event-window event))
	(old-window (selected-window))
	(def-line-height (face-height 'default))
	(prior-drag-modeline-event-time 0)
	delta)
    (while mouse-down
      (setq event (next-event event))
      (cond ((motion-event-p event)
	     (if (window-lowest-p window)
		 (error "can't drag bottommost modeline"))
	     (cond ((> (- (event-timestamp event)
			  prior-drag-modeline-event-time)
		       drag-modeline-event-lag)

	       (setq prior-drag-modeline-event-time (event-timestamp event))

	       (if (event-over-modeline-p event)
		   (setq delta 0)
		 (setq delta (- (event-y-pixel event)
				(nth 3 (window-pixel-edges window))))
		 (if (> delta 0)
		     (setq delta (+ delta def-line-height)))
		 (setq delta (/ delta def-line-height)))

	       ;; cough sputter hack kludge.  It shouldn't be possible
	       ;; to get in here when we are over the minibuffer.  But
	       ;; it is happening and that cause next-vertical-window to
	       ;; return nil which does not lead to window-height returning
	       ;; anything remotely resembling a sensible value.  So catch
	       ;; the situation and die a happy death.
	       ;;
	       ;; Oh, and the BLAT FOOP error messages suck as well but
	       ;; I don't know what should be there.  This should be
	       ;; looked at again when the new redisplay is done.
	       (if (not (next-vertical-window window))
		   (error "Try again: dragging in minibuffer does nothing"))
	       (cond ((and (> delta 0)
			   (<= (- (window-height (next-vertical-window window))
				  delta)
			       window-min-height))
		      (setq delta (- (window-height
				      (next-vertical-window window))
				     window-min-height))
		      (if (< delta 0) (error "BLAT")))
		     ((and (< delta 0)
			   (< (+ (window-height window) delta)
			      window-min-height))
		      (setq delta (- window-min-height
				     (window-height window)))
		      (if (> delta 0) (error "FOOP"))))
	       (if (= delta 0)
		   nil
		 (select-window window)
		 (enlarge-window delta)
		 ;; The call to enlarge-window may have caused the old
		 ;; window to disappear.  Don't try and select it in
		 ;; that case.
		 (if (window-live-p old-window)
		     (select-window old-window))
		 (sit-for 0)
		 ))))
	    ((button-release-event-p event)
	     (setq mouse-down nil)
	     (if modeline-click-swaps-buffers
		 (mouse-release-modeline event depress-line)))
	    ((or (button-press-event-p event)
		 (key-press-event-p event))
	     (error ""))
	    (t
	     (dispatch-event event)))
      )))

;; from Bob Weiner (bob_weiner@pts.mot.com)
(defun mouse-release-modeline (event line-num)
  "Handle modeline click EVENT on LINE-NUM by switching buffers.
If click on left half of a frame's modeline, bury current buffer.
If click on right half of a frame's modeline, raise bottommost buffer.
Args are: EVENT, the mouse release event, and LINE-NUM, the line number
within the frame at which the mouse was first depressed."
  (if (= line-num (event-y event))
      ;; Button press and release are at same line, treat this as
      ;; a click and switch buffers.
	(if (< (event-x event) (/ (window-width (event-window event)) 2))
	    ;; On left half of modeline, bury current buffer,
	    ;; displaying second buffer on list.
	    (mouse-bury-buffer event)
	  ;; On right half of modeline, raise and display bottommost
	  ;; buffer in buffer list.
	  (mouse-unbury-buffer event))))

(defconst modeline-menu
  '("Window Commands"
    ["Delete Window Above"	 delete-window			t]
    ["Delete Other Windows"	 delete-other-windows		t]
    ["Split Window Above"	 split-window-vertically	t]
    ["Split Window Horizontally" split-window-horizontally	t]
    ["Balance Windows"		 balance-windows		t]
    ))

(defun modeline-menu (event)
  (interactive "e")
  (popup-menu-and-execute-in-window
   (cons (format "Window Commands for %S:"
		 (buffer-name (event-buffer event)))
	 (cdr modeline-menu))
   event))

(defvar modeline-map (make-sparse-keymap 'modeline-map)
  "Keymap consulted for mouse-clicks on the modeline of a window.
This variable may be buffer-local; its value will be looked up in
the buffer of the window whose modeline was clicked upon.")

(define-key modeline-map 'button1 'mouse-drag-modeline)
;; button2 selects the window without setting point
(define-key modeline-map 'button2 (lambda () (interactive "@")))
(define-key modeline-map 'button3 'modeline-menu)

(make-face 'modeline-mousable "Face for mousable portions of the modeline.")

(defmacro make-modeline-command-wrapper (command)
  `#'(lambda (event)
       (interactive "e")
       (save-selected-window
	 (select-window (event-window event))
	 (call-interactively ',(eval command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Minor modes                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar minor-mode-alist nil
  "Alist saying how to show minor modes in the modeline.
Each element looks like (VARIABLE STRING);
STRING is included in the modeline iff VARIABLE's value is non-nil.

Actually, STRING need not be a string; any possible modeline element
is okay.  See `modeline-format'.")

;; Used by C code (lookup-key and friends) but defined here.
(defvar minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.")

(make-face 'modeline-mousable-minor-mode
	   "Face for mousable minor-mode strings in the modeline.")

(defvar modeline-mousable-minor-mode-extent (make-extent nil nil)
  ;; alliteration at its finest.
  "Extent managing the mousable minor mode modeline strings.")
(set-extent-face modeline-mousable-minor-mode-extent
		 'modeline-mousable-minor-mode)

;; This replaces the idiom
;;
;; (or (assq 'isearch-mode minor-mode-alist)
;;     (setq minor-mode-alist
;;           (purecopy
;;            (append minor-mode-alist
;;                    '((isearch-mode isearch-mode))))))

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.
TOGGLE is a symbol whose value as a variable specifies whether the
minor mode is active.  NAME is the name that should appear in the
modeline (it should be a string beginning with a space).  KEYMAP is a
keymap to make active when the minor mode is active.  AFTER is the
toggling symbol used for another minor mode.  If AFTER is non-nil,
then it is used to position the new mode in the minor-mode alists.
TOGGLE-FUN specifies an interactive function that is called to toggle
the mode on and off; this affects what happens when button2 is pressed
on the mode, and when button3 is pressed somewhere in the list of
modes.  If TOGGLE-FUN is nil and TOGGLE names an interactive function,
TOGGLE is used as the toggle function.

Example:  (add-minor-mode 'view-minor-mode \" View\" view-mode-map)"
  (let (el place
	(add-elt #'(lambda (elt sym)
		     (cond ((null after) ; add to front
			    (set sym (cons elt (symbol-value sym))))
			   ((and (not (eq after t))
				 (setq place (memq (assq after
							 (symbol-value sym))
						   (symbol-value sym))))
			    (setq elt (cons elt (cdr place)))
			    (setcdr place elt))
			   (t
			    (set sym (append (symbol-value sym) (list elt))))
			   )
		     (symbol-value sym)))
	toggle-keymap)
    (if toggle-fun
	(if (not (commandp toggle-fun))
	    (error "not an interactive function: %S" toggle-fun))
      (if (commandp toggle)
	  (setq toggle-fun toggle)))
    (if (and toggle-fun name)
	(progn
	  (setq toggle-keymap (make-sparse-keymap
			       (intern (concat "modeline-minor-"
					       (symbol-name toggle)
					       "-map"))))
	  (define-key toggle-keymap 'button2
	    ;; defeat the DUMB-ASS byte-compiler, which tries to
	    ;; expand the macro at compile time and fucks up.
	    (eval '(make-modeline-command-wrapper toggle-fun)))
	  (put toggle 'modeline-toggle-function toggle-fun)))
    (and name
	 (let ((hacked-name
		(if toggle-keymap
		    (cons (let ((extent (make-extent nil nil)))
			    (set-extent-keymap extent toggle-keymap)
			    (set-extent-property
			     extent 'help-echo
			     (concat "button2 turns off "
				     (if (symbolp toggle-fun)
					 (symbol-name toggle-fun)
				       (symbol-name toggle))))
			    extent)
			  (cons
			   modeline-mousable-minor-mode-extent
			   name))
		  name)))
	   (if (setq el (assq toggle minor-mode-alist))
	       (setcdr el (list hacked-name))
	     (funcall add-elt 
		      (list toggle hacked-name)
		      'minor-mode-alist))))
    (and keymap
	 (if (setq el (assq toggle minor-mode-map-alist))
	     (setcdr el keymap)
	   (funcall add-elt
		    (cons toggle keymap)
		    'minor-mode-map-alist)))
    ))

(add-minor-mode 'abbrev-mode " Abbrev")
(add-minor-mode 'overwrite-mode 'overwrite-mode)
(add-minor-mode 'auto-fill-function " Fill" nil nil 'auto-fill-mode)
;; not really a minor mode...
(add-minor-mode 'defining-kbd-macro " Def")

(defun modeline-minor-mode-menu (event)
  (interactive "e")
  (save-excursion
    (set-buffer (event-buffer event))
    (popup-menu-and-execute-in-window
     (cons (format "Minor Mode Commands for %S:"
		   (buffer-name (event-buffer event)))
	   (apply 'nconc
		  (mapcar
		   #'(lambda (x)
		       (let* ((toggle-sym (car x))
			      (toggle-fun
			       (or (get toggle-sym
					'modeline-toggle-function)
				   (and (fboundp toggle-sym)
					(commandp toggle-sym)
					toggle-sym))))
			 (if (not toggle-fun) nil
			   (list (vector
				  (concat (if (and (boundp toggle-sym)
						   (symbol-value toggle-sym))
					      "turn off " "turn on ")
					  (if (symbolp toggle-fun)
					      (symbol-name toggle-fun)
					    (symbol-name toggle-sym)))

				  toggle-fun
				  t)))))
		   minor-mode-alist)))
     event)))

(defvar modeline-minor-mode-map (make-sparse-keymap 'modeline-minor-mode-map)
  "Keymap consulted for mouse-clicks on the minor-mode modeline list.")
(define-key modeline-minor-mode-map 'button3 'modeline-minor-mode-menu)

(defvar modeline-minor-mode-extent (make-extent nil nil)
  "Extent covering the minor mode modeline strings.")
(set-extent-face modeline-minor-mode-extent 'modeline-mousable)
(set-extent-keymap modeline-minor-mode-extent modeline-minor-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              Other                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modeline-buffers-menu (event)
  (interactive "e")
  (popup-menu-and-execute-in-window
   '("Buffers Popup Menu"
     :filter buffers-menu-filter
     ["List All Buffers" list-buffers t]
     "--"
     )
   event))

(defvar modeline-buffer-id-left-map
  (make-sparse-keymap 'modeline-buffer-id-left-map)
"Keymap consulted for mouse-clicks on the left half of the buffer-id string.")

(defvar modeline-buffer-id-right-map
  (make-sparse-keymap 'modeline-buffer-id-right-map)
"Keymap consulted for mouse-clicks on the right half of the buffer-id string.")

(define-key modeline-buffer-id-left-map 'button2 'mouse-unbury-buffer)
(define-key modeline-buffer-id-right-map 'button2 'mouse-bury-buffer)
(define-key modeline-buffer-id-left-map 'button3 'modeline-buffers-menu)
(define-key modeline-buffer-id-right-map 'button3 'modeline-buffers-menu)

(make-face 'modeline-buffer-id
	   "Face for the buffer ID string in the modeline.")

(defvar modeline-buffer-id-extent (make-extent nil nil)
  "Extent covering the whole of the buffer-id string.")
(set-extent-face modeline-buffer-id-extent 'modeline-buffer-id)
  
(defvar modeline-buffer-id-left-extent (make-extent nil nil)
"Extent covering the left half of the buffer-id string.")
(set-extent-keymap modeline-buffer-id-left-extent
		   modeline-buffer-id-left-map)
(set-extent-property modeline-buffer-id-left-extent 'help-echo
		     "button2 cycles to the previous buffer")

(defvar modeline-buffer-id-right-extent (make-extent nil nil)
"Extent covering the right half of the buffer-id string.")
(set-extent-keymap modeline-buffer-id-right-extent
		   modeline-buffer-id-right-map)
(set-extent-property modeline-buffer-id-right-extent 'help-echo
		     "button2 cycles to the next buffer")

(defconst modeline-buffer-identification
  (list (cons modeline-buffer-id-left-extent (purecopy "XEmacs:"))
	(cons modeline-buffer-id-right-extent (purecopy " %17b")))
  "Modeline control for identifying the buffer being displayed.
Its default value is \"XEmacs: %17b\" (NOT!).  Major modes that edit things
other than ordinary files may change this (e.g. Info, Dired,...)")
(make-variable-buffer-local 'modeline-buffer-identification)

(defconst modeline-process nil
  "Modeline control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")
(make-variable-buffer-local 'modeline-process)

(defvar modeline-modified-map (make-sparse-keymap 'modeline-modified-map)
  "Keymap consulted for mouse-clicks on the modeline-modified string.")
(define-key modeline-modified-map 'button2
  (make-modeline-command-wrapper 'vc-toggle-read-only))

(defvar modeline-modified-extent (make-extent nil nil)
  "Extent covering the modeline-modified string.")
(set-extent-face modeline-modified-extent 'modeline-mousable)
(set-extent-keymap modeline-modified-extent modeline-modified-map)
(set-extent-property modeline-modified-extent 'help-echo
		     "button2 toggles the buffer's read-only status")

(defconst modeline-modified (purecopy '("--%1*%1+-"))
  "Modeline control for displaying whether current buffer is modified.")
(make-variable-buffer-local 'modeline-modified)

(defvar modeline-narrowed-map (make-sparse-keymap 'modeline-narrowed-map)
  "Keymap consulted for mouse-clicks on the modeline-narrowed string.")
(define-key modeline-narrowed-map 'button2
  (make-modeline-command-wrapper 'widen))

(defvar modeline-narrowed-extent (make-extent nil nil)
  "Extent covering the modeline-narrowed string.")
(set-extent-face modeline-narrowed-extent 'modeline-mousable)
(set-extent-keymap modeline-narrowed-extent modeline-narrowed-map)
(set-extent-property modeline-narrowed-extent 'help-echo
		     "button2 widens the buffer")

(setq-default
 modeline-format
 (list
  (purecopy "")
  (cons modeline-modified-extent 'modeline-modified)
  (cons modeline-buffer-id-extent 'modeline-buffer-identification)
  (purecopy "   ")
  'global-mode-string
  (purecopy "   %[(")
  (cons modeline-minor-mode-extent (list "" 'mode-name 'minor-mode-alist))
  (cons modeline-narrowed-extent "%n")
  'modeline-process
  (purecopy ")%]----")
  (purecopy '(line-number-mode "L%l--"))
  (purecopy '(column-number-mode "C%c--"))
  (purecopy '(-3 . "%p"))
  (purecopy "-%-")))
