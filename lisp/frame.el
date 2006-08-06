;;; frame.el --- multi-frame management independent of window systems.

;; Copyright (C) 1993, 1994, 1996, 1997, 2000, 2001, 2003
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

;; XEmacs addition
(defgroup frames nil
  "Support for Emacs frames and window systems."
  :group 'environment)

;; XEmacs change: No need for `frame-creation-function'.

;; XEmacs change: Emacs no longer specifies the minibuffer property here.
;;; The initial value given here for this must ask for a minibuffer.
;;; There must always exist a frame with a minibuffer, and after we
;;; delete the terminal frame, this will be the only frame.
(defcustom initial-frame-plist '(minibuffer t)
  "Plist of frame properties for creating the initial X window frame.
You can set this in your `.emacs' file; for example,
  (setq initial-frame-plist '(top 1 left 1 width 80 height 55))
Properties specified here supersede the values given in `default-frame-plist'.
The format of this can also be an alist for backward compatibility.

If the value calls for a frame without a minibuffer, and you have not created
a minibuffer frame on your own, one is created according to
`minibuffer-frame-plist'.

You can specify geometry-related options for just the initial frame
by setting this variable in your `.emacs' file; however, they won't
take effect until Emacs reads `.emacs', which happens after first creating
the frame.  If you want the frame to have the proper geometry as soon
as it appears, you need to use this three-step process:
* Specify X resources to give the geometry you want.
* Set `default-frame-plist' to override these options so that they
  don't affect subsequent frames.
* Set `initial-frame-plist' in a way that matches the X resources,
  to override what you put in `default-frame-plist'."
  :type 'plist
  :group 'frames)

(defcustom minibuffer-frame-plist '(width 80 height 2 menubar-visible-p nil
				    default-toolbar-visible-p nil)
  "Plist of frame properties for initially creating a minibuffer frame.
You can set this in your `.emacs' file; for example,
  (setq minibuffer-frame-plist '(top 1 left 1 width 80 height 2))
Properties specified here supersede the values given in
`default-frame-plist'.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

(defcustom pop-up-frame-plist nil
  "Plist of frame properties used when creating pop-up frames.
Pop-up frames are used for completions, help, and the like.
This variable can be set in your init file, like this:
  (setq pop-up-frame-plist '(width 80 height 20))
These supersede the values given in `default-frame-plist', for pop-up frames.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

(setq pop-up-frame-function
      #'(lambda ()
	  (make-frame pop-up-frame-plist)))

(defcustom special-display-frame-plist '(height 14 width 80 unsplittable t)
  "*Plist of frame properties used when creating special frames.
Special frames are used for buffers whose names are in
`special-display-buffer-names' and for buffers whose names match
one of the regular expressions in `special-display-regexps'.
This variable can be set in your init file, like this:
  (setq special-display-frame-plist '(width 80 height 20))
These supersede the values given in `default-frame-plist'.
The format of this can also be an alist for backward compatibility."
  :type 'plist
  :group 'frames)

;; XEmacs addition
(defun safe-alist-to-plist (cruftiness)
  (if (consp (car cruftiness))
      (alist-to-plist cruftiness)
    cruftiness))

;; XEmacs change: require args to be a plist instead of an alist.
(defun special-display-popup-frame (buffer &optional args)
  "Display BUFFER in its own frame, reusing an existing window if any.
Return the window chosen.
Currently we do not insist on selecting the window within its frame.
If ARGS is a plist, use it as a list of frame property specs.
If ARGS is a list whose car is t,
use (cadr ARGS) as a function to do the work.
Pass it BUFFER as first arg, and (cddr ARGS) gives the rest of the args."
  ;; if we can't display simultaneous multiple frames, just return
  ;; nil and let the normal behavior take over.
  (and (device-on-window-system-p)
       (if (and args (eq t (car args)))
	   (apply (cadr args) buffer (cddr args))
	 (let ((window (get-buffer-window buffer t)))
	   (setq args (safe-alist-to-plist args))
	   (or
	    ;; If we have a window already, make it visible.
	    (when window
	      (let ((frame (window-frame window)))
		(make-frame-visible frame)
		(raise-frame frame)
		window))
	    ;; Reuse the current window if the user requested it.
	    (when (lax-plist-get args 'same-window)
	      (condition-case nil
		  (progn (switch-to-buffer buffer) (selected-window))
		(error nil)))
	    ;; Stay on the same frame if requested.
	    (when (or (lax-plist-get args 'same-frame)
		      (lax-plist-get args 'same-window))
	      (let* ((pop-up-frames nil) (pop-up-windows t)
		     special-display-regexps special-display-buffer-names
		     (window (display-buffer buffer)))
		;; (set-window-dedicated-p window t)
		window))
	    ;; If no window yet, make one in a new frame.
	    (let ((frame (make-frame (append args
					     (safe-alist-to-plist
					      special-display-frame-plist)))))
	      (set-window-buffer (frame-selected-window frame) buffer)
	      (set-window-dedicated-p (frame-selected-window frame) t)
	      (frame-selected-window frame)))))))

;; XEmacs change: comment out
;(defun handle-delete-frame (event)
;  "Handle delete-frame events from the X server."
;  (interactive "e")
;  (let ((frame (posn-window (event-start event)))
;	(i 0)
;	(tail (frame-list)))
;    (while tail
;      (and (frame-visible-p (car tail))
;	   (not (eq (car tail) frame))
;	  (setq i (1+ i)))
;      (setq tail (cdr tail)))
;    (if (> i 0)
;	(delete-frame frame t)
;      ;; Gildea@x.org says it is ok to ask questions before terminating.
;      (save-buffers-kill-emacs))))

;;;; Arrangement of frames at startup

;; 1) Load the window system startup file from the lisp library and read the
;; high-priority arguments (-q and the like).  The window system startup
;; file should create any frames specified in the window system defaults.
;;
;; 2) If no frames have been opened, we open an initial text frame.
;;
;; 3) Once the init file is done, we apply any newly set properties
;; in initial-frame-plist to the frame.

;; These are now called explicitly at the proper times,
;; since that is easier to understand.
;; Actually using hooks within Emacs is bad for future maintenance. --rms.
;; (add-hook 'before-init-hook 'frame-initialize)
;; (add-hook 'window-setup-hook 'frame-notice-user-settings)

;; If we create the initial frame, this is it.
(defvar frame-initial-frame nil)

;; Record the properties used in frame-initialize to make the initial frame.
(defvar frame-initial-frame-plist)

(defvar frame-initial-geometry-arguments nil)

;; XEmacs addition
(defun canonicalize-frame-plists ()
  (setq initial-frame-plist (safe-alist-to-plist initial-frame-plist))
  (setq default-frame-plist (safe-alist-to-plist default-frame-plist)))

;; startup.el calls this function before loading the user's init
;; file - if there is no frame with a minibuffer open now, create
;; one to display messages while loading the init file.
(defun frame-initialize ()
  "Create an initial frame if necessary."
  ;; In batch mode, we actually use the initial terminal device for output.
  ;; XEmacs addition
  (canonicalize-frame-plists)

  (if (not (noninteractive))
      (progn
	;; Turn on special-display processing only if there's a window system.
	(setq special-display-function 'special-display-popup-frame)

	;; If there is no frame with a minibuffer besides the terminal
	;; frame, then we need to create the opening frame.  Make sure
	;; it has a minibuffer, but let initial-frame-plist omit the
	;; minibuffer spec.
	(or (delq terminal-frame (minibuffer-frame-list))
	    (progn
	      (setq frame-initial-frame-plist
		    (append initial-frame-plist default-frame-plist))
	      ;; XEmacs change: omit the scrollbar settings
; 	      (or (assq 'horizontal-scroll-bars frame-initial-frame-alist)
; 		  (setq frame-initial-frame-alist
; 			(cons '(horizontal-scroll-bars . t)
; 			      frame-initial-frame-alist)))
	      (setq default-minibuffer-frame
		    (setq frame-initial-frame
			  (make-frame initial-frame-plist
				      (car (delq terminal-device
						 (device-list))))))
	      ;; Delete any specifications for window geometry properties
	      ;; so that we won't reapply them in frame-notice-user-settings.
	      ;; It would be wrong to reapply them then,
	      ;; because that would override explicit user resizing.
	      (setq initial-frame-plist
		    (frame-remove-geometry-props initial-frame-plist))))
	;; At this point, we know that we have a frame open, so we
	;; can delete the terminal frame.
	;; XEmacs change: Do it the same way Fkill_emacs does it. -slb
	(delete-console terminal-console)
	(setq terminal-frame nil))

    ;; XEmacs change: omit the pc window-system stuff.
;    ;; No, we're not running a window system.  Use make-terminal-frame if
;    ;; we support that feature, otherwise arrange to cause errors.
;     (or (eq window-system 'pc)
; 	(setq frame-creation-function
; 	      (if (fboundp 'tty-create-frame-with-faces)
; 		  'tty-create-frame-with-faces
; 		(function
; 		 (lambda (parameters)
; 		   (error
; 		    "Can't create multiple frames without a window system"))))))
    ))

(defvar frame-notice-user-settings t
  "Non-nil means function `frame-notice-user-settings' wasn't run yet.")

;; startup.el calls this function after loading the user's init
;; file.  Now default-frame-plist and initial-frame-plist contain
;; information to which we must react; do what needs to be done.
(defun frame-notice-user-settings ()
  "Act on user's init file settings of frame parameters.
React to settings of `default-frame-plist', `initial-frame-plist' there."
  ;; XEmacs addition
  (canonicalize-frame-plists)

  ;; XEmacs change: omit menu-bar manipulations.
;   ;; Make menu-bar-mode and default-frame-alist consistent.
;   (when (boundp 'menu-bar-mode)
;     (let ((default (assq 'menu-bar-lines default-frame-alist)))
;       (if default
; 	  (setq menu-bar-mode (not (eq (cdr default) 0)))
; 	(setq default-frame-alist
; 	      (cons (cons 'menu-bar-lines (if menu-bar-mode 1 0))
; 		    default-frame-alist)))))

  ;; XEmacs change: omit tool-bar manipulations.
;   ;; Make tool-bar-mode and default-frame-alist consistent.  Don't do
;   ;; it in batch mode since that would leave a tool-bar-lines
;   ;; parameter in default-frame-alist in a dumped Emacs, which is not
;   ;; what we want.
;   (when (and (boundp 'tool-bar-mode)
; 	     (not noninteractive))
;     (let ((default (assq 'tool-bar-lines default-frame-alist)))
;       (if default
; 	  (setq tool-bar-mode (not (eq (cdr default) 0)))
; 	(setq default-frame-alist
; 	      (cons (cons 'tool-bar-lines (if tool-bar-mode 1 0))
; 		    default-frame-alist)))))

  ;; Creating and deleting frames may shift the selected frame around,
  ;; and thus the current buffer.  Protect against that.  We don't
  ;; want to use save-excursion here, because that may also try to set
  ;; the buffer of the selected window, which fails when the selected
  ;; window is the minibuffer.
  (let ((old-buffer (current-buffer)))

    ;; XEmacs change: omit special handling for MS-DOS
;     (when (and frame-notice-user-settings
; 	       (null frame-initial-frame))
;       ;; This case happens when we don't have a window system, and
;       ;; also for MS-DOS frames.
;       (let ((parms (frame-parameters frame-initial-frame)))
; 	;; Don't change the frame names.
; 	(setq parms (delq (assq 'name parms) parms))
; 	;; Can't modify the minibuffer parameter, so don't try.
; 	(setq parms (delq (assq 'minibuffer parms) parms))
; 	(modify-frame-parameters nil
; 				 (if (null window-system)
; 				     (append initial-frame-alist
; 					     default-frame-alist
; 					     parms
; 					     nil)
; 				   ;; initial-frame-alist and
; 				   ;; default-frame-alist were already
; 				   ;; applied in pc-win.el.
; 				   parms))
; 	(if (null window-system) ;; MS-DOS does this differently in pc-win.el
; 	    (let ((newparms (frame-parameters))
; 		  (frame (selected-frame)))
; 	      (tty-handle-reverse-video frame newparms)
; 	      ;; If we changed the background color, we need to update
; 	      ;; the background-mode parameter, and maybe some faces,
; 	      ;; too.
; 	      (when (assq 'background-color newparms)
; 		(unless (or (assq 'background-mode initial-frame-alist)
; 			    (assq 'background-mode default-frame-alist))
; 		  (frame-set-background-mode frame))
; 		(face-set-after-frame-default frame))))))

    ;; If the initial frame is still around, apply initial-frame-plist
    ;; and default-frame-plist to it.
    (when (frame-live-p frame-initial-frame)

      ;; XEmacs change: omit the tool-bar manipulations
;       ;; When tool-bar has been switched off, correct the frame size
;       ;; by the lines added in x-create-frame for the tool-bar and
;       ;; switch `tool-bar-mode' off.
;       (when (display-graphic-p)
; 	(let ((tool-bar-lines (or (assq 'tool-bar-lines initial-frame-alist)
; 				  (assq 'tool-bar-lines default-frame-alist))))
; 	  (when (and tool-bar-originally-present
;                      (or (null tool-bar-lines)
;                          (null (cdr tool-bar-lines))
;                          (eq 0 (cdr tool-bar-lines))))
; 	    (let* ((char-height (frame-char-height frame-initial-frame))
; 		   (image-height tool-bar-images-pixel-height)
; 		   (margin (cond ((and (consp tool-bar-button-margin)
; 				       (integerp (cdr tool-bar-button-margin))
; 				       (> tool-bar-button-margin 0))
; 				  (cdr tool-bar-button-margin))
; 				 ((and (integerp tool-bar-button-margin)
; 				       (> tool-bar-button-margin 0))
; 				  tool-bar-button-margin)
; 				 (t 0)))
; 		   (relief (if (and (integerp tool-bar-button-relief)
; 				    (> tool-bar-button-relief 0))
; 			       tool-bar-button-relief 3))
; 		   (lines (/ (+ image-height
; 				(* 2 margin)
; 				(* 2 relief)
; 				(1- char-height))
; 			     char-height))
; 		   (height (frame-parameter frame-initial-frame 'height))
; 		   (newparms (list (cons 'height (- height lines))))
; 		   (initial-top (cdr (assq 'top
; 					   frame-initial-geometry-arguments)))
; 		   (top (frame-parameter frame-initial-frame 'top)))
; 	      (when (and (consp initial-top) (eq '- (car initial-top)))
; 		(let ((adjusted-top
; 		       (cond ((and (consp top)
; 				   (eq '+ (car top)))
; 			      (list '+
; 				    (+ (cadr top)
; 				       (* lines char-height))))
; 			     ((and (consp top)
; 				   (eq '- (car top)))
; 			      (list '-
; 				    (- (cadr top)
; 				       (* lines char-height))))
; 			     (t (+ top (* lines char-height))))))
; 		  (setq newparms
; 			(append newparms
; 				`((top . ,adjusted-top))
; 				nil))))
; 	      (modify-frame-parameters frame-initial-frame newparms)
; 	      (tool-bar-mode -1)))))

	;; The initial frame we create above always has a minibuffer.
	;; If the user wants to remove it, or make it a minibuffer-only
	;; frame, then we'll have to delete the selected frame and make a
	;; new one; you can't remove or add a root window to/from an
	;; existing frame.
	;;
	;; NOTE: default-frame-plist was nil when we created the
	;; existing frame.  We need to explicitly include
	;; default-frame-plist in the properties of the screen we
	;; create here, so that its new value, gleaned from the user's
	;; .emacs file, will be applied to the existing screen.
	(if (not (eq (car
		      (or (and (lax-plist-member
				initial-frame-plist 'minibuffer)
			       (list (lax-plist-get initial-frame-plist
						    'minibuffer)))
			  (and (lax-plist-member default-frame-plist
						 'minibuffer)
			       (list (lax-plist-get default-frame-plist
						    'minibuffer)))
			 '(t)))
		     t))
	    ;; Create the new frame.
	    (let (props ;new
		  )
	      ;; If the frame isn't visible yet, wait till it is.
	      ;; If the user has to position the window,
	      ;; Emacs doesn't know its real position until
	      ;; the frame is seen to be visible.

	      ;; XEmacs change: check the initially-unmapped property
	      (if (frame-property frame-initial-frame 'initially-unmapped)
		  nil
		(while (not (frame-visible-p frame-initial-frame))
		  (sleep-for 1)))
	      (setq props (frame-properties frame-initial-frame))

	      ;; Get rid of `name' unless it was specified explicitly before.
	      (or (lax-plist-member frame-initial-frame-plist 'name)
		  (setq props (lax-plist-remprop props 'name)))

	      (setq props (append initial-frame-plist
				  default-frame-plist
				  props
				  nil))

	      ;; Get rid of `reverse', because that was handled
	      ;; when we first made the frame.
	      (laxputf props 'reverse nil)

	      ;; XEmacs addition: Get rid of `window-id', otherwise make-frame
	      ;; will think we're trying to setup an external widget.
	      (laxremf props 'window-id)

	      (if (lax-plist-member frame-initial-geometry-arguments 'height)
		  (laxremf props 'height))
	      (if (lax-plist-member frame-initial-geometry-arguments 'width)
		  (laxremf props 'width))
	      (if (lax-plist-member frame-initial-geometry-arguments 'left)
		  (laxremf props 'left))
	      (if (lax-plist-member frame-initial-geometry-arguments 'top)
		  (laxremf props 'top))
	      ;; Now create the replacement initial frame.
	      ;(setq new
	      (make-frame
	       ;; Use the geometry args that created the existing
	       ;; frame, rather than the props we get for it.
	       (append '(user-size t user-position t)
		       frame-initial-geometry-arguments
		       props))
	      ;)
	      ;; The initial frame, which we are about to delete, may be
	      ;; the only frame with a minibuffer.  If it is, create a
	      ;; new one.
	      (or (delq frame-initial-frame (minibuffer-frame-list))
		  (make-initial-minibuffer-frame nil))

	      ;; If the initial frame is serving as a surrogate
	      ;; minibuffer frame for any frames, we need to wean them
	      ;; onto a new frame.  The default-minibuffer-frame
	      ;; variable must be handled similarly.
	      (let ((users-of-initial
		     (filtered-frame-list
		      #'(lambda (frame)
				  (and (not (eq frame frame-initial-frame))
				       (eq (window-frame
					    (minibuffer-window frame))
					   frame-initial-frame))))))
		(if (or users-of-initial
			(eq default-minibuffer-frame frame-initial-frame))

		    ;; Choose an appropriate frame.  Prefer frames which
		    ;; are only minibuffers.
		    (let* ((new-surrogate
			    (car
			     (or (filtered-frame-list
				  #'(lambda (frame)
				      (eq 'only
					  (frame-property frame 'minibuffer))))
				 (minibuffer-frame-list))))
			   (new-minibuffer (minibuffer-window new-surrogate)))

		      (if (eq default-minibuffer-frame frame-initial-frame)
			  (setq default-minibuffer-frame new-surrogate))

		      ;; Wean the frames using frame-initial-frame as
		      ;; their minibuffer frame.
		      (mapcar
		       #'(lambda (frame)
			   (set-frame-property frame 'minibuffer
					       new-minibuffer))
		       users-of-initial))))

	      ;; Redirect events enqueued at this frame to the new frame.
	      ;; Is this a good idea?
	      ;; Probably not, since this whole redirect-frame-focus
	      ;; stuff is a load of trash, and so is this function we're in.
	      ;; --ben
	      ;(redirect-frame-focus frame-initial-frame new)

	      ;; Finally, get rid of the old frame.
	      (delete-frame frame-initial-frame t))

	  ;; Otherwise, we don't need all that rigamarole; just apply
	  ;; the new properties.
	  (let (newprops allprops tail)
	    (setq allprops (append initial-frame-plist
				   default-frame-plist))
	    (if (lax-plist-member frame-initial-geometry-arguments 'height)
		(laxremf allprops 'height))
	    (if (lax-plist-member frame-initial-geometry-arguments 'width)
		(remf allprops 'width))
	    (if (lax-plist-member frame-initial-geometry-arguments 'left)
		(laxremf allprops 'left))
	    (if (lax-plist-member frame-initial-geometry-arguments 'top)
		(laxremf allprops 'top))
	    (setq tail allprops)
	    ;; Find just the props that have changed since we first
	    ;; made this frame.  Those are the ones actually set by
	    ;; the init file.  For those props whose values we already knew
	    ;; (such as those spec'd by command line options)
	    ;; it is undesirable to specify the parm again
	    ;; once the user has seen the frame and been able to alter it
	    ;; manually.
	    (while tail
	      (let (newval oldval)
		(setq oldval (lax-plist-get frame-initial-frame-plist
					    (car tail)))
		(setq newval (lax-plist-get allprops (car tail)))
		(or (eq oldval newval)
		    (laxputf newprops (car tail) newval)))
	      (setq tail (cddr tail)))
	    (set-frame-properties frame-initial-frame newprops)
	    ;; XEmacs change: omit the background manipulation
; 	    ;; If we changed the background color,
; 	    ;; we need to update the background-mode parameter
; 	    ;; and maybe some faces too.
; 	    (when (assq 'background-color newparms)
; 	      (unless (assq 'background-mode newparms)
; 		(frame-set-background-mode frame-initial-frame))
; 	      (face-set-after-frame-default frame-initial-frame)))))
	    )))

    ;; Restore the original buffer.
    (set-buffer old-buffer)

    ;; Make sure the initial frame can be GC'd if it is ever deleted.
    ;; Make sure frame-notice-user-settings does nothing if called twice.
    (setq frame-notice-user-settings nil)
    (setq frame-initial-frame nil)))

(defun make-initial-minibuffer-frame (device)
  (let ((props (append '(minibuffer only)
		       (safe-alist-to-plist minibuffer-frame-plist))))
    (make-frame props device)))


;;;; Creation of additional frames, and other frame miscellanea

(defun modify-all-frames-properties (plist)
  "Modify all current and future frames' parameters according to PLIST.
This changes `default-frame-plist' and possibly `initial-frame-plist'.
See `set-frame-properties' for more information."
  (dolist (frame (frame-list))
    (set-frame-properties frame plist))

  ;; XEmacs change: iterate over plists instead of alists
  (map-plist
   #'(lambda (prop val)
       ;; initial-frame-plist needs setting only when
       ;; frame-notice-user-settings is true
       (and frame-notice-user-settings
	    (lax-plist-remprop initial-frame-plist prop))
       (lax-plist-remprop default-frame-plist prop))
   plist)

  (and frame-notice-user-settings
       (setq initial-frame-plist (append initial-frame-plist plist)))
  (setq default-frame-plist (append default-frame-plist plist)))

(defun get-other-frame ()
  "Return some frame other than the current frame.
Create one if necessary.  Note that the minibuffer frame, if separate,
is not considered (see `next-frame')."
  (let* ((this (selected-frame))
	 ;; search visible frames first
	 (next (next-frame this 'visible-nomini)))
    ;; then search iconified frames
    (if (eq this next)
	(setq next (next-frame 'visible-iconic-nomini)))
    (if (eq this next)
	;; otherwise, make a new frame
	(make-frame)
      next)))

(defun next-multiframe-window ()
  "Select the next window, regardless of which frame it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t))
  ;; XEmacs change: select-window already selects the containing frame
  ;(select-frame-set-input-focus (selected-frame))
  )

(defun previous-multiframe-window ()
  "Select the previous window, regardless of which frame it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t))
  ;; XEmacs change: select-window already selects the containing frame
  ;(select-frame-set-input-focus (selected-frame))
  )

;; XEmacs change: Emacs has make-frame-on-display
(defun make-frame-on-device (type connection &optional props)
  "Create a frame of type TYPE on CONNECTION.
TYPE should be a symbol naming the device type, i.e. one of

x	    An X display.  CONNECTION should be a standard display string
	    such as \"unix:0\", or nil for the display specified on the
	    command line or in the DISPLAY environment variable.  Only if
	    support for X was compiled into XEmacs.
tty	    A standard TTY connection or terminal.  CONNECTION should be
	    a TTY device name such as \"/dev/ttyp2\" (as determined by
	    the Unix command `tty') or nil for XEmacs' standard input
	    and output (usually the TTY in which XEmacs started).  Only
	    if support for TTY's was compiled into XEmacs.
gtk	    A GTK device.
ns	    A connection to a machine running the NeXTstep windowing
	    system.  Not currently implemented.
mswindows   A connection to a machine running Microsoft Windows NT or
	    Windows 95/97.
pc	    A direct-write MS-DOS frame.  Not currently implemented.

PROPS should be a plist of properties, as in the call to `make-frame'.

If a connection to CONNECTION already exists, it is reused; otherwise,
a new connection is opened."
  (make-frame props (make-device type connection props)))

;; XEmacs omission: Emacs has make-frame-command here, but it reduces to
;; make-frame for us.

;; XEmacs omission: the following 2 variables are not yet implemented.
;(defvar before-make-frame-hook nil
;  "Functions to run before a frame is created.")
;
;(defvar after-make-frame-functions nil
;  "Functions to run after a frame is created.
;The functions are run with one arg, the newly created frame.")
;
(defvar after-setting-font-hook nil
  "Functions to run after a frame's font has been changed.")

;; Alias, kept temporarily.
(defalias 'new-frame 'make-frame)
(make-obsolete 'new-frame 'make-frame)

;; XEmacs change: Emacs has make-frame here.  We have it in C, so no need for
;; frame-creation-function.

;; XEmacs addition: support optional DEVICE argument.
(defun filtered-frame-list (predicate &optional device)
  "Return a list of all live frames which satisfy PREDICATE.
If optional second arg DEVICE is non-nil, restrict the frames
 returned to that device."
  (let ((frames (if device (device-frame-list device)
		  (frame-list)))
	good-frames)
    (while (consp frames)
      (if (funcall predicate (car frames))
	  (setq good-frames (cons (car frames) good-frames)))
      (setq frames (cdr frames)))
    good-frames))

;; XEmacs addition: support optional DEVICE argument.
(defun minibuffer-frame-list (&optional device)
  "Return a list of all frames with their own minibuffers.
If optional second arg DEVICE is non-nil, restrict the frames
 returned to that device."
  (filtered-frame-list
   #'(lambda (frame)
	       (eq frame (window-frame (minibuffer-window frame))))
   device))

;; XEmacs omission: Emacs has frames-on-display-list here, but that is
;; essentially equivalent to supplying the optional DEVICE argument to
;; filtered-frame-list.

;; XEmacs addition: the following two functions make life a lot simpler below.
(defsubst display-frame (display)
  "Return the active frame for DISPLAY.
DISPLAY may be a frame, a device, or a console.  If it is omitted or nil,
it defaults to the selected frame."
  (cond
   ((null display) (selected-frame))
   ((framep display) display)
   ((devicep display) (selected-frame display))
   ((consolep display) (selected-frame (car (console-device-list display))))
   (t (error 'wrong-type-argument "Not a frame, device, or console" display))))

(defsubst display-device (display)
  "Return the device for DISPLAY.
DISPLAY may be a frame, a device, or a console.  If it is omitted or nil,
it defaults to the selected frame."
  (cond
   ((null display) (selected-device))
   ((framep display) (frame-device display))
   ((devicep display) display)
   ((consolep display) (car (console-device-list display)))
   (t (error 'wrong-type-argument "Not a frame, device, or console" display))))

;; Emacs compatibility function.  We do not allow display names of the type
;; HOST:SERVER.SCREEN as Emacs does, but we do handle devices and consoles.
(defun framep-on-display (&optional display)
  "Return the type of frames on DISPLAY.
DISPLAY may be a frame, a device, or a console.  If it is a frame, its type
is returned.  If DISPLAY is omitted or nil, it defaults to the selected
frame.  All frames on a given device or console are of the same type."
  (cond
   ((null display) (frame-type (selected-frame)))
   ((framep display) (frame-type display))
   ((devicep display) (device-type display))
   ((consolep display) (console-type display))
   (t (error 'wrong-type-argument "Not a frame, device, or console" display))))

;; XEmacs addition: Emacs does not have this function.
(defun frame-minibuffer-only-p (frame)
  "Return non-nil if FRAME is a minibuffer-only frame."
  (eq (frame-root-window frame) (minibuffer-window frame)))

(defun frame-remove-geometry-props (plist)
  "Return the property list PLIST, but with geometry specs removed.
This deletes all bindings in PLIST for `top', `left', `width',
`height', `user-size' and `user-position' properties.
Emacs uses this to avoid overriding explicit moves and resizings from
the user during startup."
  (setq plist (canonicalize-lax-plist (copy-sequence plist)))
  (mapcar #'(lambda (property)
	      (if (lax-plist-member plist property)
		  (progn
		    (setq frame-initial-geometry-arguments
			  (cons property
				(cons (lax-plist-get plist property)
				      frame-initial-geometry-arguments)))
		    (setq plist (lax-plist-remprop plist property)))))
	  '(height width top left user-size user-position))
  plist)

;; XEmacs change: Emacs has focus-follows-mouse here, which lets them
;; Customize it.  XEmacs has it builtin.  Should that change?

;; XEmacs change: we have focus-frame instead of multiple foo-focus-frame
;; functions.
(defun select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible."
  (raise-frame frame)
  (focus-frame frame)  ;; This also selects FRAME
  ;; XEmacs change: This is a bad idea; you should in general never warp the
  ;; pointer unless the user asks for it.
  ;;(if focus-follows-mouse
  ;;    (set-mouse-position (selected-window) (1- (frame-width frame)) 0)))
  )

(defun other-frame (arg)
  "Select the ARG'th different visible frame, and raise it.
All frames are arranged in a cyclic order.
This command selects the frame ARG steps away in that order.
A negative ARG moves in the opposite order.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil."
  (interactive "p")
  (let ((frame (selected-frame)))
    (while (> arg 0)
      (setq frame (next-frame frame 'visible-nomini))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (next-frame frame 'visible-nomini)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq frame (previous-frame frame 'visible-nomini))
      (while (not (eq (frame-visible-p frame) t))
	(setq frame (previous-frame frame 'visible-nomini)))
      (setq arg (1+ arg)))
    (select-frame-set-input-focus frame)))

(defun iconify-or-deiconify-frame ()
  "Iconify the selected frame, or deiconify if it's currently an icon."
  (interactive)
  (if (lax-plist-get (frame-properties) 'visibility)
      (iconify-frame)
    (make-frame-visible)))

(defun make-frame-names-alist ()
  (let* ((current-frame (selected-frame))
	 (falist
	  (cons
	   (cons (frame-property current-frame 'name) current-frame) nil))
	 (frame (next-frame current-frame t)))
    (while (not (eq frame current-frame))
      (progn
	(setq falist (cons (cons (frame-property frame 'name) frame) falist))
	(setq frame (next-frame frame t))))
    falist))

(defvar frame-name-history nil)
(defun select-frame-by-name (name)
  "Select the frame on the current terminal whose name is NAME and raise it.
If there is no frame by that name, signal an error."
  (interactive
   (let* ((frame-names-alist (make-frame-names-alist))
	   (default (car (car frame-names-alist)))
	   (input (completing-read
		   (format "Select Frame (default %s): " default)
		   frame-names-alist nil t nil 'frame-name-history default)))
     ;; XEmacs change: use the last param of completing-read to simplify.
     (list input)))
  (let* ((frame-names-alist (make-frame-names-alist))
	 (frame (cdr (assoc name frame-names-alist))))
    (or frame
	(error "There is no frame named `%s'" name))
    (make-frame-visible frame)
    ;; XEmacs change: make-frame-visible implies (raise-frame)
    ;; (raise-frame frame)
    ;; XEmacs change: we defined this function, might as well use it.
    (select-frame-set-input-focus frame)))

;; XEmacs-added utility functions

(defmacro save-selected-frame (&rest body)
  "Execute forms in BODY, then restore the selected frame.
The value returned is the value of the last form in BODY."
  (let ((old-frame (gensym "ssf")))
    `(let ((,old-frame (selected-frame)))
       (unwind-protect
           (progn ,@body)
         (select-frame ,old-frame)))))

(defmacro with-selected-frame (frame &rest body)
  "Execute forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY."
  `(save-selected-frame
     (select-frame ,frame)
     ,@body))

; This is in C in Emacs
(defun frame-list ()
  "Return a list of all frames on all devices/consoles."
  ;; Lists are copies, so nconc is safe here.
  (apply 'nconc (mapcar 'device-frame-list (device-list))))

(defun frame-type (&optional frame)
  "Return the type of the specified frame (e.g. `x' or `tty').
This is equivalent to the type of the frame's device.
Value is `tty' for a tty frame (a character-only terminal),
`x' for a frame that is an X window,
`ns' for a frame that is a NeXTstep window (not yet implemented),
`mswindows' for a frame that is a MS Windows desktop window,
`msprinter' for a frame that is a MS Windows print job,
`stream' for a stream frame (which acts like a stdio stream), and
`dead' for a deleted frame."
  (or frame (setq frame (selected-frame)))
  (if (not (frame-live-p frame)) 'dead
    (device-type (frame-device frame))))

(defun device-or-frame-p (object)
  "Return non-nil if OBJECT is a device or frame."
  (or (devicep object)
      (framep object)))

(defun device-or-frame-type (device-or-frame)
  "Return the type (e.g. `x' or `tty') of DEVICE-OR-FRAME.
DEVICE-OR-FRAME should be a device or a frame object.  See `device-type'
for a description of the possible types."
  (if (devicep device-or-frame)
      (device-type device-or-frame)
    (frame-type device-or-frame)))

(defun fw-frame (obj)
  "Given a frame or window, return the associated frame.
Return nil otherwise."
  (cond ((windowp obj) (window-frame obj))
	((framep obj) obj)
	(t nil)))


;;;; Frame configurations

(defun current-frame-configuration ()
  "Return a list describing the positions and states of all frames.
Its car is `frame-configuration'.
Each element of the cdr is a list of the form (FRAME PLIST WINDOW-CONFIG),
where
  FRAME is a frame object,
  PLIST is a property list specifying some of FRAME's properties, and
  WINDOW-CONFIG is a window configuration object for FRAME."
  (cons 'frame-configuration
	(mapcar (function
		 (lambda (frame)
		   (list frame
			 (frame-properties frame)
			 (current-window-configuration frame))))
		(frame-list))))

(defun set-frame-configuration (configuration &optional nodelete)
  "Restore the frames to the state described by CONFIGURATION.
Each frame listed in CONFIGURATION has its position, size, window
configuration, and other properties set as specified in CONFIGURATION.
Ordinarily, this function deletes all existing frames not
listed in CONFIGURATION.  But if optional second argument NODELETE
is given and non-nil, the unwanted frames are iconified instead."
  (or (frame-configuration-p configuration)
      (signal 'wrong-type-argument
	      (list 'frame-configuration-p configuration)))
  (let ((config-alist (cdr configuration))
	frames-to-delete)
    (mapc #'(lambda (frame)
	      (let ((properties (assq frame config-alist)))
		(if properties
		    (progn
		      (set-frame-properties
		       frame
		       ;; Since we can't set a frame's minibuffer status,
		       ;; we might as well omit the parameter altogether.
		       (lax-plist-remprop (nth 1 properties) 'minibuffer))
		      (set-window-configuration (nth 2 properties)))
		  (setq frames-to-delete (cons frame frames-to-delete)))))
	  (frame-list))
    (if nodelete
	;; Note: making frames invisible here was tried
	;; but led to some strange behavior--each time the frame
	;; was made visible again, the window manager asked afresh
	;; for where to put it.
	(mapc #'iconify-frame frames-to-delete)
      (mapc #'delete-frame frames-to-delete))))

; XEmacs change: this function is in subr.el in Emacs.
; That's because they don't always include frame.el, while we do.

(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (and (consp object)
       (eq (car object) 'frame-configuration)))


;;;; Convenience functions for accessing and interactively changing
;;;; frame parameters.

(defun frame-height (&optional frame)
  "Return number of lines available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (frame-property frame 'height))

(defun frame-width (&optional frame)
  "Return number of columns available for display on FRAME.
If FRAME is omitted, describe the currently selected frame."
  (frame-property frame 'width))

(defalias 'set-default-font 'set-frame-font)

;; XEmacs change: this function differs significantly from Emacs.
(defun set-frame-font (font-name &optional keep-size)
  "Set the font of the selected frame to FONT-NAME.
When called interactively, prompt for the name of the font to use.
To get the frame's current default font, use `(face-font-name 'default)'.

The default behavior is to keep the numbers of lines and columns in
the frame, thus may change its pixel size. If optional KEEP-SIZE is
non-nil (interactively, prefix argument) the current frame size (in
pixels) is kept by adjusting the numbers of the lines and columns."
  (interactive
   (let* ((frame (selected-frame))
	  (completion-ignore-case t)
	  (font (completing-read "Font name: "
			 (mapcar #'list
				 (font-list "*" frame))
			 nil nil nil nil
			 (face-font-name 'default frame))))
     (list font current-prefix-arg)))
  (let* ((frame (selected-frame))
	 (fht (frame-pixel-height frame))
	 (fwd (frame-pixel-width frame))
	 (face-list-to-change (face-list)))
    (when (eq (device-type) 'mswindows)
      (setq face-list-to-change
	    (delq 'border-glyph face-list-to-change)))
    ;; FIXME: Is it sufficient to just change the default face, due to
    ;; face inheritance?
    (dolist (face face-list-to-change)
      (when (face-font-instance face)
	(condition-case c
	    (set-face-font face font-name frame)
	  (error
	   (display-error c nil)
	   (sit-for 1)))))
    (if keep-size
	(set-frame-pixel-size frame fwd fht)))
  (run-hooks 'after-setting-font-hook))

(defun set-frame-property (frame prop val)
  "Set property PROP of FRAME to VAL.  See `set-frame-properties'."
  (set-frame-properties frame (list prop val)))

;; XEmacs change: this function differs significantly from Emacs.
(defun set-background-color (color-name)
  "Set the background color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use
`(face-background-name 'default)'."
  (interactive (list (read-color "Color: ")))
  ;; (set-face-foreground 'text-cursor color-name (selected-frame))
  (set-face-background 'default color-name (selected-frame)))

;; XEmacs change: this function differs significantly from Emacs.
(defun set-foreground-color (color-name)
  "Set the foreground color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use
`(face-foreground-name 'default)'."
  (interactive (list (read-color "Color: ")))
  (set-face-foreground 'default color-name (selected-frame)))

;; XEmacs change: this function differs significantly from Emacs.
(defun set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current cursor color, use
'(face-background-name 'text-cursor)'."
  (interactive (list (read-color "Color: ")))
  (set-face-background 'text-cursor color-name (selected-frame)))

;; XEmacs change: this function differs significantly from Emacs.
(defun set-mouse-color (color-name)
  "Set the color of the mouse pointer of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current mouse color, use
`(face-foreground-name 'pointer)'."
  (interactive (list (read-color "Color: ")))
  (set-face-foreground 'pointer color-name (selected-frame)))

;; XEmacs change: this function differs significantly from Emacs.
(defun set-border-color (color-name)
  "Set the color of the border of the selected frame to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current border color, use
`(face-foreground-name 'border-glyph)'."
  (interactive (list (read-color "Color: ")))
  (set-face-foreground 'border-glyph color-name (selected-frame)))

;;; BEGIN XEmacs addition
;;; This is the traditional XEmacs auto-raise and auto-lower, which applies
;;; to all frames.

(defcustom auto-raise-frame nil
  "*If true, frames will be raised to the top when selected.
Under X, most ICCCM-compliant window managers will have an option to do this
for you, but this variable is provided in case you're using a broken WM."
  :type 'boolean
  :group 'frames)

(defcustom auto-lower-frame nil
  "*If true, frames will be lowered to the bottom when no longer selected.
Under X, most ICCCM-compliant window managers will have an option to do this
for you, but this variable is provided in case you're using a broken WM."
  :type 'boolean
  :group 'frames)

(defun default-select-frame-hook ()
  "Implement the `auto-raise-frame' variable.
For use as the value of `select-frame-hook'."
  (if auto-raise-frame (raise-frame (selected-frame))))

(defun default-deselect-frame-hook ()
  "Implement the `auto-lower-frame' variable.
For use as the value of `deselect-frame-hook'."
  (if auto-lower-frame (lower-frame (selected-frame)))
  (highlight-extent nil nil))

(or select-frame-hook
    (add-hook 'select-frame-hook 'default-select-frame-hook))

(or deselect-frame-hook
    (add-hook 'deselect-frame-hook 'default-deselect-frame-hook))

;;; END XEmacs addition
;;; Following is the Emacs auto-raise/auto-lower interface, which lets the
;;; user select individual frames to auto-raise and auto-lower

;; XEmacs addition: the next two variables do not appear in Emacs
(defvar auto-raise-specifier (make-boolean-specifier auto-raise-frame)
  "Specifier that determines which frames should auto-raise.
A value of `t' means that a frame auto-raises; `nil' means it does not.")

(defvar auto-lower-specifier (make-boolean-specifier auto-lower-frame)
  "Specifier that determines which frames should auto-lower.
A value of `t' means that a frame auto-lowers; `nil' means it does not.")

;; XEmacs change: use specifiers instead of frame-parameters
(defun auto-raise-mode (arg)
  "Toggle whether or not the selected frame should auto-raise.
With arg, turn auto-raise mode on if and only if arg is positive.
Note that this controls Emacs's own auto-raise feature.
Some window managers allow you to enable auto-raise for certain windows.
You can use that for Emacs windows if you wish, but if you do,
that is beyond the control of Emacs and this command has no effect on it."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (specifier-instance auto-raise-specifier (selected-frame))
		-1 1)))
  (if (> arg 0)
      (progn
	(raise-frame (selected-frame))
	(add-hook 'select-frame-hook 'default-select-frame-hook))
    (set-specifier auto-raise-specifier (> arg 0) (selected-frame))))

;; XEmacs change: use specifiers instead of frame-parameters
(defun auto-lower-mode (arg)
  "Toggle whether or not the selected frame should auto-lower.
With arg, turn auto-lower mode on if and only if arg is positive.
Note that this controls Emacs's own auto-lower feature.
Some window managers allow you to enable auto-lower for certain windows.
You can use that for Emacs windows if you wish, but if you do,
that is beyond the control of Emacs and this command has no effect on it."
  (interactive "P")
  (if (null arg)
      (setq arg
	    (if (specifier-instance auto-lower-specifier (selected-frame))
		-1 1)))
  (if (> arg 0)
      (progn
	(lower-frame (selected-frame))
	(add-hook 'deselect-frame-hook 'default-deselect-frame-hook))
    (set-specifier auto-lower-specifier (> arg 0) (selected-frame))))

;; XEmacs omission: XEmacs does not support changing the frame name
;(defun set-frame-name (name)
;  "Set the name of the selected frame to NAME.
;When called interactively, prompt for the name of the frame.
;The frame name is displayed on the modeline if the terminal displays only
;one frame, otherwise the name is displayed on the frame's caption bar."
;  (interactive "sFrame name: ")
;  (modify-frame-parameters (selected-frame)
;			   (list (cons 'name name))))

;; XEmacs omission: XEmacs attaches scrollbars to windows, not frames.
;; See window-hscroll and ... what? window-start?
;(defun frame-current-scroll-bars (&optional frame)
;  "Return the current scroll-bar settings in frame FRAME.
;Value is a cons (VERTICAL . HORISONTAL) where VERTICAL specifies the
;current location of the vertical scroll-bars (left, right, or nil),
;and HORISONTAL specifies the current location of the horisontal scroll
;bars (top, bottom, or nil)."
;  (let ((vert (frame-parameter frame 'vertical-scroll-bars))
;	(hor nil))
;    (unless (memq vert '(left right nil))
;      (setq vert default-frame-scroll-bars))
;    (cons vert hor)))

;;;; Frame/display capabilities.
(defun display-mouse-p (&optional display)
  "Return non-nil if DISPLAY has a mouse available.
DISPLAY can be a frame, a device, a console, or nil (meaning the
selected frame)."
  (case (framep-on-display display)
    ;; We assume X, NeXTstep, and GTK *always* have a pointing device
    ((x ns gtk) t)
    (mswindows (> (declare-boundp mswindows-num-mouse-buttons) 0))
    (tty
     (and-fboundp 'gpm-is-supported-p
       (gpm-is-supported-p (display-device display))))
    (t nil)))

(defun display-popup-menus-p (&optional display)
  "Return non-nil if popup menus are supported on DISPLAY.
DISPLAY can be a frame, a device, a console, or nil (meaning the selected
frame).  Support for popup menus requires that the mouse be available."
  (and
   (memq (framep-on-display display) '(x ns gtk mswindows))
   (display-mouse-p display)))

(defun display-graphic-p (&optional display)
  "Return non-nil if DISPLAY is a graphic display.
Graphical displays are those which are capable of displaying several
frames and several different fonts at once.  This is true for displays
that use a window system such as X, and false for text-only terminals.
DISPLAY can be a frame, a device, a console, or nil (meaning the selected
frame)."
  (memq (framep-on-display display) '(x ns gtk mswindows)))

(defun display-images-p (&optional display)
  "Return non-nil if DISPLAY can display images.
DISPLAY can be a frame, a device, a console, or nil (meaning the selected
frame)."
  (display-graphic-p display))

(defalias 'display-multi-frame-p 'display-graphic-p)
(defalias 'display-multi-font-p 'display-graphic-p)

(defun display-selections-p (&optional display)
  "Return non-nil if DISPLAY supports selections.
A selection is a way to transfer text or other data between programs
via special system buffers called `selection' or `cut buffer' or
`clipboard'.
DISPLAY can be a frame, a device, a console, or nil (meaning the selected
frame)."
  (memq (framep-on-display display) '(x ns gtk mswindows)))

(defun display-screens (&optional display)
  "Return the number of screens associated with DISPLAY."
  (device-num-screens (display-device display)))

(defun display-pixel-height (&optional display)
  "Return the height of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (device-pixel-height (display-device display)))

(defun display-pixel-width (&optional display)
  "Return the width of DISPLAY's screen in pixels.
For character terminals, each character counts as a single pixel."
  (device-pixel-width (display-device display)))

(defun display-mm-height (&optional display)
  "Return the height of DISPLAY's screen in millimeters.
If the information is unavailable, value is nil."
  (device-mm-height (display-device display)))

(defun display-mm-width (&optional display)
  "Return the width of DISPLAY's screen in millimeters.
If the information is unavailable, value is nil."
  (device-mm-width (display-device display)))

(defun display-backing-store (&optional display)
  "Return the backing store capability of DISPLAY's screen.
The value may be `always', `when-mapped', `not-useful', or nil if
the question is inapplicable to a certain kind of display."
  (device-backing-store (display-device display)))

(defun display-save-under (&optional display)
  "Return non-nil if DISPLAY's screen supports the SaveUnder feature."
  (device-save-under (display-device display)))

(defun display-planes (&optional display)
  "Return the number of planes supported by DISPLAY."
  (device-bitplanes (display-device display)))

(defun display-color-cells (&optional display)
  "Return the number of color cells supported by DISPLAY."
  (device-color-cells (display-device display)))

(defun display-visual-class (&optional display)
  "Returns the visual class of DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'."
  (case (framep-on-display display)
    (x (declare-fboundp (x-display-visual-class (display-device display))))
    (gtk (declare-fboundp (gtk-display-visual-class (display-device display))))
    (mswindows (let ((planes (display-planes display)))
		 (cond ((eq planes 1) 'static-gray)
		       ((eq planes 4) 'static-color)
		       ((> planes 8) 'true-color)
		       (t 'pseudo-color))))
    (t 'static-gray)))


;; XEmacs change: omit the Emacs 18 compatibility functions:
;; screen-height, screen-width, set-screen-height, and set-screen-width.

(defun delete-other-frames (&optional frame)
  "Delete all frames except FRAME.
If FRAME uses another frame's minibuffer, the minibuffer frame is
left untouched.  FRAME nil or omitted means use the selected frame."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (let* ((mini-frame (window-frame (minibuffer-window frame)))
	 (frames (delq mini-frame (delq frame (frame-list)))))
    (mapc 'delete-frame frames)))

;; XEmacs change: we still use delete-frame-hook
;; miscellaneous obsolescence declarations
;(defvaralias 'delete-frame-hook 'delete-frame-functions)
;(make-obsolete-variable 'delete-frame-hook 'delete-frame-functions "21.4")


;; Highlighting trailing whitespace.
;; XEmacs omission: this functionality is provided by whitespace-mode in the
;; text-modes package.

;(make-variable-buffer-local 'show-trailing-whitespace)

;(defcustom show-trailing-whitespace nil
;  "*Non-nil means highlight trailing whitespace in face `trailing-whitespace'.
;
;Setting this variable makes it local to the current buffer."
;  :tag "Highlight trailing whitespace."
;  :type 'boolean
;  :group 'font-lock)


;; Scrolling
;; XEmacs omission: This functionality is always enabled on XEmacs.

;(defgroup scrolling nil
;  "Scrolling windows."
;  :version "21.1"
;  :group 'frames)

;(defcustom auto-hscroll-mode t
;  "*Allow or disallow automatic scrolling windows horizontally.
;If non-nil, windows are automatically scrolled horizontally to make
;point visible."
;  :version "21.1"
;  :type 'boolean
;  :group 'scrolling)
;(defvaralias 'automatic-hscrolling 'auto-hscroll-mode)


;; Blinking cursor
;; XEmacs omission: this functionality is provided by blink-cursor in the
;; edit-utils package.

; (defgroup cursor nil
;   "Displaying text cursors."
;   :version "21.1"
;   :group 'frames)

; (defcustom blink-cursor-delay 0.5
;   "*Seconds of idle time after which cursor starts to blink."
;   :tag "Delay in seconds."
;   :type 'number
;   :group 'cursor)

; (defcustom blink-cursor-interval 0.5
;   "*Length of cursor blink interval in seconds."
;   :tag "Blink interval in seconds."
;   :type 'number
;   :group 'cursor)

; (defvar blink-cursor-idle-timer nil
;   "Timer started after `blink-cursor-delay' seconds of Emacs idle time.
; The function `blink-cursor-start' is called when the timer fires.")

; (defvar blink-cursor-timer nil
;   "Timer started from `blink-cursor-start'.
; This timer calls `blink-cursor' every `blink-cursor-interval' seconds.")

; (defvar blink-cursor-mode nil
;   "Non-nil means blinking cursor is active.")

; (defun blink-cursor-mode (arg)
;   "Toggle blinking cursor mode.
; With a numeric argument, turn blinking cursor mode on iff ARG is positive.
; When blinking cursor mode is enabled, the cursor of the selected
; window blinks.

; Note that this command is effective only when Emacs
; displays through a window system, because then Emacs does its own
; cursor display.  On a text-only terminal, this is not implemented."
;   (interactive "P")
;   (let ((on-p (if (null arg)
; 		  (not blink-cursor-mode)
; 		(> (prefix-numeric-value arg) 0))))
;     (if blink-cursor-idle-timer
; 	(cancel-timer blink-cursor-idle-timer))
;     (if blink-cursor-timer
; 	(cancel-timer blink-cursor-timer))
;     (setq blink-cursor-idle-timer nil
; 	  blink-cursor-timer nil
; 	  blink-cursor-mode nil)
;     (if on-p
; 	(progn
; 	  ;; Hide the cursor.
; 	  ;(internal-show-cursor nil nil)
; 	  (setq blink-cursor-idle-timer
; 		(run-with-idle-timer blink-cursor-delay
; 				     blink-cursor-delay
; 				     'blink-cursor-start))
; 	  (setq blink-cursor-mode t))
;       (internal-show-cursor nil t))))

; ;; Note that this is really initialized from startup.el before
; ;; the init-file is read.

; (defcustom blink-cursor nil
;   "*Non-nil means blinking cursor mode is active."
;   :group 'cursor
;   :tag "Blinking cursor"
;   :type 'boolean
;   :set #'(lambda (symbol value)
; 	   (set-default symbol value)
; 	   (blink-cursor-mode (or value 0))))

; (defun blink-cursor-start ()
;   "Timer function called from the timer `blink-cursor-idle-timer'.
; This starts the timer `blink-cursor-timer', which makes the cursor blink
; if appropriate.  It also arranges to cancel that timer when the next
; command starts, by installing a pre-command hook."
;   (when (null blink-cursor-timer)
;     (add-hook 'pre-command-hook 'blink-cursor-end)
;     (setq blink-cursor-timer
; 	  (run-with-timer blink-cursor-interval blink-cursor-interval
; 			  'blink-cursor-timer-function))))

; (defun blink-cursor-timer-function ()
;   "Timer function of timer `blink-cursor-timer'."
;   (internal-show-cursor nil (not (internal-show-cursor-p))))

; (defun blink-cursor-end ()
;   "Stop cursor blinking.
; This is installed as a pre-command hook by `blink-cursor-start'.
; When run, it cancels the timer `blink-cursor-timer' and removes
; itself as a pre-command hook."
;   (remove-hook 'pre-command-hook 'blink-cursor-end)
;   (internal-show-cursor nil t)
;   (cancel-timer blink-cursor-timer)
;   (setq blink-cursor-timer nil))


;; Hourglass pointer
;; XEmacs omission: this functionality is provided elsewhere.

; (defcustom display-hourglass t
;   "*Non-nil means show an hourglass pointer when running under a window system."
;   :tag "Hourglass pointer"
;   :type 'boolean
;   :group 'cursor)

; (defcustom hourglass-delay 1
;   "*Seconds to wait before displaying an hourglass pointer."
;   :tag "Hourglass delay"
;   :type 'number
;   :group 'cursor)

; 
; (defcustom cursor-in-non-selected-windows t
;   "*Non-nil means show a hollow box cursor in non-selected-windows.
; If nil, don't show a cursor except in the selected window.
; Use Custom to set this variable to get the display updated."
;   :tag "Cursor in non-selected windows"
;   :type 'boolean
;   :group 'cursor
;   :set #'(lambda (symbol value)
; 	   (set-default symbol value)
; 	   (force-mode-line-update t)))


;;;; Key bindings
;; XEmacs change: these keybindings are in keydef.el.

;(define-key ctl-x-5-map "2" 'make-frame-command)
;(define-key ctl-x-5-map "1" 'delete-other-frames)
;(define-key ctl-x-5-map "0" 'delete-frame)
;(define-key ctl-x-5-map "o" 'other-frame)


;;; XEmacs addition: nothing below this point appears in the Emacs version.

;;; Iconifying emacs.
;;;
;;; The function iconify-emacs replaces every non-iconified emacs window
;;; with a *single* icon.  Iconified emacs windows are left alone.  When
;;; emacs is in this globally-iconified state, de-iconifying any emacs icon
;;; will uniconify all frames that were visible, and iconify all frames
;;; that were not.  This is done by temporarily changing the value of
;;; `map-frame-hook' to `deiconify-emacs' (which should never be called
;;; except from the map-frame-hook while emacs is iconified).
;;;
;;; The title of the icon representing all emacs frames is controlled by
;;; the variable `icon-name'.  This is done by temporarily changing the
;;; value of `frame-icon-title-format'.  Unfortunately, this changes the
;;; titles of all emacs icons, not just the "big" icon.
;;;
;;; It would be nice if existing icons were removed and restored by
;;; iconifying the emacs process, but I couldn't make that work yet.

(defvar icon-name nil) ; set this at run time, not load time.

(defvar iconification-data nil)

(defun iconify-emacs ()
  "Replace every non-iconified FRAME with a *single* icon.
Iconified frames are left alone.  When XEmacs is in this
globally-iconified state, de-iconifying any emacs icon will uniconify
all frames that were visible, and iconify all frames that were not."
  (interactive)
  (if iconification-data (error "already iconified?"))
  (let* ((frames (frame-list))
	 (rest frames)
	 (me (selected-frame))
	 frame)
    (while rest
      (setq frame (car rest))
      (setcar rest (cons frame (frame-visible-p frame)))
;      (if (memq (cdr (car rest)) '(icon nil))
;	  (progn
;	    (make-frame-visible frame) ; deiconify, and process the X event
;	    (sleep-for 500 t) ; process X events; I really want to XSync() here
;	    ))
      (or (eq frame me) (make-frame-invisible frame))
      (setq rest (cdr rest)))
    (or (boundp 'map-frame-hook) (setq map-frame-hook nil))
    (or icon-name
	(setq icon-name (concat invocation-name " @ " (system-name))))
    (setq iconification-data
	    (list frame-icon-title-format map-frame-hook frames)
	  frame-icon-title-format icon-name
	  map-frame-hook 'deiconify-emacs)
    (iconify-frame me)))


(defun deiconify-emacs (&optional ignore)
  (or iconification-data (error "not iconified?"))
  (setq frame-icon-title-format (car iconification-data)
	map-frame-hook (car (cdr iconification-data))
	iconification-data (car (cdr (cdr iconification-data))))
  (while iconification-data
    (let ((visibility (cdr (car iconification-data))))
      (cond (visibility  ;; JV  (Note non-nil means visible in XEmacs)
	     (make-frame-visible (car (car iconification-data))))
;	    (t ;; (eq visibility 'icon) ;; JV Not in XEmacs!!!
;	     (make-frame-visible (car (car iconification-data)))
;	     (sleep-for 500 t) ; process X events; I really want to XSync() here
;	     (iconify-frame (car (car iconification-data))))
	    ;; (t nil)
	    ))
    (setq iconification-data (cdr iconification-data))))

(defun suspend-or-iconify-emacs ()
  "Call iconify-emacs if using a window system, otherwise suspend.

`suspend' here can mean different things; if the current TTY console was
created by gnuclient, that console is suspended, and the related devices and
frames are removed from the display.  Otherwise the Emacs process as a whole
is suspended--that is, the traditional Unix suspend takes place.  "
  (interactive)
  (cond ((device-on-window-system-p)
	 (iconify-emacs))
	((and (eq (device-type) 'tty)
	      (declare-fboundp (console-tty-controlling-process
				(selected-console))))
	 (suspend-console (selected-console)))
	(t
	 (suspend-emacs))))

;; This is quite a mouthful, but it should be descriptive, as it's
;; bound to C-z.  FSF takes the easy way out by binding C-z to
;; different things depending on window-system.  We can't do the same,
;; because we allow simultaneous X and TTY consoles.
(defun suspend-emacs-or-iconify-frame ()
  "Iconify the selected frame if using a window system, otherwise suspend.

`suspend' here can mean different things; if the current TTY console was
created by gnuclient, the console is suspended, and the related devices and
frames are removed from the display.  Otherwise the Emacs process as a whole
is suspended--that is, the traditional Unix suspend takes place.  "
  (interactive)
  (cond ((device-on-window-system-p)
	 (iconify-frame))
	((and (eq (frame-type) 'tty)
	      (declare-fboundp (console-tty-controlling-process
				(selected-console))))
	 (suspend-console (selected-console)))
	(t
	 (suspend-emacs))))


;;; Application-specific frame-management

(defcustom get-frame-for-buffer-default-frame-name nil
  "*The default frame to select; see doc of `get-frame-for-buffer'."
  :type 'string
  :group 'frames)

(defcustom get-frame-for-buffer-default-instance-limit nil
  "*The default instance limit for creating new frames; 
see doc of `get-frame-for-buffer'."
  :type 'integer
  :group 'frames)

(defun get-frame-name-for-buffer (buffer)
  (let ((mode (and (get-buffer buffer)
		   (save-excursion (set-buffer buffer)
				   major-mode))))
    (or (get mode 'frame-name)
	get-frame-for-buffer-default-frame-name)))

(defun get-frame-for-buffer-make-new-frame (buffer &optional frame-name plist)
  (let* ((fr (make-frame plist))
	 (w (frame-root-window fr)))
    ;;
    ;; Make the one buffer being displayed in this newly created
    ;; frame be the buffer of interest, instead of something
    ;; random, so that it won't be shown in two-window mode.
    ;; Avoid calling switch-to-buffer here, since that's something
    ;; people might want to call this routine from.
    ;;
    ;; (If the root window doesn't have a buffer, then that means
    ;; there is more than one window on the frame, which can only
    ;; happen if the user has done something funny on the frame-
    ;; creation-hook.  If that's the case, leave it alone.)
    ;;
    (if (window-buffer w)
	(set-window-buffer w buffer))
    fr))

(defcustom get-frame-for-buffer-default-to-current nil
  "*When non-nil, `get-frame-for-buffer' will default to the selected frame."
  :type 'boolean
  :group 'frames)

(defun get-frame-for-buffer-noselect (buffer
				      &optional not-this-window-p on-frame)
  "Return a frame in which to display BUFFER.
This is a subroutine of `get-frame-for-buffer' (which see)."
  (let (name limit)
    (cond
     ((or on-frame (eq (selected-window) (minibuffer-window)))
      ;; don't switch frames if a frame was specified, or to list
      ;; completions from the minibuffer, etc.
      nil)

     ((setq name (get-frame-name-for-buffer buffer))
      ;;
      ;; This buffer's mode expressed a preference for a frame of a particular
      ;; name.  That always takes priority.
      ;;
      (let ((limit (get name 'instance-limit))
	    (defaults (get name 'frame-defaults))
	    (matching-frames '())
	    frames frame already-visible)
	;; Sort the list so that iconic frames will be found last.  They
	;; will be used too, but mapped frames take precedence.  And
	;; fully visible frames come before occluded frames.
        ;; Hidden frames come after really visible ones
	(setq frames
	      (sort (frame-list)
		    #'(lambda (s1 s2)
			(cond ((frame-totally-visible-p s2)
			       nil)
			      ((not (frame-visible-p s2))
			       (frame-visible-p s1))
			      ((eq (frame-visible-p s2) 'hidden)
			       (eq (frame-visible-p s1) t ))
			      ((not (frame-totally-visible-p s2))
			       (and (frame-visible-p s1)
				    (frame-totally-visible-p s1)))))))
	;; but the selected frame should come first, even if it's occluded,
	;; to minimize thrashing.
	(setq frames (cons (selected-frame)
			   (delq (selected-frame) frames)))

	(setq name (symbol-name name))
	(while frames
	  (setq frame (car frames))
	  (if (equal name (frame-name frame))
	      (if (get-buffer-window buffer frame)
		  (setq already-visible frame
			frames nil)
		(setq matching-frames (cons frame matching-frames))))
	  (setq frames (cdr frames)))
	(cond (already-visible
	       already-visible)
	      ((or (null matching-frames)
		   (eq limit 0) ; means create with reckless abandon
		   (and limit (< (length matching-frames) limit)))
	       (get-frame-for-buffer-make-new-frame
		buffer
		name
		(alist-to-plist (acons 'name name
				       (plist-to-alist defaults)))))
	      (t
	       ;; do not switch any of the window/buffer associations in an
	       ;; existing frame; this function only picks a frame; the
	       ;; determination of which windows on it get reused is up to
	       ;; display-buffer itself.
;;	       (or (window-dedicated-p (selected-window))
;;		   (switch-to-buffer buffer))
	       (car matching-frames)))))

     ((setq limit get-frame-for-buffer-default-instance-limit)
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name, but the user wants a new frame rather than
      ;; reusing the existing one.
      (let* ((defname
	       (or (plist-get default-frame-plist 'name)
		   default-frame-name))
	     (frames
	      (sort (filtered-frame-list #'(lambda (x)
					     (or (frame-visible-p x)
						 (frame-iconified-p x))))
		    #'(lambda (s1 s2)
			(cond ((and (frame-visible-p s1)
				    (not (frame-visible-p s2))))
			      ((and (eq (frame-visible-p s1) t)
				    (eq (frame-visible-p s2) 'hidden)))
			      ((and (frame-visible-p s2)
				    (not (frame-visible-p s1)))
			       nil)
			      ((and (equal (frame-name s1) defname)
				    (not (equal (frame-name s2) defname))))
			      ((and (equal (frame-name s2) defname)
				    (not (equal (frame-name s1) defname)))
			       nil)
			      ((frame-totally-visible-p s2)
			       nil)
			      (t))))))
	;; put the selected frame last.  The user wants a new frame,
	;; so don't reuse the existing one unless forced to.
	(setq frames (append (delq (selected-frame) frames) (list frames)))
	(if (or (eq limit 0) ; means create with reckless abandon
		(< (length frames) limit))
	    (get-frame-for-buffer-make-new-frame buffer)
	  (car frames))))

     (not-this-window-p
      (let ((w-list (windows-of-buffer buffer))
	    f w
	    (first-choice nil)
	    (second-choice (if get-frame-for-buffer-default-to-current
			       (selected-frame)
			     nil))
	    (last-resort nil))
	(while (and w-list (null first-choice))
	  (setq w (car w-list)
		f (window-frame w))
	  (cond ((eq w (selected-window)) nil)
		((not (frame-visible-p f))
		 (if (null last-resort)
		     (setq last-resort f)))
		((eq f (selected-frame))
		 (setq first-choice f))
		((null second-choice)
		 (setq second-choice f)))
	  (setq w-list (cdr w-list)))
	(or first-choice second-choice last-resort)))

     (get-frame-for-buffer-default-to-current (selected-frame))

     (t
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name.  So try to find a frame already displaying this
      ;; buffer.
      ;;
      (let ((w (or (get-buffer-window buffer nil)	; check current first
		   (get-buffer-window buffer 'visible)	; then visible
		   (get-buffer-window buffer 0))))	; then iconic
	(cond ((null w)
	       ;; It's not in any window - return nil, meaning no frame has
	       ;; preference.
	       nil)
	      (t
	       ;; Otherwise, return the frame of the buffer's window.
	       (window-frame w))))))))


;; The pre-display-buffer-function is called for effect, so this needs to
;; actually select the frame it wants.  Fdisplay_buffer() takes notice of
;; changes to the selected frame.
(defun get-frame-for-buffer (buffer &optional not-this-window-p on-frame
				    shrink-to-fit)
  "Select and return a frame in which to display BUFFER.
Normally, the buffer will simply be displayed in the selected frame.
But if the symbol naming the major-mode of the buffer has a `frame-name'
property (which should be a symbol), then the buffer will be displayed in
a frame of that name.  If there is no frame of that name, then one is
created.

If the major-mode doesn't have a `frame-name' property, then the frame
named by `get-frame-for-buffer-default-frame-name' will be used.  If
that is nil (the default) then the currently selected frame will used.

If the frame-name symbol has an `instance-limit' property (an integer)
then each time a buffer of the mode in question is displayed, a new frame
with that name will be created, until there are `instance-limit' of them.
If instance-limit is 0, then a new frame will be created each time.

If a buffer is already displayed in a frame, then `instance-limit' is
ignored, and that frame is used.

If the frame-name symbol has a `frame-defaults' property, then that is
prepended to the `default-frame-plist' when creating a frame for the
first time.

This function may be used as the value of `pre-display-buffer-function',
to cause the `display-buffer' function and its callers to exhibit the
above behavior."
  (let ((frame (get-frame-for-buffer-noselect
		buffer not-this-window-p on-frame)))
    (if (not (eq frame (selected-frame)))
	frame
      (select-frame frame)
      (or (frame-visible-p frame)
	  ;; If the frame was already visible, just focus on it.
	  ;; If it wasn't visible (it was just created, or it used
	  ;; to be iconified) then uniconify, raise, etc.
	  (make-frame-visible frame))
      frame)))

(defun frames-of-buffer (&optional buffer visible-only)
  "Return list of frames that BUFFER is currently being displayed on.
If the buffer is being displayed on the currently selected frame, that frame
is first in the list.  VISIBLE-ONLY will only list non-iconified frames."
  (let ((list (windows-of-buffer buffer))
	(cur-frame (selected-frame))
	next-frame frames save-frame)

    (while list
      (if (memq (setq next-frame (window-frame (car list)))
		frames)
	  nil
	(if (eq cur-frame next-frame)
	    (setq save-frame next-frame)
	  (and
	   (or (not visible-only)
	       (frame-visible-p next-frame))
	   (setq frames (append frames (list next-frame))))))
	(setq list (cdr list)))

    (if save-frame
	(append (list save-frame) frames)
      frames)))

(defcustom temp-buffer-shrink-to-fit nil
  "*When non-nil resize temporary output buffers to minimize blank lines."
  :type 'boolean
  :group 'frames)

(defcustom temp-buffer-max-height .5
  "*Proportion of frame to use for temp windows."
  :type 'number
  :group 'frames)

(defun show-temp-buffer-in-current-frame (buffer)
  "For use as the value of `temp-buffer-show-function':
always displays the buffer in the selected frame, regardless of the behavior
that would otherwise be introduced by the `pre-display-buffer-function', which
is normally set to `get-frame-for-buffer' (which see)."
  (let ((pre-display-buffer-function nil)) ; turn it off, whatever it is
    (let ((window (display-buffer buffer nil nil temp-buffer-shrink-to-fit)))
      (if (not (eq (last-nonminibuf-frame) (window-frame window)))
	  ;; only the pre-display-buffer-function should ever do this.
	  (error "display-buffer switched frames on its own!!"))
      (setq minibuffer-scroll-window window)
      (set-window-start window 1) ; obeys narrowing
      (set-window-point window 1)
      nil)))

(setq pre-display-buffer-function 'get-frame-for-buffer)
(setq temp-buffer-show-function 'show-temp-buffer-in-current-frame)


;; from Bob Weiner <bweiner@pts.mot.com>, modified by Ben Wing

;; By adding primitives to directly access the window hierarchy,
;; we can move many functions into Lisp.  We do it this way
;; because the implementations are simpler in Lisp, and because
;; new functions like this can be added without requiring C
;; additions.

(defun frame-utmost-window-2 (window position left-right-p major-end-p
				     minor-end-p)
  ;; LEFT-RIGHT-P means we're looking for the leftmost or rightmost
  ;; window, instead of the highest or lowest.  In this case, we
  ;; say that the "major axis" goes left-to-right instead of top-to-
  ;; bottom.  The "minor axis" always goes perpendicularly.
  ;;
  ;; If MAJOR-END-P is t, we're looking for a windows that abut the
  ;; end (i.e. right or bottom) of the major axis, instead of the
  ;; start.
  ;;
  ;; If MINOR-END-P is t, then we want to start counting from the
  ;; end of the minor axis instead of the beginning.
  ;;
  ;; Here's the general idea: Imagine we're trying to count the number
  ;; of windows that abut the top; call this function foo().  So, we
  ;; start with the root window.  If this is a vertical combination
  ;; window, then foo() applied to the root window is the same as
  ;; foo() applied to the first child.  If the root is a horizontal
  ;; combination window, then foo() applied to the root is the
  ;; same as the sum of foo() applied to each of the children.
  ;; Otherwise, the root window is a leaf window, and foo() is 1.
  ;; Now it's clear that, each time foo() encounters a leaf window,
  ;; it's encountering a different window that abuts the top.
  ;; With a little examining, you can see that foo encounters the
  ;; top-abutting windows in order from left to right.  We can
  ;; modify foo() to return the nth top-abutting window by simply
  ;; keeping a global variable that is decremented each time
  ;; foo() encounters a leaf window and would return 1.  If the
  ;; global counter gets to zero, we've encountered the window
  ;; we were looking for, so we exit right away using a `throw'.
  ;; Otherwise, we make sure that all normal paths return nil.

  (let (child)
    (cond ((setq child (if left-right-p
			   (window-first-hchild window)
			 (window-first-vchild window)))
	   (if major-end-p
	       (while (window-next-child child)
		 (setq child (window-next-child child))))
	   (frame-utmost-window-2 child position left-right-p major-end-p
				  minor-end-p))
	  ((setq child (if left-right-p
			   (window-first-vchild window)
			 (window-first-hchild window)))
	   (if minor-end-p
	       (while (window-next-child child)
		 (setq child (window-next-child child))))
	   (while child
	     (frame-utmost-window-2 child position left-right-p major-end-p
				    minor-end-p)
	     (setq child (if minor-end-p
			     (window-previous-child child)
			   (window-next-child child))))
	   nil)
	  (t
	   (setcar position (1- (car position)))
	   (if (= (car position) 0)
	       (throw 'fhw-exit window)
	     nil)))))

(defun frame-utmost-window-1 (frame position left-right-p major-end-p)
  (let (minor-end-p)
    (or frame (setq frame (selected-frame)))
    (or position (setq position 0))
    (if (>= position 0)
	(setq position (1+ position))
      (setq minor-end-p t)
      (setq position (- position)))
    (catch 'fhw-exit
      ;; we use a cons here as a simple form of call-by-reference.
      ;; scheme has "boxes" for the same purpose.
      (frame-utmost-window-2 (frame-root-window frame) (list position)
			     left-right-p major-end-p minor-end-p))))


(defun frame-highest-window (&optional frame position)
  "Return the highest window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the top of the frame: 0 means the leftmost window abutting the
 top of the frame, 1 the next-leftmost, etc.  POSITION can also
 be less than zero: -1 means the rightmost window abutting the
 top of the frame, -2 the next-rightmost, etc.
If omitted, POSITION defaults to 0, i.e. the leftmost highest window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position nil nil))

(defun frame-lowest-window (&optional frame position)
  "Return the lowest window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the bottom of the frame: 0 means the leftmost window abutting the
 bottom of the frame, 1 the next-leftmost, etc.  POSITION can also
 be less than zero: -1 means the rightmost window abutting the
 bottom of the frame, -2 the next-rightmost, etc.
If omitted, POSITION defaults to 0, i.e. the leftmost lowest window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position nil t))

(defun frame-leftmost-window (&optional frame position)
  "Return the leftmost window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the left edge of the frame: 0 means the highest window abutting the
 left edge of the frame, 1 the next-highest, etc.  POSITION can also
 be less than zero: -1 means the lowest window abutting the
 left edge of the frame, -2 the next-lowest, etc.
If omitted, POSITION defaults to 0, i.e. the highest leftmost window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position t nil))

(defun frame-rightmost-window (&optional frame position)
  "Return the rightmost window on FRAME which is at POSITION.
If omitted, FRAME defaults to the currently selected frame.
POSITION is used to distinguish between multiple windows that abut
 the right edge of the frame: 0 means the highest window abutting the
 right edge of the frame, 1 the next-highest, etc.  POSITION can also
 be less than zero: -1 means the lowest window abutting the
 right edge of the frame, -2 the next-lowest, etc.
If omitted, POSITION defaults to 0, i.e. the highest rightmost window.
If there is no window at the given POSITION, return nil."
  (frame-utmost-window-1 frame position t t))



;; frame properties.

(put 'cursor-color 'frame-property-alias [text-cursor background])
(put 'modeline 'frame-property-alias 'has-modeline-p)


(provide 'frame)

;;; frame.el ends here
