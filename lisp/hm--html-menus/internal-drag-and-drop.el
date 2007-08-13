;;; $Id: internal-drag-and-drop.el,v 1.3 1997/03/28 02:28:42 steve Exp $
;;; 
;;; Copyright (C) 1996, 1997 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 1, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	This package provides functions to define and call internal
;;;	drag and drop actions in the emacs. One could start such an 
;;;	action by clicking with the mouse in the source buffer and 
;;;	then in the destination buffer. The action could depend on
;;;	the points where you've clicked with the mouse, on the state
;;;	of the region, the point, the mark and any other properties
;;;	of the source and the destination buffers. The actions are
;;;	defined by the variable `idd-actions', which is a buffer local
;;;	variable. The following is an example for the hm--html-mode:
;;;	(defvar hm--html-idd-actions
;;;	  '((nil (((idd-if-major-mode-p . dired-mode)
;;;		   (idd-if-dired-file-on-line-p 
;;;				. ".*\\.\\(gif\\)\\|\\(jpq\\)"))
;;;		  hm--html-idd-add-include-image-from-dired-line)
;;;		 (((idd-if-major-mode-p . dired-mode)
;;;		   (idd-if-dired-no-file-on-line-p . nil))
;;;		  hm--html-idd-add-file-link-to-file-on-dired-line)
;;;		 (((idd-if-major-mode-p . dired-mode)
;;;		   (idd-if-dired-no-file-on-line-p . t))
;;;		  hm--html-idd-add-file-link-to-directory-of-buffer)
;;;		 (((idd-if-major-mode-p . w3-mode)
;;;		   (idd-if-url-at-point-p . t))
;;;		  hm--html-idd-add-html-link-from-w3-buffer-point)
;;;		 (((idd-if-major-mode-p . w3-mode))
;;;		  hm--html-idd-add-html-link-to-w3-buffer)
;;;		 (((idd-if-local-file-p . t))
;;;		  hm--html-idd-add-file-link-to-buffer)))
;;;	Look at the variable `idd-actions' for further descriptions.
;;;
;;;	
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your load path directories.
;;;	
;;;	Put the following in your .emacs:
;;;	(autoload 'idd-mouse-drag-and-drop "internal-drag-and-drop"
;;;	   "Performs a drag and drop action.
;;;         At first you must click on the source and 
;;;         after that on the destination."
;;;	   t)
;;;	
;;;	Define actions in the variable `idd-actions'.
;;;
;;;	The variable `idd-global-mouse-keys' defines the mouse keys,
;;;	which are bound to the drag and drop command.
;;;
;;;	The variable `idd-drag-and-drop-mouse-binding-type' determines
;;;	if you've to hold a mouse button down during moving the mouse
;;;	from the source to the destination or not.
;;;

(require 'adapt)
(require 'cl)

(defvar idd-global-mouse-keys (if (adapt-emacs19p)
				  [(meta control mouse-1)]
				[(meta control button1)])
  "The mouse keys for the command `idd-mouse-drag-and-drop'.
The command `idd-mouse-drag-and-drop' is bound during the loading
of the package internal-drag-and-drop to this keys in the global
key map. 

Set it to nil, if you don't want to bind this function during loading.

If the command is already bound in the global keymap during loading,
then this key sequence will not be bind.")

(defvar idd-global-help-mouse-keys (if (adapt-emacs19p)
				       [(meta control mouse-3)]
				     [(meta control button3)])
  "The mouse keys for the command `idd-help-mouse-drag-and-drop'.
The command `idd-help-mouse-drag-and-drop' is bound during the loading
of the package internal-drag-and-drop to this keys in the global
key map. 

Set it to nil, if you don't want to bind this function during loading.

If the command is already bound in the global keymap during loading,
then this key sequence will not be bind.")

(defvar idd-drag-and-drop-mouse-binding-type 'click
  "*The type of the drag and drop mouse binding.
The value maybe `click or 'press-button-during-move.
A value of `click means, that you've to click over the source, leave
the button and click it again over the destination.
A value of 'press-button-during-move means, that you've to press
the button down over the source and hold it until the mouse pointer
is over the destination.

The disadvantage of the `press-button-during-move' type compared with 
the `click' type is, that you can't select a destination region and 
therefore a drag and drop action depending on a selected region can't
be started with that type of mouse binding.")

(defvar idd-actions '((((idd-if-region-active-p . nil))
		       (((idd-if-region-active-p . t))
			idd-action-copy-region))

		      (((idd-if-region-active-p . t))
		       (((idd-if-region-active-p . t))
			idd-action-copy-replace-region))

		      (((idd-if-region-active-p . nil)
			(idd-if-modifiers-p . nil))
		       (((idd-if-region-active-p . t))
			idd-action-move-region))

		      (((idd-if-region-active-p . t)
			(idd-if-modifiers-p . nil))
		       (((idd-if-region-active-p . t))
			idd-action-move-replace-region))
		      )
  "The list with actions, depending on the source and the destination.
The list looks like:
  '((<destination-specification-1> (<source-specification-1> <action-1-1>)
                                   (<source-specification-2> <action-1-2>)
                                   :
     )
    (<destination-specification-2> (<source-specification-1> <action-2-1>)
                                   (<source-specification-2> <action-2-2>)
                                    :
     )
    :
    )
The <source-specification> looks like the following:
  '([(<specification-type> <value>)])
with <specification-type> :== idd-if-minor-mode-p | idd-if-buffer-name-p
			      | idd-if-region-active-p | idd-if-url-at-point-p
                              | idd-if-major-mode-p | idd-if-variable-non-nil-p
                              | idd-if-dired-file-on-line-p
                              | idd-if-dired-no-file-on-line-p
                              | idd-if-local-file-p | idd-if-buffer-name-p
                              | idd-if-modifiers-p | ...

The <specification-type> - functions must have two arguments, the first one
is the source or destination and the second is the <value>. It must return
nil, if the test wasn't successfull and a number (in general 1), which 
specifies the weight of the test function. The weights of all single tests
are added to a summary weight and assigned to the action. The action
with the highest weight is called from the action handler. Look at
the definition of `idd-if-major-mode-p', `idd-if-minor-mode-p' and so on for
examples. Look at the function `idd-get-source-or-destination-alist', if
you wan't to know the structure of the 'source-or-destination' argument
of these functions.

The <destination-specification> looks like <source-specification>,
but in general it could be set to nil in mode specific idd-action
lists.

If <destination-specification-1> or <source-specification-1> is set to
nil, then every source or source matches. `idd-actions' is a
buffer local variable, which should be at least mode depended. So if
the <destination-specification-1> is set to nil it says, that the destination
buffer must only have a specific mode. But however, it's also possible
to define a general `idd-actions' list, where the destination mode is
specified by `idd-if-major-mode-p'.

<action> ist a function, which has two arguments, the first specifies the
source and the second the destination. Look at the function definition
of `idd-action-copy-region' and `idd-action-copy-replace-region'. They are 
examples for such actions.")

(make-variable-buffer-local 'idd-actions)

(defvar idd-help-instead-of-action nil
  "*If this variable is t, then a help buffer is displayed.
No action will be performed if this variable is t.")

(defvar idd-help-start-action-keymap nil
  "Keymap used in an extent in the help buffer to start the action.")

(defvar idd-help-source nil
  "Contains the source of an action. Used only in the help buffer.")

(defvar idd-help-destination nil
  "Contains the destination of an action. Used only in the help buffer.")

(defvar idd-help-start-extent nil
  "The start extent in the help buffer.")

(defun idd-compare-a-specification (source-or-destination
				    specification)
  "Tests if SOURCE-OR-DESTINATION matches the SPECIFICATION.
It returns a value (1 in general) if both are matching or nil."
  (eval (list (car specification)
	      'source-or-destination
	      '(cdr specification))))

(defun idd-compare-specifications-1 (source-or-destination
				     specifications
				     value)
  "Internal function of `idd-compare-specifications'.
VALUE is the value of the last matches."
  (cond ((not specifications) value)
	(t (let ((match (idd-compare-a-specification source-or-destination
						     (car specifications))))
	     (cond ((not match) 0)
		   (t (idd-compare-specifications-1 source-or-destination
						    (cdr specifications)
						    (+ value match))))))))

(defun idd-compare-specifications (source-or-destination
				   specifications)
  "Determines how good SOURCE-OR-DESTINATION and SPECIFICATIONS are matching.
A return value of zero means, that they don't match. The higher the
return value the better is the matching."
  (cond ((not specifications) 1)
	(t (idd-compare-specifications-1 source-or-destination
					 specifications
					 0))))

(defun idd-get-action-depending-on-source (source
					   actions-depending-on-source
					   destination-value
					   value-action-pair)
  "Internal function of `idd-get-action-depending-on-source-and-destination'."
  (let ((source-value (idd-compare-specifications
		       source
		       (car (car actions-depending-on-source)))))
    (cond ((not actions-depending-on-source) value-action-pair)
	  ((or (= source-value 0)
	       (<= (+ destination-value source-value) (car value-action-pair)))
	   (idd-get-action-depending-on-source 
	    source
	    (cdr actions-depending-on-source)
	    destination-value
	    value-action-pair))
	  (t (idd-get-action-depending-on-source 
	      source
	      (cdr actions-depending-on-source)
	      destination-value
	      (cons (+ destination-value source-value)
		    (second (car actions-depending-on-source))))))))

(defun idd-get-action-depending-on-source-and-destination (source
							   destination
							   actions
							   value-action-pair)
  "Internal function of `idd-get-action'.
VALUE-ACTION-PAIR is a list like (<value> <action>).
It returns VALUE-ACTION-PAIR, if no other action is found, which has a
value higher than (car VALUE-ACTION-PAIR)."
  (let ((destination-value
	 (idd-compare-specifications destination (car (car actions)))))
    (cond ((not actions) value-action-pair)
	  ((= destination-value 0)
	   (idd-get-action-depending-on-source-and-destination
	    source
	    destination
	    (cdr actions)
	    value-action-pair))
	  (t (idd-get-action-depending-on-source-and-destination
	      source
	      destination
	      (cdr actions)
	      (idd-get-action-depending-on-source
	       source
	       (cdr (car actions))
	       destination-value
	       value-action-pair))))))

(defun idd-get-action (source destination actions)
  "Returns the action, which depends on the SOURCE and the DESTINATION.
The list ACTIONS contains all possible actions. Look at the variable
`idd-actions' for a description of the format of this list."
  (idd-get-action-depending-on-source-and-destination source
						      destination
						      actions
						      '(0 . nil)))

;(autoload 'ange-ftp-ftp-path "ange-ftp"
;  "Parse PATH according to ange-ftp-path-format (which see).
;Returns a list (HOST USER PATH), or nil if PATH does not match the format.")

(defun idd-set-point (source-or-destination)
  "Sets the point and buffer to SOURCE-OR-DESTINATION."
  (set-buffer (cdr (assoc ':buffer source-or-destination)))
  (goto-char (cdr (assoc ':drag-or-drop-point source-or-destination))))

(defun idd-set-region (source-or-destination)
  "Sets the point, mark and buffer to SOURCE-OR-DESTINATION.
The region is active after this function is called."
  (set-buffer (cdr (assoc ':buffer source-or-destination)))
  (goto-char (car (cdr (assoc ':region-active source-or-destination))))
  (set-mark (cdr (cdr (assoc ':region-active source-or-destination))))
  (activate-region))


;;; Specification type functions for the list `idd-actions'

(defun idd-if-region-active-p (source-or-destination value)
  "Checks if the region in the SOURCE-OR-DESTINATION was active.
It returns 1, if the region was active and VALUE is t, or if
the region was not active and VALUE is nil. Otherwise it returns
nil."
  (if (cdr (assoc ':region-active source-or-destination))
      (if value 1 nil)
    (if value nil 1)))

(defun idd-get-buffer-url (source-or-destination)
  "Returns the URL of the buffer specified by SOURCE-OR-DESTINATION."
  (save-excursion
    (idd-set-point source-or-destination)
    (url-view-url t)))

(defun idd-get-url-at-point (source-or-destination)
  "Returns the URL at the point specified by SOURCE-OR-DESTINATION.
It returns nil, if there is no URL."
  (save-excursion
    (idd-set-point source-or-destination)
    (w3-view-this-url t)))

(defun idd-if-url-at-point-p (source-or-destination value)
  "Checks if there is an URL at the point of SOURCE-OR-DESTINATION.
If that is t and VALUE is t, or that is nil and VALUE is nil, then 1
is returned. Otherwise nil is returned."
  (if value
      (if (idd-get-url-at-point source-or-destination)
	  1
	nil)
    (if (idd-get-url-at-point source-or-destination)
	nil
      1)))

(defun idd-if-major-mode-p (source-or-destination mode)
  "Checks, if the major mode of SOURCE-OR-DESTINATION is MODE.
It returns 1, if that is t and nil otherwise."
  (save-excursion
    (set-buffer (cdr (assoc ':buffer source-or-destination)))
    (if (eq major-mode mode)
	1
      nil)))

(defun idd-if-variable-non-nil-p (source-or-destination variable)
  "Checks, if the variable named VARIABLE isn't t in  SOURCE-OR-DESTINATION.
It returns 1, if this is t."
  (save-excursion
    (set-buffer (cdr (assoc ':buffer source-or-destination)))
    (if (eval variable)
	1
      nil)))

(defun idd-if-minor-mode-p (source-or-destination minor-mode-variable)
  "Checks, if the variable MINOR-MODE-VARIABLE is t in SOURCE-OR-DESTINATION.
MINOR-MODE-VARIABLE is the name of the variable!."
  (idd-if-variable-non-nil-p source-or-destination minor-mode-variable))

(defun idd-get-dired-filename-from-line (source-or-destination)
  "Returns the filename form the line in a dired buffer.
The position and the buffer is specified by SOURCE-OR-DESTINATION."
  (save-excursion
    (idd-set-point source-or-destination)
    (dired-get-filename nil t)))

(defun idd-if-dired-file-on-line-p (source-or-destination filename-regexp)
  "Checks, if the filename on the line match FILENAME-REGEXP.
The function `dired-get-filename' is used, to get the filename from
the SOURCE-OR-DESTINATION. It returns 1, if it matchs or nil."
  (let ((case-fold-search t))
    (if (and (idd-get-dired-filename-from-line source-or-destination)
	     (string-match filename-regexp
			   (idd-get-dired-filename-from-line
			    source-or-destination)))
	1
      nil)))
	       
(defun idd-if-dired-no-file-on-line-p (source-or-destination value)
  "Checks, if a filename is in the dired buffer of SOURCE-OR-DESTINATION.
It returns 1, if a filename is on the line and if VALUE is t, or if
no filename is on the line and VALUE is nil, otherwise it returns
nil. For the test the function `dired-get-filename' is used."
  (if (idd-get-dired-filename-from-line source-or-destination)
      (if value nil 1)
    (if value 1 nil)))

(defun idd-get-local-filename (source-or-destination)
  "Returns the filename of a local file specified by SOURCE-OR-DESTINATION."
  (buffer-file-name (cdr (assoc ':buffer source-or-destination))))

(defun idd-get-directory-of-buffer (source-or-destination)
  "Returns the directory name assigned to the SOURCE-OR-DESTINATION buffer."
  (save-excursion
    (idd-set-point source-or-destination)
    default-directory))

(defun idd-if-local-file-p (source-or-destination value)
  "Checks, if SOURCE-OR-DESTINATION has a file on the local filesystem.
If that is t and VALUE is t, or that is nil and VALUE is nil, then 1
is returned. Otherwise nil is returned."
  (let ((filename (idd-get-local-filename source-or-destination)))
    (if (and filename
;	     (not (ange-ftp-ftp-path filename)))
	     (not (file-remote-p filename)))
	(if value 1 nil)
      (if value nil 1))))

(defun idd-if-buffer-name-p (source-or-destination buffer-name)
  "Checks, if SOURCE-OR-DESTINATION has a buffer called BUFFER-NAME.
It returns 1 if this is the case or nil otherwise."
  (if (string= buffer-name
	       (buffer-name (cdr (assoc ':buffer source-or-destination))))
      1
    nil))

(defun idd-list-1-subset-of-list-2 (list-1 list-2)
  "Returns t, if LIST-1 is a subset of LIST-2."
  (cond ((not list-1))
	((member (car list-1) list-2)
	 (idd-list-1-subset-of-list-2 (cdr list-1) list-2))
	(t nil)))

(defun idd-same-elements-p (list-1 list-2)
  "Returns t, if both list have the same modifiers."
  (and (= (length list-1) (length list-2))
       (idd-list-1-subset-of-list-2 list-1 list-2)))

(defun idd-if-modifiers-p (source-or-destination modifiers)
  "Checks, if the MODIFIERS hold during selecting the SOURCE-OR-DESTINATION.
Returns 1, if the list MODIFIERS contains the same modifiers,
or if any modyfiers are hold and MODIFIERS is t,
or if no modyfiers are hold and MODIFIERS is nil.
Otherwise nil is returned."
  (let ((event-modifiers (event-modifiers
			  (cdr (assoc ':event source-or-destination)))))
    (cond ((not modifiers)
	   (if event-modifiers nil 1))
	  ((listp modifiers)
	   (if (idd-same-elements-p modifiers event-modifiers)
	       1
	     nil))
	  (t (if event-modifiers 1 nil)))))

;;; action functions

(defun idd-action-copy-region (source destination)
  "Copy the region from DESTINATION to SOURCE."
  (idd-set-region source)
  (let ((region-contents (buffer-substring (point) (mark))))
    (idd-set-point destination)
    (insert region-contents)))

(defun idd-action-copy-replace-region (source destination)
  "Copy the region from SOURCE and replace the DESTINATION region with it."
  (idd-set-region source)
  (let ((region-contents (buffer-substring (point) (mark))))
    (idd-set-region destination)
    (delete-region (point) (mark))
    (insert region-contents)))

(defmacro* idd-with-source-and-destination (source
					    destination
					    &key
					    do-in-source
					    do-in-destination)
  "Macro, usefull for the definition of action functions.
Look at the example `idd-action-move-region'."
  `(progn
     (if (idd-if-region-active-p ,source t)
	 (idd-set-region ,source)
       (idd-set-point ,source))
     ,(when do-in-source
	(cons 'progn do-in-source))
     (if (idd-if-region-active-p ,destination t)
	 (idd-set-region ,destination)
       (idd-set-point ,destination))
     ,(when do-in-destination
	(cons 'progn do-in-destination))))
  
(defun idd-action-move-region (source destination)
  "Move the region from SOURCE to DESTINATION."
  (let ((region))
    (idd-with-source-and-destination 
     source destination
     :do-in-source ((setq region (buffer-substring (point) (mark)))
		    (delete-region (point) (mark)))
     :do-in-destination ((insert region)))))


(defun idd-action-move-replace-region (source destination)
  "Delete the region at SOURCE and overwrite the DESTINATION region with it."
  (let ((region))
    (idd-with-source-and-destination 
     source destination
     :do-in-source ((setq region (buffer-substring (point) (mark)))
		    (delete-region (point) (mark)))
     :do-in-destination ((delete-region (point) (mark))
			 (insert region)))))


;;; Performing the drag and drop

(defun idd-display-help-about-action (action source destination)
  "Display a help buffer with information about the action."
  (if (> (car action) 0)
      (if (symbol-function (cdr action))
	  (progn
	    (with-displaying-help-buffer
	     '(lambda ()
		(set-buffer "*Help*")
		(setq idd-help-source source)
		(setq idd-help-destination destination)
		(insert "Drag and drop action: `")
		(let ((start (point)))
		  (insert (format "%s" (cdr action)))
		  (setq idd-help-start-extent (make-extent start (point)))
		  (set-extent-mouse-face idd-help-start-extent 'highlight)
		  (set-extent-face idd-help-start-extent 'bold)
		  (set-extent-keymap idd-help-start-extent
				     idd-help-start-action-keymap)
		  )
		(insert "'\n")
		(insert (format "Source buffer       : `%s'\n"
				(buffer-name (cdr (assoc ':buffer source)))))
		(insert (format "Destination buffer  : `%s'\n"
				(buffer-name (cdr (assoc ':buffer destination))
					     )))
		(insert "=================================================="
			"====================\n")
		(insert "Look at `idd-actions' in the "
			"destination buffer for other actions!\n")
		(insert (format "The documentation of `%s':\n\n"
				(cdr action)))
		(insert (documentation (cdr action)))))
	    )
	(error "Error: Action %s isn't a valid function!" (cdr action)))
    (message "No valid action defined for this source and this destination!")))

(defun idd-call-action (action source destination)
  "Calls the drag and drop ACTION with its arguments SOURCE and DESTINATION."
  (if (> (car action) 0)
      (if (symbol-function (cdr action))
	  (eval (list (cdr action) 'source 'destination))
	(error "Error: Action %s isn't a valid function!" (cdr action)))
    (message "No valid action defined for this source and this destination!")))

(defun idd-start-help-mouse-drag-and-drop ()
  "Starts help on `idd-start-mouse-drag-and-drop'."
  (interactive)
  (let ((idd-help-instead-of-action t))
    (idd-start-mouse-drag-and-drop)))

(defun idd-start-mouse-drag-and-drop ()
  "Starts a drag and drop command.
This command could be used to start a drag and drop command without a
button event. Therefore this should not be bind direct to a mouse button."
  (interactive)
  (let ((source-event)
	(drag-and-drop-message "Drag&Drop: Click on the source!"))
    (message drag-and-drop-message)
    (setq source-event
	  (next-command-event nil drag-and-drop-message))
    (if (button-press-event-p source-event)
	(idd-mouse-drag-and-drop source-event)
      (message "Wrong event! Exit drag and drop."))))

(defun idd-help-mouse-drag-and-drop (source-event)
  "Displays help about the drag and drop action."
  (interactive "@e")
  (let ((idd-help-instead-of-action t))
    (idd-mouse-drag-and-drop source-event)))

(defun idd-mouse-drag-and-drop (source-event)
  "Performs a drag and drop action.
It calls the command `idd-mouse-drag-and-drop-click' or 
`idd-mouse-drag-and-drop-press-button-during-move' depending on
the value of `idd-drag-and-drop-mouse-binding-type'."
  (interactive "@e")
  (if (eq idd-drag-and-drop-mouse-binding-type 'click)
      (idd-mouse-drag-and-drop-click source-event)
    (idd-mouse-drag-and-drop-press-button-during-move source-event)))

(defun idd-get-source-or-destination-alist (event)
  "Returns an alist with the description of a source or destination point.
The EVENT must be the button event, which has selected the source or
destination of the drag and drop command.

The alist has the following structure:
   '((:buffer . <buffer-of-the-event>)
     (:drag-or-drop-point . <closest-point-to-the-event>)
     (:region-active . <t-or-nil>)
     (:event . EVENT))

Note: <closest-point-to-the-event> is (event-closest-point EVENT),
if the EVENT is a mouse event and if it isn't nil. Otherwise the
point is used."
;  (set-buffer (event-buffer event))
  (list (cons ':buffer (event-buffer event))
	(cons ':drag-or-drop-point (set-marker
				    (make-marker)
				    (if (mouse-event-p event)
					(or (event-closest-point event)
					    (point))
				      (point))))
	(cons ':region-active (if (region-active-p)
				  (cons (set-marker (make-marker) (point))
					(set-marker (make-marker) (mark)))))
	(cons ':event event))
  )

(defun idd-mouse-drag-and-drop-press-button-during-move (source-event)
  "Performs a drag and drop action.
At first you must press the button down over the source and then
move with the pressed button to the destination, where you must leave
the button up.
This must be bind to a mouse button. The SOURCE-EVENT must be a 
button-press-event.

The disadvantage of this command compared with the command
`idd-mouse-drag-and-drop-click' is, that you can't select a
destination region."
  (interactive "@e")
  (let ((drag-and-drop-message 
	 "Drag&Drop: Leave the button over the destination!")
	(source (idd-get-source-or-destination-alist source-event))
	(destination nil)
	(destination-event))
    (message drag-and-drop-message)
    (setq destination-event 
	  (next-command-event nil drag-and-drop-message))
    (message "")
    (cond ((button-release-event-p destination-event)
	   (setq destination (idd-get-source-or-destination-alist
			      destination-event))
	   (idd-set-point destination)
	   (if idd-help-instead-of-action
	       (idd-display-help-about-action (idd-get-action source
							      destination
							      idd-actions)
					      source
					      destination)
	     (idd-call-action (idd-get-action source destination idd-actions)
			      source
			      destination)))
	  (t (message "Wrong event! Exit drag and drop.") nil))))

(defun idd-mouse-drag-and-drop-click (source-event)
  "Performs a drag and drop action.
At first you must click on the source and after that on the destination.
This must be bind to a mouse button. The SOURCE-EVENT must be a 
button-press-event."
  (interactive "@e")
  (let ((drag-and-drop-message "Drag&Drop: Click on the destination!")
	(source (idd-get-source-or-destination-alist source-event))
	(destination nil)
	(destination-event))
    (message drag-and-drop-message)
    (if (and (adapt-xemacsp) (mouse-event-p source-event))
	(dispatch-event (next-command-event)))
    (setq destination-event 
	  (next-command-event nil drag-and-drop-message))
    (message "")
    (cond ((button-press-event-p destination-event)
	   (mouse-track destination-event)
	   (setq destination (idd-get-source-or-destination-alist
			      destination-event))
	   (idd-set-point destination)
	   (if (adapt-emacs19p)
	       (while (not (button-release-event-p (next-command-event)))))
	   (if idd-help-instead-of-action
	       (idd-display-help-about-action (idd-get-action source
							      destination
							      idd-actions)
					      source
					      destination)
	     (idd-call-action (idd-get-action source destination idd-actions)
			      source
			      destination)))
	  ((and (adapt-emacs19p)
		(button-click-event-p destination-event))
	   (setq destination (idd-get-source-or-destination-alist
			      destination-event))
	   (idd-set-point destination)
	   (if idd-help-instead-of-action
	       (idd-display-help-about-action (idd-get-action source
							      destination
							      idd-actions)
					      source
					      destination)
	     (idd-call-action (idd-get-action source destination idd-actions)
			      source
			      destination)))
	  (t (message "Wrong event! Exit drag and drop.") nil))))

(defun idd-help-start-action (event)
  "Used to start the action from the help buffer."
  (interactive "@e")
  (idd-set-point idd-help-destination)
  (idd-call-action (idd-get-action idd-help-source
				   idd-help-destination
				   idd-actions)
		   idd-help-source
		   idd-help-destination)
  (delete-extent idd-help-start-extent))

;; keymap for help buffer extents
(if (not idd-help-start-action-keymap)
    (progn
      (setq idd-help-start-action-keymap
	    (make-sparse-keymap 'idd-help-start-action-keymap))
      (if (adapt-emacs19p)
	  (define-key idd-help-start-action-keymap [(mouse-2)]
	    'idd-help-start-action)
	(define-key idd-help-start-action-keymap "[(button2)]"
	  'idd-help-start-action))))

;; global key bindings
(when idd-global-mouse-keys
  (unless (where-is-internal 'idd-mouse-drag-and-drop global-map t)
    (define-key global-map idd-global-mouse-keys 'idd-mouse-drag-and-drop))
  (unless (where-is-internal 'idd-help-mouse-drag-and-drop global-map t)
    (define-key global-map
      idd-global-help-mouse-keys 'idd-help-mouse-drag-and-drop)))


(provide 'internal-drag-and-drop)
