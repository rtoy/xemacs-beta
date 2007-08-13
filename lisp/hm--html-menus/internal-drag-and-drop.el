;;; $Id: internal-drag-and-drop.el,v 1.1.1.1 1996/12/18 22:43:20 steve Exp $
;;; 
;;; Copyright (C) 1996 Heiko Muenkel
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
;;;	the points where youve clicked with the mouse, on the state
;;;	of the region, the point, the mark and any other properties
;;;	of the source and the destination buffers. The actions are
;;;	defined by the variable `idd-actions', which is a buffer local
;;;	variable. The following is an example for the html-mode:
;;;	(defvar html-idd-actions
;;;	  '((nil (((idd-major-mode-p . dired-mode)
;;;		   (idd-dired-file-on-line-p . ".*\\.\\(gif\\)\\|\\(jpq\\)"))
;;;		  hm--html-idd-add-include-image-from-dired-line)
;;;		 (((idd-major-mode-p . dired-mode)
;;;		   (idd-dired-no-file-on-line-p . nil))
;;;		  hm--html-idd-add-file-link-to-file-on-dired-line)
;;;		 (((idd-major-mode-p . dired-mode)
;;;		   (idd-dired-no-file-on-line-p . t))
;;;		  hm--html-idd-add-file-link-to-directory-of-buffer)
;;;		 (((idd-major-mode-p . w3-mode)
;;;		   (idd-url-at-point-p . t))
;;;		  hm--html-idd-add-html-link-from-w3-buffer-point)
;;;		 (((idd-major-mode-p . w3-mode))
;;;		  hm--html-idd-add-html-link-to-w3-buffer)
;;;		 (((idd-local-file-p . t))
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
;;;	(define-key global-map [(meta button1)] 'idd-mouse-drag-and-drop)
;;;	
;;;	Define actions in the variable `idd-actions'.
;;;

(defvar idd-actions nil
  "The list with actions, depending on the source and the destination.
The list looks like:
  '((<source-specification-1> (<destination-specification-1> <action-1-1>)
                              (<destination-specification-2> <action-1-2>)
                              :
     )
    (<source-specification-2> (<destination-specification-1> <action-2-1>)
                              (<destination-specification-2> <action-2-2>)
                              :
     )
    :
    )
The <source-specification> looks like the following:
  '([(<specification-type> <value>)])
with <specification-type> :== idd-minor-mode-p | idd-buffer-name-p
			      | idd-region-active-p ...

The <destination-specification> looks like <source-specification>, except
that a valid <specification-type> is also idd-major-mode-p.

If <source-specification-1> or <destination-specification-1> is set to
nil, then every source or destination matches. `idd-actions' is a
buffer local variable, which should be at least mode depended. So if
the <source-specification-1> is set to nil it says, that the source
buffer must only have a specific mode. But however, it's also possible
to define a general `idd-actions' list, where the source mode is
specified by idd-major-mode-p.

<action> ist a function, which has two arguments, the specifies the
source and the second the destination.")

(make-variable-buffer-local 'idd-actions)

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

(defun idd-get-action-depending-on-destination (destination
						actions-depending-on-dest
						source-value
						value-action-pair)
  "Internal function of `idd-get-action-depending-on-source-and-destination'."
  (let ((destination-value (idd-compare-specifications
			    destination
			    (car (car actions-depending-on-dest)))))
    (cond ((not actions-depending-on-dest) value-action-pair)
	  ((or (= destination-value 0)
	       (<= (+ source-value destination-value) (car value-action-pair)))
	   (idd-get-action-depending-on-destination 
	    destination
	    (cdr actions-depending-on-dest)
	    source-value
	    value-action-pair))
	  (t (idd-get-action-depending-on-destination 
	      destination
	      (cdr actions-depending-on-dest)
	      source-value
	      (cons (+ source-value destination-value)
		    (second (car actions-depending-on-dest))))))))

(defun idd-get-action-depending-on-source-and-destination (source
							   destination
							   actions
							   value-action-pair)
  "Internal function of `idd-get-action'.
VALUE-ACTION-PAIR is a list like (<value> <action>).
It returns VALUE-ACTION-PAIR, if no other action is found, which has a
value higher than (car VALUE-ACTION-PAIR)."
  (let ((source-value (idd-compare-specifications source (car (car actions)))))
    (cond ((not actions) value-action-pair)
	  ((= source-value 0)
	   (idd-get-action-depending-on-source-and-destination
	    source
	    destination
	    (cdr actions)
	    value-action-pair))
	  (t (idd-get-action-depending-on-source-and-destination
	      source
	      destination
	      (cdr actions)
	      (idd-get-action-depending-on-destination
	       destination
	       (cdr (car actions))
	       source-value
	       value-action-pair))))))

(defun idd-get-action (source destination actions)
  "Returns the action, which depends on the SOURCE and the DESTINATION.
The list ACTIONS contains all possible actions. Look at the variable
`idd-actions' for a description of the format of this list."
  (idd-get-action-depending-on-source-and-destination source
						      destination
						      actions
						      '(0 . nil)))

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

(defun idd-url-at-point-p (source-or-destination value)
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

(defun idd-major-mode-p (source-or-destination mode)
  "Checks, if the major mode of SOURCE-OR-DESTINATION is MODE.
It returns 1, if that is t and nil otherwise."
  (save-excursion
    (set-buffer (cdr (assoc ':buffer source-or-destination)))
    (if (eq major-mode mode)
	1
      nil)))

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

(defun idd-get-dired-filename-from-line (source-or-destination)
  "Returns the filename form the line in a dired buffer.
The position and the buffer is specified by SOURCE-OR-DESTINATION."
  (save-excursion
    (idd-set-point source-or-destination)
    (dired-get-filename nil t)))

(defun idd-dired-file-on-line-p (source-or-destination filename-regexp)
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
	       
(defun idd-dired-no-file-on-line-p (source-or-destination value)
  "Checks, if a filename is in the dired buffer of SOURCE-OR-DESTINATION.
It returns 1, if a filename is on the line and if VALUE is t, or if
no filename is on the line and VALUE is nil, otherwise it returns
nil. For the test the function `dired-get-filename' is used."
  (if (idd-get-dired-filename-from-line source-or-destination)
      (if value nil 1)
    (if value 1 nil)))

(autoload 'ange-ftp-ftp-path "ange-ftp"
  "Parse PATH according to ange-ftp-path-format (which see).
Returns a list (HOST USER PATH), or nil if PATH does not match the format.")

(defun idd-get-local-filename (source-or-destination)
  "Returns the filename of a local file specified by SOURCE-OR-DESTINATION."
  (buffer-file-name (cdr (assoc ':buffer source-or-destination))))

(defun idd-get-directory-of-buffer (source-or-destination)
  "Returns the directory name assigned to the SOURCE-OR-DESTINATION buffer."
  (save-excursion
    (idd-set-point source-or-destination)
    default-directory))

(defun idd-local-file-p (source-or-destination value)
  "Checks, if SOURCE-OR-DESTINATION has a file on the local filesystem.
If that is t and VALUE is t, or that is nil and VALUE is nil, then 1
is returned. Otherwise nil is returned."
  (let ((filename (idd-get-local-filename source-or-destination)))
    (if (and filename
	     (not (ange-ftp-ftp-path filename)))
	(if value 1 nil)
      (if value nil 1))))

(defun idd-call-action (action source destination)
  "Calls the drag and drop ACTION with its arguments SOURCE and DESTINATION."
  (if (> (car action) 0)
      (if (symbol-function (cdr action))
	  (eval (list (cdr action) 'source 'destination))
	(error "Error: Action %s isn't a valid function!" (cdr action)))
    (message "No valid action defined for this source and this destination!")))

(defun idd-mouse-drag-and-drop (source-event)
  "Performs a drag and drop action.
At first you must click on the source and after that on the destination."
  (interactive "@e")
  (let ((source (list (cons ':buffer (current-buffer))
		      (cons ':drag-or-drop-point
			    (event-closest-point source-event))
		      (cons ':region-active (if (region-active-p)
						(cons (point)
						      (mark))))))
	(destination nil)
	(destination-event))
    (if (adapt-xemacsp)
	(dispatch-event (next-command-event)))
    (setq destination-event 
	  (next-command-event nil "Drag&Drop: Click on the destination!"))
    (cond ((button-press-event-p destination-event)
	   (setq destination (list (cons ':buffer 
					 (event-buffer destination-event))
				   (cons ':drag-or-drop-point 
					 (event-closest-point 
					  destination-event))
				   (cons ':region-active nil)))
	   (if (adapt-emacs19p)
	       (while (not (button-release-event-p (next-command-event)))))
	   (idd-call-action (idd-get-action source destination idd-actions)
			    source
			    destination))
	  (t (setq action "Wrong event") nil))))


(provide 'internal-drag-and-drop)
