;;; dragdrop.el --- window system-independent Drag'n'Drop support.

;; Copyright (C) 1998 Oliver Graf <ograf@fga.de>

;; Maintainer: XEmacs Development Team, Oliver Graf <ograf@fga.de>
;; Keywords: drag, drop, dumped

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

;; This file is dumped with XEmacs (when drag'n'drop support is compiled in).

;;; Code:

;; we need mouse-set-point
(require 'mouse)
(provide 'dragdrop)

;; I think this is a better name for the custom group
;; looks better in the menu and the group display as dragdrop
;; Anyway: is dragdrop- a good prefix for all this?
;; What if someone type drop<TAB> into the minibuffer?
(defgroup drag-n-drop nil
  "Window system-independent drag'n'drop support."
  :group 'editing)

(defcustom dragdrop-drop-at-point nil
  "*If non-nil, the drop handler functions will drop text at the cursor location.
Otherwise, the cursor will be moved to the location of the pointer drop before
text is inserted."
  :type 'boolean
  :group 'drag-n-drop)

(defcustom dragdrop-autoload-tm-view nil
  "*If non-nil, autoload tm-view if a MIME buffer needs to be decoded.
Otherwise, the buffer is only decoded if tm-view is already avaiable."
  :type 'boolean
  :group 'drag-n-drop)

;; the widget for editing the drop-functions
(define-widget 'dragdrop-function-widget 'list
  "Widget for editing drop dispatch functions."
  :args `((choice :tag "Function"
		  (function-item dragdrop-drop-url-default)
		  (function-item dragdrop-drop-mime-default)
		  (function-item dragdrop-drop-log-function)
		  (function :tag "Other" nil))
	  (choice :tag "Button" :value t
		  (choice-item :tag "Ignore" t)
		  (choice-item 0) (choice-item 1) (choice-item 2)
		  (choice-item 3) (choice-item 4) (choice-item 5)
		  (choice-item 6) (choice-item 7))
	  (radio-button-choice :tag "Modifiers"
			       (const :tag "Ignore Modifier Keys" t)
			       (checklist :greedy t
					  :format "Modifier Keys:\n%v"
					  :extra-offset 6
					  (const shift)
					  (const control)
					  (const meta)
					  (const alt)
					  (const hyper)
					  (const super)))
	  (repeat :inline t :value nil :tag "Extra Function Arguments"
		  (sexp :tag "Arg" :value nil)))
  :value '(nil t t))

;; button and widget selectors are still "shaky":
;; button may be a number or t (or nil?), t means "Ignore"
;; mods may be t or nil or a list of mod-syms, t means "Ignore"
;; but this seems to be a porblem for the widget, well perhaps I find
;; a solution...
(defcustom dragdrop-drop-functions '((dragdrop-drop-url-default t t)
				     (dragdrop-drop-mime-default t t))
  "This is the standart drop function search list.
Each element is a list of a function, a button selector, a modifier
selector and optional argumets to the function call.
The function must accept at least two arguments: first is the event
of the drop, second the object data, followed by any of the optional
arguments provided in this list.
The functions are called in order, until one returns t."
  :group 'drag-n-drop
  :type '(repeat dragdrop-function-widget))

(defgroup dnd-debug nil
  "Drag'n'Drop debugging options."
  :group 'drag-n-drop)

(defcustom dragdrop-drop-log nil
  "If non-nil, every drop is logged.
The name of the buffer is set in the custom 'dragdrop-drop-log-name"
  :group 'dnd-debug
  :type 'boolean)

(defcustom dragdrop-drop-log-name "*drop log buffer*"
  "The name of the buffer used to log drops.
Set dragdrop-drop-log to non-nil to enable this feature."
  :group 'dnd-debug
  :type 'string)

(defvar dragdrop-drop-log-buffer nil
  "Buffer to log drops in debug mode.")

(defun dragdrop-drop-dispatch (object)
  "This function identifies DROP type misc-user-events.
It calls functions which will handle the drag."
  (let ((event current-mouse-event))
    (and dragdrop-drop-log
	 (dragdrop-drop-log-function event object))
    (dragdrop-drop-find-functions event object)))

(defun dragdrop-drop-find-functions (event object)
  "Finds valid drop-handle functions and executes them to dispose the drop.
It does this by looking for extent-properties called 'dragdrop-drop-functions
and for variables named like this."
  (catch 'dragdrop-drop-is-done
    (and (event-over-text-area-p event)
	 ;; let's search the extents
	 (catch 'dragdrop-extents-done
	   (let ((window (event-window event))
		 (pos (event-point event))
		 (cpos (event-closest-point event))
		 (buffer nil))
	     (or window (throw 'dragdrop-extents-done nil))
	     (or pos (setq pos cpos))
	     (select-window window)
	     (setq buffer (window-buffer))
	     (let ((ext (extent-at pos buffer 'dragdrop-drop-functions)))
	       (while (not (eq ext nil))
		 (dragdrop-drop-do-functions
		  (extent-property ext 'dragdrop-drop-functions)
		  event
		  object)
		 (setq ext (extent-at pos buffer 'dragdrop-drop-functions ext)))))))
    ;; now look into the variable dragdrop-drop-functions
    (dragdrop-drop-do-functions dragdrop-drop-functions event object)))

(defun dragdrop-compare-mods (first-mods second-mods)
  "Returns t if both first-mods and second-mods contain the same elements.
Order is not important."
  (let ((moda (copy-sequence first-mods))
	(modb (copy-sequence second-mods)))
    (while (and (not (eq moda ()))
		(not (eq modb ())))
      (setq modb (delete (car moda) modb))
      (setq moda (delete (car moda) moda)))
    (and (eq moda ())
	 (eq modb ()))))

(defun dragdrop-drop-do-functions (drop-funs event object)
  "Calls all functions in drop-funs with object until one returns t.
Returns t if one of drop-funs returns t. Otherwise returns nil."
  (let ((flist nil)
	(button (event-button event))
	(mods (event-modifiers event)))
    (while (not (eq drop-funs ()))
      (setq flist (car drop-funs))
      (and (or (eq (cadr flist) t)
	       (= (cadr flist) button))
	   (or (eq (caddr flist) t)
	       (dragdrop-compare-mods (caddr flist) modifiers))
	   (apply (car flist) `(,event ,object ,@(cdddr flist)))
	   ;; (funcall (car flist) event object)
	   (throw 'dragdrop-drop-is-done t))
      (setq drop-funs (cdr drop-funs))))
  nil)

(defun dragdrop-drop-log-function (event object &optional message buffer)
  "Logs any drops into a buffer.
If buffer is nil, it inserts the data into a buffer called after
dragdrop-drop-log-name.
If dragdrop-drop-log is non-nil, this is done automatically for each drop.
The function always returns nil."
  (save-excursion
    (cond ((buffer-live-p buffer)
	   (set-buffer buffer))
	  ((stringp buffer)
	   (set-buffer (get-buffer-create buffer)))
	  ((buffer-live-p dragdrop-drop-log-buffer)
	   (set-buffer dragdrop-drop-log-buffer))
	  (t
	   (setq dragdrop-drop-log-buffer (get-buffer-create dragdrop-drop-log-name))
	   (set-buffer dragdrop-drop-log-buffer)))
    (insert (format "* %s: %s\n"
		    (current-time-string)
		    (if message message "received a drop")))
    (insert (format "  at %d,%d (%d,%d) with button %d and mods %s\n"
		    (event-x event)
		    (event-y event)
		    (event-x-pixel event)
		    (event-y-pixel event)
		    (event-button event)
		    (event-modifiers event)))
    (insert (format "  data is of type %s (%d %s)\n"
	     (cond ((eq (car object) 'dragdrop-URL) "URL")
		   ((eq (car object) 'dragdrop-MIME) "MIME")
		   (t "UNKNOWN"))
	     (length (cdr object))
	     (if (= (length (cdr object)) 1) "element" "elements")))
    (let ((i 1)
	  (data (cdr object)))
      (while (not (eq data ()))
	(insert (format "    Element %d: %S\n"
			i (car data)))
	(setq i (1+ i))
	(setq data (cdr data))))
    (insert "----------\n"))
  nil)

(defun dragdrop-drop-url-default (event object)
  "Default handler for dropped URL data.
Finds files and URLs. Returns nil if object does not contain URL data."
  (cond ((eq (car object) 'dragdrop-URL)
	 (let ((data (cdr object))
	       (frame (event-channel event))
	       (x pop-up-windows))
	   (setq pop-up-windows nil)
	   (while (not (eq data ()))
	     (cond ((dragdrop-is-some-url "file" (car data))
		    ;; if it is some file, pop it to a buffer
		    (pop-to-buffer (find-file-noselect
				    (substring (car data) 5))
				   nil frame))
		   ;; to-do: open ftp URLs with efs...
		   (t 
		    ;; some other URL, try to fire up some browser for it
		    (if (boundp 'browse-url-browser-function)
			(funcall browse-url-browser-function (car data))
		      (display-message 'error 
			"Can't show URL, no browser selected"))))
	     (undo-boundary)
	     (setq data (cdr data)))
	   (make-frame-visible frame)
	   (setq pop-up-windows x)
	   t))
	(t nil)))

(defun dragdrop-drop-mime-default (event object)
  "Default handler for dropped MIME data.
Inserts text into buffer, creates MIME buffers for other types.
Returns nil if object does not contain MIME data."
  (cond ((eq (car object) 'dragdrop-MIME)
	 (let ((ldata (cdr object))
	       (frame (event-channel event))
	       (x pop-up-windows)
	       (data nil))
	   ;; how should this be handled???
	   ;; insert drops of text/* into buffer
	   ;; create new buffer if pointer is outside buffer...
	   ;; but there are many other ways...
	   ;;	
	   ;; first thing: check if it's only text/plain and if the
	   ;; drop happened inside some buffer. if yes insert it into
	   ;; this buffer (hope it is not encoded in some MIME way)
	   ;;
	   ;; Remember: ("text/plain" "dosnotmatter" "somedata")
	   ;; drops are inserted at mouse-point, if inside a buffer
	   (while (not (eq ldata ()))
	     (setq data (car ldata))
	     (if (and (listp data)
		      (= (length data) 3)
		      (listp (car data))
		      (stringp (caar data))
		      (string= (caar data) "text/plain")
		      (event-over-text-area-p event))
		 (let ((window (event-window event)))
		   (and window
			(select-window window))
		   (and (not dragdrop-drop-at-point)
			(mouse-set-point event))
		   (insert (caddr data)))
	       (let ((buf (get-buffer-create "*MIME-Drop data*")))
		 (set-buffer buf)
		 (pop-to-buffer buf nil frame)
		 (or (featurep 'tm-view)
		     (and dragdrop-autoload-tm-view
			  (require 'tm-view)))
		 (cond ((stringp data)
			;; this is some raw MIME stuff
			;; create some buffer and let tm do the job
			;;
			;; this is always the same buffer!!!
			;; change?
			(erase-buffer)
			(insert data)
			(and (featurep 'tm-view)
			     (mime/viewer-mode buf)))
		       ((and (listp data)
			     (= (length data) 3))
			;; change the internal content-type representation to the
			;; way tm does it ("content/type" (key . value)*)
			;; but for now list will do the job
			;;
			;; this is always the same buffer!!!
			;; change?
			(erase-buffer)
			(insert (caddr data))
			(and (featurep 'tm-view)
			     ;; this list of (car data) should be done before
			     ;; enqueing the event
			     (mime/viewer-mode buf (car data) (cadr data))))
		       (t
			(display-message 'error "Wrong drop data")))))
	     (undo-boundary)
	     (setq ldata (cdr ldata)))
	   (make-frame-visible frame)
	   (setq pop-up-windows x))
	 t)
	(t nil)))

(defun dragdrop-is-some-url (method url)
  "Returns true if method equals the start of url.
If method does not end into ':' this is appended before the
compare."
  (cond ((and (stringp url)
	      (stringp method)
	      (> (length url) (length method)))
	 ;; is this ?: check efficient enough?
	 (if (not (string= (substring method -1) ":"))
	     (setq method (concat method ":")))
	 (string= method (substring url 0 (length method))))
	(t nil)))

;;; dragdrop.el ends here
