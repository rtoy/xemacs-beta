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

;; This file is dumped with XEmacs (when window system support is compiled in).

;;; Code:

(provide 'dragdrop)

;; we need mouse-set-point
(require 'mouse)

;; I think this is a better name for the custom group
;; looks better in the menu and the group display as dragdrop
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

(defcustom dragdrop-drop-functions '(dragdrop-drop-url-default
				     dragdrop-drop-mime-default)
  "This is the standart drop function search list.
Each variable in this list is called with the drop data until
one of the functions return t, or the end of the list is reached."
  :group 'drag-n-drop
  :type '(repeat (choice (function-item dragdrop-drop-url-default)
                         (function-item dragdrop-drop-mime-default)
                         (function :tag "other"))))

(defun dragdrop-drop-dispatch (object)
  "This function identifies DROP type misc-user-events.
It tries to find out how to handle the dropped data by looking
for dragdrop-drop-functions in extents and variables."
  (catch 'dragdrop-drop-is-done
    (and (event-over-text-area-p current-mouse-event)
	 ;; let's search the extents
	 (catch 'dragdrop-extents-done
	   (let ((window (event-window current-mouse-event))
		 (pos (event-point current-mouse-event))
		 (cpos (event-closest-point current-mouse-event))
		 (buffer nil))
	     (or window (throw 'dragdrop-extents-done nil))
	     (or pos (setq pos cpos))
	     (select-window window)
	     (setq buffer (window-buffer))
	     (let ((ext (extent-at pos buffer 'dragdrop-drop-functions)))
	       (while (not (eq ext nil))
		 (dragdrop-drop-do-functions
		  (extent-property ext 'dragdrop-drop-functions)
		  object)
		 (setq ext (extent-at pos buffer 'dragdrop-drop-functions ext)))))))
    ;; now look into the variable dragdrop-drop-functions
    (dragdrop-drop-do-functions dragdrop-drop-functions object)))

(defun dragdrop-drop-do-functions (drop-funs object)
  "Calls all functions in drop-funs with object until one returns t.
Returns t if one of drop-funs returns t. Otherwise returns nil."
  (while (not (eq drop-funs ()))
    (and (funcall (car drop-funs) object)
	 (throw 'dragdrop-drop-is-done t))
    (setq drop-funs (cdr drop-funs)))
  nil)

(defun dragdrop-drop-url-default (object)
  "Default handler for dropped URL data.
Finds files and URLs. Returns nil if object does not contain URL data."
  (cond ((eq (car object) 'dragdrop-URL)
	 (let ((data (cdr object))
	       (frame (event-channel current-mouse-event))
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

(defun dragdrop-drop-mime-default (object)
  "Default handler for dropped MIME data.
Inserts text into buffer, creates MIME buffers for other types.
Returns nil if object does not contain MIME data."
  (cond ((eq (car object) 'dragdrop-MIME)
	 (let ((ldata (cdr object))
	       (frame (event-channel current-mouse-event))
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
		      (string= (car data) "text/plain")
		      (event-over-text-area-p current-mouse-event))
		 (let ((window (event-window current-mouse-event)))
		   (and window
			(select-window window))
		   (and (not dragdrop-drop-at-point)
			(mouse-set-point current-mouse-event))
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
			     (mime/viewer-mode buf (list (car data)) (cadr data))))
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
